# Loading packages

devtools::install_github("statsbomb/StatsBombR")
devtools::install_github("FCrSTATS/SBpitch")
library(StatsBombR)
library(tidyverse)
library(SBpitch)

# Getting the Data

comp_sb <- FreeCompetitions()

laliga1920 <-
  comp_sb %>%
  filter(competition_id==11 & season_id==42)

laliga1920_matches <- FreeMatches(laliga1920) %>%  filter(home_team.home_team_name %in% c("Barcelona", "Real Madrid"), away_team.away_team_name %in% c("Barcelona", "Real Madrid"))

sb_laliga1920 <-StatsBombFreeEvents(MatchesDF = laliga1920_matches, Parallel = TRUE) %>%
  allclean() %>%
  glimpse()


# Simple Use Case
# Team Stats: Calculate shots, goals, shots per goal

shots_goals <- sb_laliga1920 %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)) %>%
  mutate(shots_per_goal = round(shots/goals, digits = 1)) %>%
  arrange(desc(shots_per_goal))


# Team Stats: Calculate shots and goals per game

shots_goals_per_team <- sb_laliga1920 %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm =
                          TRUE)/n_distinct(match_id),
            goals = sum(shot.outcome.name=="Goal", na.rm =
                          TRUE)/n_distinct(match_id))

# Plot Shots per game

ggplot(data = shots_goals_per_team, aes(x = reorder(team.name, shots), y = shots)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(y="Shots") +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous( expand = c(0,0))+
  coord_flip() +
  theme_SB()

# Player Shots Per 90 minutes

player_shots <- sb_laliga1920 %>%
  group_by(player.name, player.id) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE)) %>%
  arrange(desc(shots))

player_minutes <- get.minutesplayed(sb_laliga1920) %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed))

player_shots_minutes <- left_join(player_shots, player_minutes) %>%
  mutate(nineties = minutes/90) %>%
  mutate(shots_per90 = shots/nineties) %>%
  arrange(desc(shots_per90)) %>%
  filter(minutes > 120)

# Ploting Ivan Rakitić Compleated Passes

rakitic_passes <- sb_laliga1920 %>%
  filter(player.id == 5470 & type.name == "Pass")

create_Pitch(grass_colour = "#224C56",
             line_colour =  "#797876",
             background_colour = "#224C56",
             goal_colour = "#224C56",
             JdeP = TRUE,
             goaltype = "box") +
  geom_segment(data = rakitic_passes, aes(x = location.x, y = location.y,
                                          xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size = 0.6, color = "gray",arrow = arrow(length = unit(0.08, "inches"))) +
  labs(title = "Ivan Rakitic, Completed Passes", subtitle = "vs Real Madrid") +
  scale_y_reverse() +
  coord_fixed(ratio = 105/100)
