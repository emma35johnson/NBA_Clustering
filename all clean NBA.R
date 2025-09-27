all.data <- rbind(
  read.csv("C:/Users/emma3/Downloads/regular_season_box_scores_2010_2024_part_1.csv", header = T), 
  read.csv("C:/Users/emma3/Downloads/regular_season_box_scores_2010_2024_part_2.csv", header = T),
  read.csv("C:/Users/emma3/Downloads/regular_season_box_scores_2010_2024_part_3.csv", header = T)
)

library(openxlsx)

setwd("C:/Users/emma3/OneDrive/Documents/Spring 2025/Statistical Learning/NBA Project")
write.csv(all.data, "all.data.csv", row.names = F)

################################################################################

data2324 <- all.data %>%
  filter(season_year == "2023-24") %>%
  select(-season_year, -game_date, -gameId, -matchup, -teamCity, -teamName, -teamSlug)

player_totals <- data2324 %>%
  mutate(
    minutes_seconds = ifelse(minutes == "", "00:00", minutes),
    total_seconds = period_to_seconds(ms(minutes_seconds))
  ) %>%
  group_by(personId) %>%
  summarise(
    personName = first(personName),
    position = {
      pos_vals <- position[position != ""]
      if (length(pos_vals) > 0) pos_vals[1] else "X"
    },
    total_minutes = sum(total_seconds, na.rm = TRUE) / 60,
    fieldGoalsMade = sum(fieldGoalsMade, na.rm = TRUE),
    fieldGoalsAttempted = sum(fieldGoalsAttempted, na.rm = TRUE),
    threePointersMade = sum(threePointersMade, na.rm = TRUE),
    threePointersAttempted = sum(threePointersAttempted, na.rm = TRUE),
    freeThrowsMade = sum(freeThrowsMade, na.rm = TRUE),
    freeThrowsAttempted = sum(freeThrowsAttempted, na.rm = TRUE),
    reboundsOffensive = sum(reboundsOffensive, na.rm = TRUE),
    reboundsDefensive = sum(reboundsDefensive, na.rm = TRUE),
    reboundsTotal = sum(reboundsTotal, na.rm = TRUE),
    assists = sum(assists, na.rm = TRUE),
    steals = sum(steals, na.rm = TRUE),
    blocks = sum(blocks, na.rm = TRUE),
    turnovers = sum(turnovers, na.rm = TRUE),
    foulsPersonal = sum(foulsPersonal, na.rm = TRUE),
    points = sum(points, na.rm = TRUE)
  ) %>%
  ungroup()

player_stats <- player_totals %>%
  mutate(
    fieldGoalPercentage = fieldGoalsMade / fieldGoalsAttempted,
    threePointPercentage = threePointersMade / threePointersAttempted,
    freeThrowPercentage = freeThrowsMade / freeThrowsAttempted
  ) %>%
  select(
    playerID = personId,
    playerName = personName,
    position,
    minutes = total_minutes,
    FGper = fieldGoalPercentage,
    TPper = threePointPercentage,
    FTper = freeThrowPercentage,
    rebounds = reboundsTotal,
    assists,
    steals,
    blocks,
    turnovers,
    fouls = foulsPersonal,
    points
  )

player.stats <- player_stats %>%
  mutate(playerID = as.factor(playerID))

glimpse(player.stats)

write.csv(player.stats, "player.stats.csv", row.names = F)

################################################################################

team.data <- all.data %>%
  group_by(teamId, teamTricode, season_year) %>%
  summarise(
    fieldGoalsMade = sum(fieldGoalsMade, na.rm = TRUE),
    fieldGoalsAttempted = sum(fieldGoalsAttempted, na.rm = TRUE),
    threePointersMade = sum(threePointersMade, na.rm = TRUE),
    threePointersAttempted = sum(threePointersAttempted, na.rm = TRUE),
    freeThrowsMade = sum(freeThrowsMade, na.rm = TRUE),
    freeThrowsAttempted = sum(freeThrowsAttempted, na.rm = TRUE),
    reboundsOffensive = sum(reboundsOffensive, na.rm = TRUE),
    reboundsDefensive = sum(reboundsDefensive, na.rm = TRUE),
    rebounds = sum(reboundsTotal, na.rm = TRUE),
    assists = sum(assists, na.rm = TRUE),
    steals = sum(steals, na.rm = TRUE),
    blocks = sum(blocks, na.rm = TRUE),
    turnovers = sum(turnovers, na.rm = TRUE),
    foulsPersonal = sum(foulsPersonal, na.rm = TRUE),
    points = sum(points, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    fieldGoalPercentage = fieldGoalsMade / fieldGoalsAttempted,
    threePointPercentage = threePointersMade / threePointersAttempted,
    freeThrowPercentage = freeThrowsMade / freeThrowsAttempted
  ) %>%
  select(
    teamID = teamId,
    teamCode = teamTricode,
    season = season_year,
    FGper = fieldGoalPercentage,
    TPper = threePointPercentage,
    FTper = freeThrowPercentage,
    rebounds,
    assists,
    steals,
    blocks,
    turnovers,
    points
  ) %>% 
  mutate(teamID  = as.factor(teamID))

glimpse(team.data)

write.csv(team.data, "team.data.csv", row.names = F)

