library(nflreadr)
library(tidyverse)
library(gt)
library(ggimage)
library(gtExtras)
library(nflplotR)


pbp <- load_pbp(2023)

pbp |> 
  filter(passer %in% c("D.Watson", "L.Jackson", "J.Burrow", "R.Wilson")) |> 
  group_by(posteam, passer_player_id, passer, pass_location, pass_length) |> 
  summarise(
    HEPA = mean(total_home_epa, na.rm = T),
    AWEPA = mean(total_away_epa,na.rm = T), 
    successr = mean(complete_pass/sum(pass), na.rm = T),
    passs = sum(pass)
  )%>%
  drop_na()%>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))


# Your data manipulation and summarization code
afc_north_qbs <- pbp |> 
  filter(passer %in% c("B.Purdy")) |> 
  group_by(posteam, passer_id, passer, pass_location, pass_length) |> 
  summarise(
    EPA = mean(epa, na.rm = TRUE),
    HEPA = mean(total_home_epa, na.rm = TRUE),
    AWEPA = mean(total_away_epa, na.rm = TRUE), 
    successr = mean(complete_pass / sum(pass), na.rm = TRUE),
    passs = sum(pass)
  ) %>%
  drop_na() %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) 

# Creating a GT table
b_purdy <- afc_north_qbs %>%
  dplyr::select(passer,
                pass_location, 
                pass_length, 
                passs, 
                HEPA, 
                AWEPA, 
                successr,
                EPA,
                posteam,
                team_wordmark) %>% 
  mutate(EPA = round(EPA, 2),
         successr = 100*round(successr, 3), 
         HEPA = round(HEPA, 2),
         AWEPA = round(AWEPA, 2)
         ) %>% 
  gt() %>%
  tab_header(
    title = "Brock Prudy Passing Performance (2023 Season)",
    subtitle = "Comparing Home EPA, Away EPA, Success Rate, and Passes Thrown"
  ) %>%
  cols_align(align = "center") %>%
  gtExtras::gt_img_rows(team_wordmark) %>%
  nflplotR::gt_nfl_headshots(columns = c(passer), height = 50) %>% 
  cols_label(
    passer = "Quarterback",
    team_wordmark = "Team Name",
    pass_location = "Pass Location",
    pass_length = "Pass Length",
    EPA = "Expected Points Added",
    HEPA = "Home EPA",
    AWEPA = "Away EPA",
    successr = "Success Rate",
    passs = "Passes Thrown"
  ) %>%
  gtExtras::gt_hulk_col_numeric(
    columns = c(EPA),
    palette = c("#AA0000", "#000000")) %>%
  gtExtras::gt_hulk_col_numeric(
    columns = c(HEPA),
    palette = c("#B3995D", "#FFFFFF")) %>%
  gtExtras::gt_hulk_col_numeric(
    columns = c(AWEPA),
    palette = c("#AA0000", "#000000")) %>%
  gtExtras::gt_hulk_col_numeric(
    columns = c(successr),
    palette = c("#B3995D", "#FFFFFF")) %>%
  gtExtras::gt_theme_espn() %>%
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_column_labels(
      columns = everything()
    )
  )%>%
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "darkgray",
      weight = px(2)
    ),
    locations = cells_body(
      columns = everything()
    )
  ) %>%
  tab_source_note(
    source_note = md("**Data Source:** nflfastR")
  ) %>%
  tab_footnote(
    footnote = "Strange Bird",
    locations = cells_title(groups = "subtitle")
  )



# Display the table
b_purdy
gtsave(b_purdy ,"brock_purdy_passing_performance.png")
