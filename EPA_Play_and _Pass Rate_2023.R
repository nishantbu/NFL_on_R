install.packages("ggthemes")
install.packages("ggrepel")
install.packages("ggsave")

library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)
view(teams_colors_logos)
names(teams_colors_logos)
pbp <- load_pbp(2023)
qb_epa_play <- pbp |> 
  filter(pass == 1 | rush == 1, !is.na(epa)) |> 
  filter(!is.na(epa)) |>
  group_by(id) |>
  summarise(name = first(name),
            team = last(posteam),
            plays = n(),
            epa_play = mean(epa),
            wpa_play = mean(wpa),
            pass_attempt = sum(incomplete_pass + complete_pass, na.rm = T)) |>
  filter(plays >= 10, pass_attempt >= 30)|>
  mutate(pass_rate = pass_attempt/plays) |>
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

qb_epa_play |>
  ggplot(aes(x = pass_rate, y= epa_play)) + 
  geom_point(aes(fill = team_color, color = team_color2, size = plays),
             shape = 21, aplha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  ggrepel::geom_label_repel(aes(label = name)) + 
  theme_bw()+
  geom_hline(yintercept = mean(qb_epa_play$epa_play), linetype = "dashed") +
  geom_vline(xintercept = mean(qb_epa_play$pass_rate), linetype = "dashed") +
  labs(x= "Pass Rate",
       y = "EPA/Play",
       title = "EPA/play and Pass Rate, 2023",
       caption = "By STRANGE BIRD") +
  #scale_x_continuous(breaks = scales::pretty_break(n = 8)) +
  #scale_y_continuous(braaks = scales::pretty_breaks(n = 8)) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"))
ggsave('epa-pass-rate.png', width = 14, height = 10, dpi = "retina")

qb_epa_play |> 
  ggplot(aes(x = epa_play, y = fct_reorder(name, epa_play))) +
  geom_bar(aes(fill = team_color, color = team_color2), 
           stat = "identity", alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_minimal() +
  geom_image(aes(image = team_logo_espn, x = ifelse(epa_play > 0, epa_play + 0.01, epa_play - 0.01)), 
             asp = 16/9, size = 0.035) +
  labs(x = "EPA/Play",
       y = "Quarterback",
       title = "Each Quarterback's EPA/Play, 2023",
       #subtitle = "First 4 Weeks",
       caption = "Strange Bird") +
  
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5)) 
ggsave('QB-pass-epa-23.png', width = 14, height = 10, dpi = "retina")

qb_epa_play |> gt()

qb_gt <- qb_epa_play |> 
  arrange(-epa_play) |>
  slice_head(n = 10) |> 
  mutate(rank = row_number()) |> 
  dplyr::select(rank, name, team_wordmark, pass_attempt, plays, pass_rate, epa_play, 
                wpa_play) |> 
  mutate(pass_rate = 100*round(pass_rate, 3),
         epa_play = round(epa_play, 2),
         wpa_pay = round (wpa_play, 4)) |> 
  gt() |> 
  cols_align(align = "center") |> 
  gtExtras::gt_img_rows(team_wordmark) |> 
  cols_label(rank = "Rank",
             name = "Quarterback",
             team_wordmark = "",
             pass_attempt = "Pass Attempts",
             plays = "Plays",
             pass_rate = "Pass Rate",
             epa_play = "EPA Per Play",
             wpa_play = "Win Probability Added") |> 
  gtExtras::gt_theme_espn() |> 
  gtExtras::gt_hulk_col_numeric(epa_play)

print(qb_gt)

gtsave(qb_gt, "qb_gt_table.pdf")


# Create GT Table
qb_gt <- qb_epa_play |> 
  arrange(-epa_play) |>
  slice_head(n = 10) |> 
  mutate(rank = row_number()) |> 
  dplyr::select(rank, name, team_wordmark, pass_attempt, plays, pass_rate, epa_play, wpa_play) |> 
  mutate(pass_rate = 100 * round(pass_rate, 3),
         epa_play = round(epa_play, 2),
         wpa_play = round(wpa_play, 3)) |> 
  gt() |> 
  tab_header(
    title = md("**Top 10 Quarterbacks by EPA/Play**"),
    subtitle = md("2023 Season - First 4 Weeks")
  ) |> 
  cols_align(align = "center") |> 
  gtExtras::gt_img_rows(team_wordmark) |> 
  cols_label(rank = "Rank",
             name = "Quarterback",
             team_wordmark = "",
             pass_attempt = "Pass Attempts",
             plays = "Plays",
             pass_rate = "Pass Rate (%)",
             epa_play = "EPA Per Play",
             wpa_play = "Win Probability Added") |> 
  gtExtras::gt_theme_espn() |> 
  gtExtras::gt_hulk_col_numeric(epa_play) |> 
  data_color(
    columns = vars(wpa_play),
    colors = scales::col_numeric(
      palette = c("red", "yellow", "green"),
      domain = c(min(qb_epa_play$wpa_play), max(qb_epa_play$wpa_play))
    )
  ) |> 
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(16),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.border.bottom.style = "solid",
    table_body.border.bottom.width = px(1),
    table_body.border.bottom.color = "gray"
  )

print(qb_gt)

gtsave(qb_gt, "qb_gt_table.png")
