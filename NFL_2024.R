library(tidyverse)
library(nflfastR)
library(ggimage)
library(ggplot2)
library(gt)
library(gtExtras)

View(teams_colors_logos)

pbp <- nflreadr::load_pbp(2023)

names(pbp)


pbp_rp = pbp |>
  filter(pass == 1 | rush == 1) |> 
  filter(!is.na(epa))

defensive_pass_efficiency_23 <- pbp |> 
  filter(pass == 1) |> 
  group_by(defteam) |> 
  summarize(passes = n(),
            pass_epa = mean(epa))

defensive_rush_efficiency_23<- pbp |> 
  filter(rush == 1) |> 
  group_by(defteam) |> 
  summarize(rushes = n(),
            rush_epa = mean(epa))

total_eff_def <- defensive_pass_efficiency_23|> 
  left_join(defensive_rush_efficiency_23, by = "defteam") |>
  mutate(pass_epa = 100*round(pass_epa, 3),
         rush_epa = 100*round(rush_epa, 2))

total_eff_def <- total_eff_def |> 
  left_join(teams_colors_logos, by = c("defteam" = "team_abbr"))

total_eff_def |> 
  ggplot(aes(x = pass_epa, y = rush_epa)) +
  geom_hline(yintercept = mean(total_eff_def$rush_epa), linetype = "dashed") +
  geom_vline(xintercept = mean(total_eff_def$pass_epa), linetype = "dashed") +
  #geom_smooth(method = "lm") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  labs(x = "EPA/Defensive pass",
       y = "EPA/Defensive rush",
       title = "EPA/Defensive pass and EPA/Defensive Rush in 2023",
       subtitle = "2023",
       caption = "By STRANGE BIRD") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#========================================================================================

Offense_pass_efficiency_23 <- pbp |> 
  filter(pass == 1) |> 
  group_by(posteam) |> 
  summarize(passes = n(),
            pass_epa = mean(epa),
            wpa_pass = mean(wpa))

Offense_rush_efficiency_23<- pbp |> 
  filter(rush == 1) |> 
  group_by(posteam) |> 
  summarize(rushes = n(),
            rush_epa = mean(epa),
            wpa_rush = mean(wpa))

total_eff_off <- Offense_pass_efficiency_23|> 
  left_join(Offense_rush_efficiency_23, by = "posteam") |>
  mutate(pass_epa = 100*round(pass_epa, 4),
         rush_epa = 100*round(rush_epa, 3),
         wpa_pass = 100*round(wpa_pass, 4),
         wpa_rush = 100*round(wpa_rush, 3))

total_eff_off <- total_eff_off |> 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

total_eff_off |> 
  ggplot(aes(x = pass_epa, y = rush_epa)) +
  geom_hline(yintercept = mean(total_eff_off$rush_epa), linetype = "dashed") +
  geom_vline(xintercept = mean(total_eff_off$pass_epa), linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  labs(x = "EPA/Defensive pass",
       y = "EPA/Defensive rush",
       title = "EPA/Defensive pass and EPA/Defensive Rush in 2023",
       subtitle = "2023",
       caption = "By STRANGE BIRD") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Win probability added plot 

total_eff_off |> 
  ggplot(aes(x = wpa_pass, y = wpa_rush)) +
  geom_hline(yintercept = mean(total_eff_off$wpa_rush), linetype = "dashed") +
  geom_vline(xintercept = mean(total_eff_off$wpa_pass), linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  labs(x = "WPA/offensive pass",
       y = "WPA/offensive rush",
       title = "WPA/Offensive pass and WPA/offensive Rush in 2023",
       subtitle = "2023",
       caption = "By STRANGE BIRD") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

