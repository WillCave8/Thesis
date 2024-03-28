library(tidyverse)

fg_data <- read_csv("fangraphs_season_level.csv")

library(ggplot2)

specific_players <- c("Bryce Elder", "Jordan Lyles", "Blake Snell", "Gerrit Cole", "Dylan Cease", "Spencer Strider")
fg_data %>% 
  filter(Season == 2023 & IP >= 162) %>%
  ggplot(aes(Stuff_plus, ERA)) + 
  geom_point() + 
  geom_text(data = fg_data %>% filter(Season == 2023 & IP >= 162 & NameASCII %in% specific_players),
            aes(label = NameASCII), vjust = 1.7, hjust = 0.5, size = 2.5, color = "black") +
  ggtitle("FanGraphs Stuff+ vs ERA, 2023 (Qualified Pitchers)") +
  xlab("Stuff+") +
  ylab("ERA") +
  labs(
    subtitle = "Data via FanGraphs"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.subtitle = element_text(hjust = 0.5, face = "italic"), # Italicize and center subtitle
    axis.title.y = element_text(vjust = 0.5),
    axis.title.x = element_text(vjust = -0.5)
  ) +
  scale_y_reverse() +
  geom_smooth(method="lm", se=FALSE, color="red", linewidth = 0.5) +
  annotate("text", x = 100, y = 1, hjust = 0, vjust = 0, 
           label = paste("R-Squared =", round(summary(lm(ERA ~ Stuff_plus, 
                                                         data = fg_data %>% 
                                                           filter(Season == 2023 & IP >= 162)))$r.squared, digits=3)))

fg_data %>% 
  filter(Season == 2023 & IP >= 162) %>%
  ggplot(aes(Stf_plus_CH, ERA)) + 
  geom_point() + 
  geom_text(data = fg_data %>% filter(Season == 2023 & IP >= 162 & NameASCII %in% specific_players),
            aes(label = NameASCII), vjust = 1.7, hjust = 0.5, size = 2.5, color = "black") +
  ggtitle("FanGraphs Stuff+ vs ERA, 2023 (Qualified Pitchers)") +
  xlab("Stuff+") +
  ylab("ERA") +
  labs(
    subtitle = "Data via FanGraphs"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.subtitle = element_text(hjust = 0.5, face = "italic"), # Italicize and center subtitle
    axis.title.y = element_text(vjust = 0.5),
    axis.title.x = element_text(vjust = -0.5)
  ) +
  scale_y_reverse() +
  geom_smooth(method="lm", se=FALSE, color="red", linewidth = 0.5) +
  annotate("text", x = 100, y = 1, hjust = 0, vjust = 0, 
           label = paste("R-Squared =", round(summary(lm(ERA ~ Stuff_plus, 
                                                         data = fg_data %>% 
                                                           filter(Season == 2023 & IP >= 162)))$r.squared, digits=3)))


qual_p <- fg_data %>% 
  filter(Season == 2023 & IP >= 162) %>%
  select(Name)
changeup_2023 %>%
  merge(qual_p, by.x = "name", by.y = "Name") -> qual_ch


changeup_2023_visuals <- data_2023 %>%
  filter(pitch_type == "CH") %>%
  na.omit() %>%
  cbind(data.frame(test = predict(stuff_changeup_model, .))) %>%
  mutate(scaled_test = 100 - (scale(test) * 100))

ggplot(changeup_2023_visuals, aes(x = release_speed_diff, y = pfx_z_diff)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap of Changeup Speed Differential and Vertical Movement",
       x = "Release Speed Difference", 
       y = "Vertical Movement Difference",
       fill = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")

no_outliers_ch <- changeup_2023_visuals %>%
  filter(scaled_test >= 0 & scaled_test <= 200)

n_bins <- 10

bin_avg <- changeup_2023_visuals %>%
  group_by(
    x_bin = cut(pfx_x_diff, n_bins),
    y_bin = cut(pfx_z_diff, n_bins)
  ) %>%
  mutate(count = n()) %>%
  filter(count >= 100) %>%
  group_by(x_bin, y_bin) %>%
  summarise(avg_scaled_test = mean(scaled_test, na.rm = TRUE), .groups = "drop")

ggplot(bin_avg, aes(x = x_bin, y = y_bin, fill = avg_scaled_test)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 100, name = "Stuff+") +
  labs(
    title = "Difference in Horizontal Movement vs Difference in 
Vertical Movement on Changeups, 2023 MLB Season",
    x = "Difference in Horizontal Movement",
    y = "Difference in Vertical Movement"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

changeup_2023_visuals %>% group_by(pitcher) %>%
  summarise(vb = mean(pfx_z_diff),
            velo = mean(release_speed_diff))

changeup_2023_visuals %>%
  mutate(total_release_diff = abs(release_extension_diff) + 
           abs(release_pos_x_diff) + abs(release_pos_z_diff)) %>%
  group_by(pitcher) %>%
  mutate(mean_rel = mean(total_release_diff),
         mean_stuff = mean(scaled_test)) %>%
  select(pitcher, mean_rel, mean_stuff) %>%
  unique() -> with_rel

ggplot(with_rel, aes(x = mean_rel, y = mean_stuff)) +
  geom_point()
