library(tidyverse)
fastballs <- SC_data2 %>%
  filter(pitch_type == "FF")

changeups <- SC_data2 %>%
  filter(pitch_type == "CH")

sliders <- SC_data2 %>%
  filter(pitch_type == "SL")

rbind(changeups, sliders) %>%
  filter(pitcher == 680570) %>%
  ggplot(aes(x = pfx_x_diff, y = pfx_z_diff, color = pitch_type)) +
  geom_point() +
  labs(
    title = "Grayson Rodriguez Slider and Changeup 
Movement Difference from Fastball, 2023",
    x = "Horizontal Movement Difference",
    y = "Vertical Movement Difference",
    color = "Pitch Type"
  ) +
  scale_color_manual(values = c("CH" = "blue", "SL" = "orange")) +
  theme(plot.title = element_text(hjust = 0.5))

rbind(changeups, sliders) %>%
  filter(pitcher == 680570) %>%
  group_by(pitch_type) %>%
  summarise(mean(delta_run_exp, na.rm = TRUE))


mean(abs(changeups$pfx_x_diff), na.rm = TRUE)
mean(abs(changeups$pfx_x_diff), na.rm = TRUE)

SC_data2 %>% na.omit() %>%
  filter(pitcher == 680570) %>%
  ggplot(aes(x = spin_axis, y = pfx_x, color = pitch_type)) +
  geom_point() +
  labs(
    title = "Grayson Rodriguez Spin Axis and Horizontal Movement, 2023",
    x = "Spin Axis",
    y = "Horizontal Movement",
    color = "Pitch Type"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


SC_data2 %>% na.omit() %>%
  filter(pitcher == 477132) %>%
  filter(pitch_type == "CU") %>% 
  summarize(mean(spin_axis))
