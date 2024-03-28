library(caret)
library(xgboost)
library(data.table)

as.data.table(xgb.importance(model = stuff_fastball_model$finalModel)) %>%
  as.data.frame() %>%
  setNames(c("Variable", "Gain", "Cover", "Frequency")) %>%
  group_by(Category = case_when(
    Variable %in% c("vx0", "ax", "pfx_x") ~ "Horizontal Movement",
    Variable %in% c("vz0", "az", "pfx_z") ~ "Vertical Movement",
    Variable %in% c("release_speed", "vy0", "ay") ~ "Speed",
    Variable %in% c("spin_axis", "release_spin_rate") ~ "Spin",
    Variable %in% c("release_pos_x", "release_pos_z", "release_extension", "p_throwsR") ~ "Release Point"
  )) %>%
  summarize(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE)
  ) %>%
  arrange(desc(Gain)) %>%
  ggplot(aes(x = reorder(Category, Gain), y = Gain)) +
  geom_bar(stat = "identity", color = "#F76900", fill = "#F76900") +
  ggtitle("Fastball Stuff+ Variable Importance") +
  xlab("Variable Type") +
  ylab("Relative Importance") +
  theme_minimal() + theme(
    plot.title = element_text(hjust = 0.35), 
    plot.subtitle = element_text(hjust = 0.5, face = "italic"), # Italicize and center subtitle
    axis.title.y = element_text(vjust = 0.5, color = "black"),
    axis.title.x = element_text(vjust = -0.5, color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  coord_flip()

as.data.table(xgb.importance(model = stuff_sinker_model$finalModel)) %>%
  as.data.frame() %>%
  setNames(c("Variable", "Gain", "Cover", "Frequency")) %>%
  group_by(Category = case_when(
    Variable %in% c("vx0", "ax", "pfx_x") ~ "Horizontal Movement",
    Variable %in% c("vz0", "az", "pfx_z") ~ "Vertical Movement",
    Variable %in% c("release_speed", "vy0", "ay") ~ "Speed",
    Variable %in% c("spin_axis", "release_spin_rate") ~ "Spin",
    Variable %in% c("release_pos_x", "release_pos_z", "release_extension", "p_throwsR") ~ "Release Point"
  )) %>%
  summarize(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE)
  ) %>%
  arrange(desc(Gain)) %>%
  ggplot(aes(x = reorder(Category, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  labs(title = "Sinker Stuff+ Variable Importance",
       x = "Variable Type",
       y = "Relative Importance") +
  theme_minimal() +
  coord_flip()

as.data.table(xgb.importance(model = stuff_cutter_model$finalModel)) %>%
  as.data.frame() %>%
  setNames(c("Variable", "Gain", "Cover", "Frequency")) %>%
  group_by(Category = case_when(
    Variable %in% c("vx0", "ax", "pfx_x") ~ "Horizontal Movement",
    Variable %in% c("vz0", "az", "pfx_z") ~ "Vertical Movement",
    Variable %in% c("release_speed", "vy0", "ay") ~ "Speed",
    Variable %in% c("spin_axis", "release_spin_rate") ~ "Spin",
    Variable %in% c("release_pos_x", "release_pos_z", "release_extension", "p_throwsR") ~ "Release Point"
  )) %>%
  summarize(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE)
  ) %>%
  arrange(desc(Gain)) %>%
  ggplot(aes(x = reorder(Category, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  labs(title = "Cutter Stuff+ Variable Importance",
       x = "Variable Type",
       y = "Relative Importance") +
  theme_minimal() +
  coord_flip()

as.data.table(xgb.importance(model = stuff_slider_model$finalModel)) %>%
  as.data.frame() %>%
  setNames(c("Variable", "Gain", "Cover", "Frequency")) %>%
  group_by(Category = case_when(
    Variable %in% c("vx0", "ax", "pfx_x") ~ "Horizontal Movement",
    Variable %in% c("vz0", "az", "pfx_z") ~ "Vertical Movement",
    Variable %in% c("release_speed", "vy0", "ay") ~ "Speed",
    Variable %in% c("spin_axis", "release_spin_rate") ~ "Spin",
    Variable %in% c("release_pos_x", "release_pos_z", "release_extension") ~ "Release Point",
    Variable %in% c("vx0_diff", "ax_diff", "pfx_x_diff") ~ "Horizontal Movement Difference",
    Variable %in% c("vz0_diff", "az_diff", "pfx_z_diff") ~ "Vertical Movement Difference",
    Variable %in% c("release_speed_diff", "vy0_diff", "ay_diff") ~ "Speed Difference",
    Variable %in% c("spin_axis_diff", "release_spin_rate_diff") ~ "Spin Difference",
    Variable %in% c("release_pos_x_diff", "release_pos_z_diff", "release_extension_diff") ~ "Release Point Difference"
  )) %>%
  summarize(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE)
  ) %>%
  arrange(desc(Gain)) %>%
  ggplot(aes(x = reorder(Category, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  labs(title = "Slider Stuff+ Variable Importance",
       x = "Variable Type",
       y = "Relative Importance") +
  theme_minimal() +
  coord_flip()

as.data.table(xgb.importance(model = stuff_curveball_model$finalModel)) %>%
  as.data.frame() %>%
  setNames(c("Variable", "Gain", "Cover", "Frequency")) %>%
  group_by(Category = case_when(
    Variable %in% c("vx0", "ax", "pfx_x") ~ "Horizontal Movement",
    Variable %in% c("vz0", "az", "pfx_z") ~ "Vertical Movement",
    Variable %in% c("release_speed", "vy0", "ay") ~ "Speed",
    Variable %in% c("spin_axis", "release_spin_rate") ~ "Spin",
    Variable %in% c("release_pos_x", "release_pos_z", "release_extension", "p_throwsR") ~ "Release Point",
    Variable %in% c("vx0_diff", "ax_diff", "pfx_x_diff") ~ "Horizontal Movement Difference",
    Variable %in% c("vz0_diff", "az_diff", "pfx_z_diff") ~ "Vertical Movement Difference",
    Variable %in% c("release_speed_diff", "vy0_diff", "ay_diff") ~ "Speed Difference",
    Variable %in% c("spin_axis_diff", "release_spin_rate_diff") ~ "Spin Difference",
    Variable %in% c("release_pos_x_diff", "release_pos_z_diff", "release_extension_diff") ~ "Release Point Difference"
  )) %>%
  summarize(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE)
  ) %>%
  arrange(desc(Gain)) %>%
  ggplot(aes(x = reorder(Category, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  labs(title = "Curveball Stuff+ Variable Importance",
       x = "Variable Type",
       y = "Relative Importance") +
  theme_minimal() +
  coord_flip()

as.data.table(xgb.importance(model = stuff_splitter_model$finalModel)) %>%
  as.data.frame() %>%
  setNames(c("Variable", "Gain", "Cover", "Frequency")) %>%
  group_by(Category = case_when(
    Variable %in% c("vx0", "ax", "pfx_x") ~ "Horizontal Movement",
    Variable %in% c("vz0", "az", "pfx_z") ~ "Vertical Movement",
    Variable %in% c("release_speed", "vy0", "ay") ~ "Speed",
    Variable %in% c("spin_axis", "release_spin_rate") ~ "Spin",
    Variable %in% c("release_pos_x", "release_pos_z", "release_extension", "p_throwsR") ~ "Release Point",
    Variable %in% c("vx0_diff", "ax_diff", "pfx_x_diff") ~ "Horizontal Movement Difference",
    Variable %in% c("vz0_diff", "az_diff", "pfx_z_diff") ~ "Vertical Movement Difference",
    Variable %in% c("release_speed_diff", "vy0_diff", "ay_diff") ~ "Speed Difference",
    Variable %in% c("spin_axis_diff", "release_spin_rate_diff") ~ "Spin Difference",
    Variable %in% c("release_pos_x_diff", "release_pos_z_diff", "release_extension_diff") ~ "Release Point Difference"
  )) %>%
  summarize(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE)
  ) %>%
  arrange(desc(Gain)) %>%
  ggplot(aes(x = reorder(Category, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  labs(title = "Splitter Stuff+ Variable Importance",
       x = "Variable Type",
       y = "Relative Importance") +
  theme_minimal() +
  coord_flip()

as.data.table(xgb.importance(model = stuff_changeup_model$finalModel)) %>%
  as.data.frame() %>%
  setNames(c("Variable", "Gain", "Cover", "Frequency")) %>%
  group_by(Category = case_when(
    Variable %in% c("vx0", "ax", "pfx_x") ~ "Horizontal Movement",
    Variable %in% c("vz0", "az", "pfx_z") ~ "Vertical Movement",
    Variable %in% c("release_speed", "vy0", "ay") ~ "Speed",
    Variable %in% c("spin_axis", "release_spin_rate") ~ "Spin",
    Variable %in% c("release_pos_x", "release_pos_z", "release_extension", "p_throwsR") ~ "Release Point",
    Variable %in% c("vx0_diff", "ax_diff", "pfx_x_diff") ~ "Horizontal Movement Difference",
    Variable %in% c("vz0_diff", "az_diff", "pfx_z_diff") ~ "Vertical Movement Difference",
    Variable %in% c("release_speed_diff", "vy0_diff", "ay_diff") ~ "Speed Difference",
    Variable %in% c("spin_axis_diff", "release_spin_rate_diff") ~ "Spin Difference",
    Variable %in% c("release_pos_x_diff", "release_pos_z_diff", "release_extension_diff") ~ "Release Point Difference"
  )) %>%
  summarize(
    Gain = sum(Gain, na.rm = TRUE),
    Cover = sum(Cover, na.rm = TRUE),
    Frequency = sum(Frequency, na.rm = TRUE)
  ) %>%
  arrange(desc(Gain)) %>%
  ggplot(aes(x = reorder(Category, Gain), y = Gain)) +
  geom_bar(stat = "identity", color = "#F76900", fill = "#F76900") +
  ggtitle("Changeup Stuff+ Variable Importance") +
  xlab("Variable Type") +
  ylab("Relative Importance") +
  theme_minimal() + theme(
    plot.title = element_text(hjust = 0.1), 
    plot.subtitle = element_text(hjust = 0.5, face = "italic"), # Italicize and center subtitle
    axis.title.y = element_text(vjust = 0.5, color = "black"),
    axis.title.x = element_text(vjust = -0.5, color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  coord_flip()
