library(gt)
fb_sumstats <- SC_data2 %>%
  filter(pitch_type == "FF") %>%
  select(-c(game_pk, pitcher, game_year, at_bat_number, pitch_type, p_throws,
            release_speed_diff, release_pos_x_diff, release_pos_z_diff,
            pfx_x_diff, pfx_z_diff, vx0_diff, vy0_diff, vz0_diff,
            ax_diff, ay_diff, az_diff, release_spin_rate_diff,
            release_extension_diff, spin_axis_diff))

calculate_summary_stats <- function(x) {
  c(Min = round(min(x, na.rm = TRUE), 2),
    Q1 = round(quantile(x, 0.25, na.rm = TRUE), 2),
    Median = round(median(x, na.rm = TRUE), 2),
    Mean = round(mean(x, na.rm = TRUE), 2),
    Q3 = round(quantile(x, 0.75, na.rm = TRUE), 2),
    Max = round(max(x, na.rm = TRUE), 2))
}

fb_sumstats_df <- data.frame(
  t(sapply(fb_sumstats, calculate_summary_stats))
)
colnames(fb_sumstats_df) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")

# For Changeup (CH)
ch_sumstats <- SC_data2 %>%
  filter(pitch_type == "CH") %>%
  select(-c(game_pk, pitcher, game_year, at_bat_number, pitch_type, p_throws,
            release_speed_diff, release_pos_x_diff, release_pos_z_diff,
            pfx_x_diff, pfx_z_diff, vx0_diff, vy0_diff, vz0_diff,
            ax_diff, ay_diff, az_diff, release_spin_rate_diff,
            release_extension_diff, spin_axis_diff))

ch_sumstats_df <- data.frame(
  t(sapply(ch_sumstats, calculate_summary_stats))
)
colnames(ch_sumstats_df) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")

# For Curveball (CU)
cu_sumstats <- SC_data2 %>%
  filter(pitch_type == "CU") %>%
  select(-c(game_pk, pitcher, game_year, at_bat_number, pitch_type, p_throws,
            release_speed_diff, release_pos_x_diff, release_pos_z_diff,
            pfx_x_diff, pfx_z_diff, vx0_diff, vy0_diff, vz0_diff,
            ax_diff, ay_diff, az_diff, release_spin_rate_diff,
            release_extension_diff, spin_axis_diff))

cu_sumstats_df <- data.frame(
  t(sapply(cu_sumstats, calculate_summary_stats))
)
colnames(cu_sumstats_df) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")

# For Cutter (FC)
fc_sumstats <- SC_data2 %>%
  filter(pitch_type == "FC") %>%
  select(-c(game_pk, pitcher, game_year, at_bat_number, pitch_type, p_throws,
            release_speed_diff, release_pos_x_diff, release_pos_z_diff,
            pfx_x_diff, pfx_z_diff, vx0_diff, vy0_diff, vz0_diff,
            ax_diff, ay_diff, az_diff, release_spin_rate_diff,
            release_extension_diff, spin_axis_diff))

fc_sumstats_df <- data.frame(
  t(sapply(fc_sumstats, calculate_summary_stats))
)
colnames(fc_sumstats_df) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")

# For Splitter (FS)
fs_sumstats <- SC_data2 %>%
  filter(pitch_type == "FS") %>%
  select(-c(game_pk, pitcher, game_year, at_bat_number, pitch_type, p_throws,
            release_speed_diff, release_pos_x_diff, release_pos_z_diff,
            pfx_x_diff, pfx_z_diff, vx0_diff, vy0_diff, vz0_diff,
            ax_diff, ay_diff, az_diff, release_spin_rate_diff,
            release_extension_diff, spin_axis_diff))

fs_sumstats_df <- data.frame(
  t(sapply(fs_sumstats, calculate_summary_stats))
)
colnames(fs_sumstats_df) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")

# For Sinker (SI)
si_sumstats <- SC_data2 %>%
  filter(pitch_type == "SI") %>%
  select(-c(game_pk, pitcher, game_year, at_bat_number, pitch_type, p_throws,
            release_speed_diff, release_pos_x_diff, release_pos_z_diff,
            pfx_x_diff, pfx_z_diff, vx0_diff, vy0_diff, vz0_diff,
            ax_diff, ay_diff, az_diff, release_spin_rate_diff,
            release_extension_diff, spin_axis_diff))

si_sumstats_df <- data.frame(
  t(sapply(si_sumstats, calculate_summary_stats))
)
colnames(si_sumstats_df) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")

# For Slider (SL)
sl_sumstats <- SC_data2 %>%
  filter(pitch_type == "SL") %>%
  select(-c(game_pk, pitcher, game_year, at_bat_number, pitch_type, p_throws,
            release_speed_diff, release_pos_x_diff, release_pos_z_diff,
            pfx_x_diff, pfx_z_diff, vx0_diff, vy0_diff, vz0_diff,
            ax_diff, ay_diff, az_diff, release_spin_rate_diff,
            release_extension_diff, spin_axis_diff))

sl_sumstats_df <- data.frame(
  t(sapply(sl_sumstats, calculate_summary_stats))
)
colnames(sl_sumstats_df) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max")


si_sumstats_df %>%
  gt(rownames_to_stub = TRUE) %>%
  tab_header(
    title = "Sinkers, 2022-2023"
  ) %>%
  fmt_number(
    columns = c("Min", "Q1", "Median", "Mean", "Q3", "Max"),
    decimals = 2
  ) %>%
  tab_spanner(
    label = "Summary Statistics",
    columns = c("Min", "Q1", "Median", "Mean", "Q3", "Max")
  )
