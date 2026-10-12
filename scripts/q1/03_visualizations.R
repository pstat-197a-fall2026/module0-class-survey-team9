library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

data_dir <- Sys.getenv("Q1_DATA_DIR", unset = "")
out_dir  <- Sys.getenv("Q1_OUT_DIR",  unset = "results/q1/figures")

# Creating Paths
if (data_dir == "") {
  freq_path    <- "results/q1/tables/course_frequency.csv"
  summary_path <- "results/q1/tables/group_summary.csv"
  analysis_path <- "data/processed/q1/q1-analysis.csv"
} else {
  freq_path    <- file.path(data_dir, "course_frequency.csv")
  summary_path <- file.path(data_dir, "group_summary.csv")
  analysis_path <- file.path(data_dir, "q1-analysis.csv")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Data ------------------------------------------------------------------
course_freq   <- read_csv(freq_path, show_col_types = FALSE)
group_summary <- read_csv(summary_path, show_col_types = FALSE)
analysis      <- read_csv(analysis_path, show_col_types = FALSE)

rating_levels <- sort(unique(analysis$stat_comfort))
analysis <- analysis |>
  mutate(comfort_f = factor(stat_comfort, levels = rating_levels))

x_breaks <- seq(min(analysis$n_ud_pstat), max(analysis$n_ud_pstat))
small_cut <- 5  # groups with fewer than this many students are flagged

# Shared styling: one ordered palette used for every rating-coloured figure
comfort_cols <- setNames(
  scales::viridis_pal(option = "D", begin = 0.05, end = 0.95)(length(rating_levels)),
  rating_levels
)
base_theme <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey30"),
        plot.caption = element_text(colour = "grey40", hjust = 0),
        panel.grid.minor = element_blank())
note <- "Sample patterns only; not evidence of statistical significance or causation."

save_fig <- function(p, name, w = 8, h = 5.5) {
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
}

# ---- Figure 1: distribution of course counts -------------------------------
course_freq <- course_freq |> mutate(small = n_students < small_cut)

fig1 <- ggplot(course_freq, aes(factor(n_ud_pstat), n_students, fill = small)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = n_students), vjust = -0.4, size = 3.8) +
  scale_fill_manual(values = c(`FALSE` = "#3B6EA8", `TRUE` = "#D98C3F"),
                    labels = c(`FALSE` = paste0(small_cut, "+ students"),
                               `TRUE` = paste0("Fewer than ", small_cut, " students")),
                    name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Figure 1. Distribution of upper-division PSTAT course counts",
       subtitle = "Most students fall in the middle of the range; the extremes have very few students",
       x = "Number of UD PSTAT courses taken", y = "Number of students") +
  base_theme + theme(legend.position = "bottom", panel.grid.major.x = element_blank())
save_fig(fig1, "fig1_course_count_distribution")

# ---- Figure 2: bubble plot -------------------------------------------------
bubble <- analysis |> count(n_ud_pstat, stat_comfort, name = "n")

fig2 <- ggplot(bubble, aes(n_ud_pstat, stat_comfort, size = n)) +
  geom_point(colour = "#3B6EA8", alpha = 0.65) +
  scale_size_area(max_size = 16, breaks = pretty_breaks(4), name = "Number of\nstudents") +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = rating_levels, limits = range(rating_levels) + c(-0.4, 0.4)) +
  labs(title = "Figure 2. UD PSTAT courses vs. statistics comfort",
       subtitle = "Bubble area is proportional to the number of students with that combination",
       x = "Number of UD PSTAT courses taken", y = "Self-reported statistics comfort",
       caption = note) +
  base_theme
save_fig(fig2, "fig2_bubble_courses_vs_comfort")

# ---- Figure 3: 100% stacked bars -------------------------------------------
pct <- analysis |>
  count(n_ud_pstat, comfort_f, name = "n") |>
  group_by(n_ud_pstat) |>
  mutate(total = sum(n), pct = n / total) |>
  ungroup()
totals <- distinct(pct, n_ud_pstat, total)

fig3 <- ggplot(pct, aes(factor(n_ud_pstat), pct, fill = comfort_f)) +
  geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
  geom_text(data = totals, aes(factor(n_ud_pstat), 1, label = paste0("n = ", total)),
            inherit.aes = FALSE, vjust = -0.5, size = 3.6) +
  scale_fill_manual(values = comfort_cols, name = "Comfort\nrating", drop = FALSE) +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.08)),
                     breaks = seq(0, 1, 0.25)) +
  labs(title = "Figure 3. Comfort ratings by UD PSTAT course count",
       subtitle = "Percent of students within each course-count group; n above each bar",
       x = "Number of UD PSTAT courses taken", y = "Percent of students within group",
       caption = paste0("Groups with fewer than ", small_cut, " students give little information.\n", note)) +
  base_theme + theme(panel.grid.major.x = element_blank())
save_fig(fig3, "fig3_comfort_distribution_by_courses")

# ---- Figure 4: mean comfort ------------------------------------------------
group_summary <- group_summary |> mutate(small = n_students < small_cut)

fig4 <- ggplot(group_summary, aes(n_ud_pstat, mean_comfort)) +
  geom_line(colour = "grey70", linewidth = 0.6) +
  geom_point(aes(shape = small, colour = small), size = 3.5) +
  geom_text(aes(label = paste0("n = ", n_students)), vjust = -1.2, size = 3.5) +
  scale_colour_manual(values = c(`FALSE` = "#3B6EA8", `TRUE` = "#D98C3F"),
                      labels = c(`FALSE` = paste0(small_cut, "+ students"),
                                 `TRUE` = paste0("Fewer than ", small_cut, " students")),
                      name = NULL) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17),
                     labels = c(`FALSE` = paste0(small_cut, "+ students"),
                                `TRUE` = paste0("Fewer than ", small_cut, " students")),
                     name = NULL) +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(limits = range(rating_levels) + c(-0.25, 0.4), breaks = rating_levels,
                     expand = expansion(mult = 0)) +
  labs(title = "Figure 4. Mean statistics comfort by UD PSTAT course count",
       subtitle = "Labels show group size; interpret small groups cautiously",
       x = "Number of UD PSTAT courses taken", y = "Mean comfort rating",
       caption = paste0("Comfort is an ordinal rating, so the mean assumes equal spacing between categories.\n", note)) +
  base_theme + theme(legend.position = "bottom")
save_fig(fig4, "fig4_mean_comfort_by_courses")

message("Saved figures to: ", normalizePath(out_dir))
