# 05 supplements 04, I made visualizations of the findings in 04.

library(readr) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ordinal)
library(mgcv)

data_dir <- Sys.getenv("Q1_DATA_DIR", unset = "")
out_dir  <- Sys.getenv("Q1_OUT_DIR",  unset = "results/q1/figures")
analysis_path <- if (data_dir == "") "data/processed/q1/q1-analysis.csv" else file.path(data_dir, "q1-analysis.csv")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Data and models 
model_data <- read_csv(analysis_path, show_col_types = FALSE) |>
  select(n_ud_pstat, stat_comfort) |> drop_na() |>
  mutate(stat_comfort = ordered(stat_comfort, levels = 1:5))

ordinal_fit <- clm(stat_comfort ~ n_ud_pstat, data = model_data, link = "logit")

model_data$stat_comfort_int <- as.integer(as.character(model_data$stat_comfort))
gam_fit <- gam(stat_comfort_int ~ s(n_ud_pstat, k = 3), family = ocat(R = 5), data = model_data)

sp <- cor.test(model_data$n_ud_pstat, as.numeric(model_data$stat_comfort), method = "spearman", exact = FALSE)

# Pull the key numbers used in annotations
coef_row <- coef(summary(ordinal_fit))["n_ud_pstat", ]
clm_or   <- exp(coef_row["Estimate"]); clm_p <- coef_row["Pr(>|z|)"]
gam_p    <- summary(gam_fit)$s.table[1, "p-value"]
gam_dev  <- summary(gam_fit)$dev.expl
rho <- unname(sp$estimate); sp_p <- sp$p.value

# Styling (matches q1_figures.R)
rating_levels <- 1:5
# Same ordered palette as q1_figures.R: built from the ratings actually observed
obs_levels <- sort(unique(model_data$stat_comfort_int))
comfort_cols <- setNames(scales::viridis_pal(option = "D", begin = 0.05, end = 0.95)(length(obs_levels)), obs_levels)
base_theme <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(colour = "grey30"),
        plot.caption = element_text(colour = "grey40", hjust = 0), panel.grid.minor = element_blank())
note <- "Sample patterns only; not evidence of causation."
save_fig <- function(p, name, w = 8, h = 5.5)
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white")

grid <- data.frame(n_ud_pstat = seq(min(model_data$n_ud_pstat), max(model_data$n_ud_pstat), length.out = 100))
x_breaks <- seq(min(model_data$n_ud_pstat), max(model_data$n_ud_pstat))
obs_means <- model_data |> group_by(n_ud_pstat) |>
  summarise(mean_comfort = mean(stat_comfort_int), n = n(), .groups = "drop")

# Figure 5: ordinal logistic predicted probabilities
# Predicted probability of each rating across the observed range (with 95% CI)
pr <- predict(ordinal_fit, newdata = grid["n_ud_pstat"], type = "prob", se.fit = TRUE, interval = TRUE)
# clm() drops rating levels with no observations (1 and 2 here), so index by column name
obs_ratings <- colnames(pr$fit)
prob_long <- bind_rows(lapply(obs_ratings, function(k) {
  data.frame(n_ud_pstat = grid$n_ud_pstat, rating = factor(k, levels = 1:5),
             fit = pr$fit[, k], lwr = pr$lwr[, k], upr = pr$upr[, k])
}))

fig5 <- ggplot(prob_long, aes(n_ud_pstat, fit, colour = rating, fill = rating)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.22, colour = NA) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ rating, nrow = 1, labeller = labeller(rating = function(x) paste("Comfort", x))) +
  scale_colour_manual(values = comfort_cols, guide = "none") +
  scale_fill_manual(values = comfort_cols, guide = "none") +
  scale_x_continuous(breaks = seq(2, 10, 2)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(title = "Figure 5. Ordinal logistic model: predicted comfort probabilities",
       subtitle = sprintf("Probabilities barely move with course count\n(odds ratio per course = %.2f, p = %.2f)", clm_or, clm_p),
       x = "Number of UD PSTAT courses taken", y = "Predicted probability",
       caption = paste0("Lines are model predictions; bands are 95% confidence intervals.\n", note)) +
  base_theme + theme(panel.spacing = unit(1, "lines"), strip.text = element_text(face = "bold"))
save_fig(fig5, "fig5_ordinal_predicted_probabilities", w = 10, h = 5)

# Figure 6: expected comfort, linear-in-logit model vs GAM vs observed
pm <- as.matrix(predict(ordinal_fit, newdata = grid["n_ud_pstat"], type = "prob")$fit)
exp_clm <- pm %*% as.numeric(colnames(pm))
exp_gam <- predict(gam_fit, newdata = grid, type = "response") %*% 1:5
curves <- bind_rows(
  data.frame(n_ud_pstat = grid$n_ud_pstat, expected = as.numeric(exp_clm), model = "Ordinal logistic (linear on logit scale)"),
  data.frame(n_ud_pstat = grid$n_ud_pstat, expected = as.numeric(exp_gam), model = "Ordinal GAM (flexible)"))

fig6 <- ggplot() +
  geom_line(data = curves, aes(n_ud_pstat, expected, colour = model, linetype = model), linewidth = 1.2) +
  geom_point(data = obs_means, aes(n_ud_pstat, mean_comfort, size = n), shape = 21,
             fill = "grey80", colour = "grey30", alpha = 0.85) +
  scale_colour_manual(values = c("#3B6EA8", "#D98C3F"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed"), name = NULL) +
  scale_size_area(max_size = 10, breaks = c(2, 6, 12, 18), name = "Students in\ngroup (observed)") +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(limits = c(3, 5), breaks = 3:5) +
  labs(title = "Figure 6. Model-implied vs. observed mean comfort",
       subtitle = "The logistic model is nearly flat; the GAM's U-shape is driven by tiny groups at the ends\n(gray points are observed group means, sized by group n)",
       x = "Number of UD PSTAT courses taken", y = "Expected comfort rating",
       caption = paste0("Expected rating = sum of rating x predicted probability.\n", note)) +
  base_theme + theme(legend.position = "right")
save_fig(fig6, "fig6_model_expected_comfort", w = 9)

# Figure 7: GAM smooth term on the latent (ordinal logit) scale
pg <- predict(gam_fit, newdata = grid, type = "terms", se.fit = TRUE)
smooth_df <- data.frame(n_ud_pstat = grid$n_ud_pstat, fit = pg$fit[, 1], se = pg$se.fit[, 1]) |>
  mutate(lwr = fit - 1.96 * se, upr = fit + 1.96 * se)

fig7 <- ggplot(smooth_df, aes(n_ud_pstat, fit)) +
  geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#3B6EA8", alpha = 0.2) +
  geom_line(colour = "#3B6EA8", linewidth = 1.2) +
  geom_rug(data = model_data, aes(x = n_ud_pstat, y = NULL), sides = "b", alpha = 0.3, length = unit(0.03, "npc")) +
  scale_x_continuous(breaks = x_breaks) +
  labs(title = "Figure 7. Ordinal GAM: smooth effect of course count",
       subtitle = sprintf("The 95%% band includes or just touches zero throughout\n(smooth term p = %.2f; %.1f%% deviance explained)", gam_p, 100 * gam_dev),
       x = "Number of UD PSTAT courses taken", y = "Effect on latent comfort (centered)",
       caption = paste0("Rug marks show where students' course counts fall; the band is wide where data are sparse.\n", note)) +
  base_theme
save_fig(fig7, "fig7_gam_smooth_effect")

# Figure 8: Spearman rank correlation
rank_df <- model_data |> mutate(rank_courses = rank(n_ud_pstat), rank_comfort = rank(as.numeric(stat_comfort)))
fig8 <- ggplot(rank_df, aes(rank_courses, rank_comfort)) +
  geom_jitter(width = 0.8, height = 0.8, alpha = 0.55, colour = "#3B6EA8", size = 2.2) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "#D98C3F", fill = "#D98C3F", alpha = 0.2) +
  labs(title = "Figure 8. Spearman rank correlation",
       subtitle = sprintf("rho = %.2f, p = %.2f: essentially no monotonic association", rho, sp_p),
       x = "Rank of UD PSTAT course count", y = "Rank of comfort rating",
       caption = paste0("Both variables converted to ranks (ties averaged); points jittered to reduce overlap.\nOrange line is a least-squares line through the ranks with 95% band.\n", note)) +
  base_theme
save_fig(fig8, "fig8_spearman_rank_correlation")

message(sprintf("OR=%.3f p=%.3f | GAM p=%.3f dev=%.3f | rho=%.3f p=%.3f", clm_or, clm_p, gam_p, gam_dev, rho, sp_p))
message("Saved figures to: ", normalizePath(out_dir))
