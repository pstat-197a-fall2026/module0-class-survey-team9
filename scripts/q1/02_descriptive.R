# Summarize Q1 data without plots or modeling. Run script 01 first.
library(dplyr)
library(tibble)

# Load the prepared dataset; do not recalculate course counts
analysis <- read.csv('data/processed/q1/q1-analysis.csv') %>% as_tibble()
stopifnot(!anyDuplicated(analysis$response_id))
print(analysis %>% summarise(across(c(n_ud_pstat, stat_comfort), ~ sum(is.na(.x)))))
# Stop for an explicit missing-data decision rather than silently omit students.
stopifnot(!anyNA(analysis[c('response_id', 'n_ud_pstat', 'stat_comfort')]),
          all(analysis$n_ud_pstat >= 0 & analysis$n_ud_pstat == floor(analysis$n_ud_pstat)),
          all(analysis$stat_comfort %in% 2:5))

# Overall summaries for both variables
overall <- tibble(variable = rep(c('n_ud_pstat', 'stat_comfort'), each = nrow(analysis)),
                  value = c(analysis$n_ud_pstat, analysis$stat_comfort)) %>%
  group_by(variable) %>%
  summarise(n_students = n(), mean = mean(value), median = median(value),
            sd = sd(value), min = min(value), max = max(value), .groups = 'drop')

# Frequencies and percentages
course_frequency <- analysis %>% count(n_ud_pstat, name = 'n_students') %>%
  mutate(percent = 100 * n_students / sum(n_students)) %>% arrange(n_ud_pstat)
# Metadata documents categories 2-5; retain rating 2 even with zero responses.
comfort_frequency <- tibble(stat_comfort = 2:5) %>%
  left_join(analysis %>% count(stat_comfort, name = 'n_students'), by = 'stat_comfort') %>%
  mutate(n_students = coalesce(n_students, 0L),
         percent = 100 * n_students / sum(n_students)) %>% arrange(stat_comfort)

# Comfort summaries by course count; singleton sample SD stays NA
group_summary <- analysis %>% group_by(n_ud_pstat) %>%
  summarise(n_students = n(), mean_comfort = mean(stat_comfort),
            median_comfort = median(stat_comfort), sd_comfort = sd(stat_comfort),
            min_comfort = min(stat_comfort), max_comfort = max(stat_comfort),
            pct_comfort_4_or_5 = 100 * mean(stat_comfort %in% c(4, 5)),
            .groups = 'drop') %>% arrange(n_ud_pstat)
message('Groups with fewer than five students:')
print(group_summary %>% filter(n_students < 5), width = Inf)

# Validate full-precision results before rounding exports
stopifnot(sum(course_frequency$n_students) == nrow(analysis),
          sum(comfort_frequency$n_students) == nrow(analysis),
          sum(group_summary$n_students) == nrow(analysis),
          all(overall$n_students == nrow(analysis)),
          all(group_summary$n_students == course_frequency$n_students),
          all(overall$mean >= overall$min & overall$mean <= overall$max),
          all(overall$median >= overall$min & overall$median <= overall$max),
          all(group_summary$mean_comfort >= group_summary$min_comfort &
                group_summary$mean_comfort <= group_summary$max_comfort),
          all(group_summary$median_comfort >= group_summary$min_comfort &
                group_summary$median_comfort <= group_summary$max_comfort),
          all(is.na(group_summary$sd_comfort) == (group_summary$n_students == 1)),
          abs(weighted.mean(group_summary$mean_comfort, group_summary$n_students) -
                mean(analysis$stat_comfort)) < 1e-8)
for (frequency in list(course_frequency, comfort_frequency)) {
  stopifnot(abs(sum(frequency$percent) - 100) < 1e-8,
            all(frequency$percent >= 0 & frequency$percent <= 100))
}
stopifnot(all(group_summary$pct_comfort_4_or_5 >= 0 &
                group_summary$pct_comfort_4_or_5 <= 100))

# Export four tables; round statistics only, keeping integer counts unchanged
dir.create('results/q1/tables', recursive = TRUE, showWarnings = FALSE)
tables <- list(overall_summary = overall, course_frequency = course_frequency,
               comfort_frequency = comfort_frequency, group_summary = group_summary)
for (name in names(tables)) {
  table <- tables[[name]] %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
  write.csv(table, paste0('results/q1/tables/', name, '.csv'), row.names = FALSE, na = '')
}
print(overall)
message('Four descriptive tables saved; consistency checks passed.')
