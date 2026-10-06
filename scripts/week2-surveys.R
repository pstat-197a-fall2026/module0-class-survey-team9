# PSTAT 197A: class survey analysis, Fall 2026
# The data folder should be in the project root.
# Required packages: tidyverse and broom.

library(tidyverse)

background <- read_csv('data/background-clean.csv', show_col_types = FALSE)
interest <- read_csv('data/interest-clean.csv', show_col_types = FALSE)

# Each file contains only responses with consent for that section:
# 51 background responses and 49 project-preference responses.
# response_id is an artificial identifier used to join the two files.

# Check the number of responses and the available variables.
nrow(background)
nrow(interest)
glimpse(background)
glimpse(interest)

# Proficiency categories have a meaningful order.
# Converting them to factors here keeps that order in subsequent summaries.
proficiency_levels <- c('beg', 'int', 'adv')

background <- background %>%
  mutate(across(ends_with('.prof'),
                ~ factor(.x, levels = proficiency_levels, ordered = TRUE)))

## individual variable summaries
###############################

# one variable, one summary
background %>%
  summarise(mean = mean(math.comf, na.rm = TRUE))

# many variables, same summary
background %>%
  summarise(across(ends_with('.comf'), ~ mean(.x, na.rm = TRUE)))

background %>%
  summarise(across(ends_with('.comf'), ~ median(.x, na.rm = TRUE)))

# many variables, many summaries
# The suffix after the underscore identifies the summary statistic.
background %>%
  summarise(across(
    ends_with('.comf'),
    list(mean = ~ mean(.x, na.rm = TRUE),
         median = ~ median(.x, na.rm = TRUE),
         min = ~ min(.x, na.rm = TRUE),
         max = ~ max(.x, na.rm = TRUE)),
    .names = '{.col}_{.fn}'
  )) %>%
  pivot_longer(everything(),
               names_to = c('variable', '.value'), names_sep = '_')

# proficiency responses are factors
# Without specified levels, factor() uses alphabetical order.
background %>%
  pull(math.prof) %>%
  as.character() %>%
  factor() %>%
  fct_count()

# but levels have a substantive order: beginner, intermediate, advanced
background %>%
  pull(math.prof) %>%
  fct_count()

# same summary, each variable
# A long table gives each skill/level combination its own row.
# .drop = FALSE retains categories with zero responses.
proficiency_counts <- background %>%
  select(ends_with('.prof')) %>%
  pivot_longer(everything(), names_to = 'skill', values_to = 'level') %>%
  count(skill, level, .drop = FALSE)

proficiency_counts

# clean up names and arrange skills in columns
# The escaped dot is literal; $ restricts the match to the end of a name.
proficiency_counts %>%
  mutate(skill = str_remove(skill, '\\.prof$')) %>%
  pivot_wider(names_from = skill, values_from = n, values_fill = 0) %>%
  arrange(level)

# what about converting to numeric? is this meaningful?
# The factor order gives beg = 1, int = 2, adv = 3.
# Means impose equal spacing between categories. That is an assumption,
# not something established by the survey responses.
background %>%
  select(ends_with('.prof')) %>%
  rename_with(~ str_remove(.x, '\\.prof$')) %>%
  mutate(across(everything(), as.numeric)) %>%
  summarise(across(
    everything(),
    list(mean = ~ mean(.x, na.rm = TRUE),
         median = ~ median(.x, na.rm = TRUE)),
    .names = '{.col}_{.fn}'
  )) %>%
  pivot_longer(everything(),
               names_to = c('variable', '.value'), names_sep = '_')

## multivariable thinking
########################

# unique combinations of proficiency ratings
proficiency <- background %>%
  select(ends_with('.prof')) %>%
  mutate(across(everything(), as.numeric))

proficiency %>%
  rename_with(~ str_remove(.x, '\\.prof$')) %>%
  count(prog, math, stat)

# unique combinations of comfort ratings
comfort <- background %>%
  select(ends_with('.comf'))

comfort %>%
  rename_with(~ str_remove(.x, '\\.comf$')) %>%
  count(prog, math, stat)

# Is project preference associated with self-assessed proficiency?
# Join by ID, not row position. The files have different numbers of rows.
# An inner join includes only respondents who consented to both sections.
# type: ind = industry, lab = research lab, both = no preference.
joint <- interest %>%
  inner_join(background, by = 'response_id')

nrow(joint)

# Use the same fixed score intervals as the slides.
# These intervals keep tied scores together; they are not equal-sized groups.
preference_counts <- joint %>%
  mutate(across(ends_with('.prof'), as.numeric)) %>%
  mutate(mean_proficiency = rowMeans(pick(ends_with('.prof'))),
         proficiency_group = cut(
           mean_proficiency,
           breaks = c(-Inf, 2, 2.5, Inf),
           labels = c('1.00-2.00', '>2.00-2.50', '>2.50-3.00')
         )) %>%
  count(proficiency_group, type) %>%
  complete(proficiency_group, type = c('ind', 'lab', 'both'),
           fill = list(n = 0)) %>%
  pivot_wider(names_from = type, values_from = n, values_fill = 0)

preference_counts

# proportions within each proficiency group
# An empty group has undefined proportions, reported as NA.
preference_counts %>%
  mutate(n = ind + lab + both,
         across(c(ind, lab, both), ~ .x / na_if(n, 0)))

## clustering
#############

# proficiency and comfort retain the same row order from background.
# Do not include response_id in the clustering variables.
cluster_data <- bind_cols(proficiency, comfort)

# The supplied data have complete ratings. Stop if that changes so that
# missing-data handling becomes an explicit analysis decision.
stopifnot(all(complete.cases(cluster_data)))

# Cluster responses into three groups, as in the slides.
# Multiple random starts reduce dependence on the initial centers.
set.seed(92922)
clust <- kmeans(cluster_data, centers = 3, nstart = 25)

broom::tidy(clust)

# Proficiency uses scores 1-3. Comfort retains its original numeric scale.
# We have not standardized the variables, so scales affect distances.
# What happens if you standardize them or change the number of clusters?

# Plot the fitted clusters using the first two principal components.
# PCA is only a visualization here; clustering used all six variables.
projection <- prcomp(cluster_data, center = TRUE, scale. = FALSE)

as_tibble(projection$x[, 1:2]) %>%
  mutate(cluster = factor(clust$cluster)) %>%
  ggplot(aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.75) +
  labs(x = 'Principal component 1',
       y = 'Principal component 2',
       color = 'Cluster')

# Cluster numbers are arbitrary labels. Interpret the fitted centers.
# Several respondents can overlap in the plot because ratings are discrete.

## summary of classes taken
###########################

# Courses are now stored in ONE comma-separated column named courses.
# There are no longer separate yes/no columns for each course.
# Put each respondent/course combination on its own row.
course_responses <- background %>%
  select(response_id, courses) %>%
  separate_rows(courses, sep = ',\\s*') %>%
  mutate(courses = str_trim(courses)) %>%
  filter(!is.na(courses), courses != '') %>%
  distinct(response_id, courses)

# Each denominator is the number of shared background respondents (51),
# not the total survey count (55) or the number of course selections.
# The supplied background data have no missing course responses.
# Courses not selected by anyone will not appear in this table.
classes <- course_responses %>%
  count(courses, name = 'n') %>%
  rename(class = courses) %>%
  mutate(proportion = n / nrow(background)) %>%
  arrange(desc(proportion))

classes

classes %>%
  ggplot(aes(x = proportion, y = reorder(class, proportion))) +
  geom_point() +
  scale_x_sqrt() +
  labs(x = 'Proportion of shared background respondents', y = '')

# Each course-series selection means at least one course in that series,
# not necessarily completion of the entire series.

## ideas for extending the analysis
##################################

# Explore associations between coursework and self-assessed skills.
# Compare language preferences or summarize domains of interest.
# Try alternative clustering variables, scaling choices, or values of k.
# Keep conclusions descriptive and identify which consent subset you use.
#
# Be careful with interest$areas: some option labels contain commas,
# including Predictive modeling, generally. Splitting on every comma would
# break those labels. Consider str_detect() with fixed() for full labels.
