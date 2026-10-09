# Prepare and validate Q1 data. Run from the project root before script 02.
library(dplyr)
library(stringr)
library(tibble)

# Load survey data and check IDs and missing values
background <- read.csv('data/background-clean-final-for-problem-1.csv',
                       check.names = FALSE, stringsAsFactors = FALSE) %>%
  as_tibble()
required <- c('response_id', 'courses', 'pstat_class_count', 'stat.comf')
stopifnot(all(required %in% names(background)))
message('Respondents: ', nrow(background))
print(background %>% count(response_id) %>% filter(n > 1))
stopifnot(!anyNA(background$response_id), !anyDuplicated(background$response_id))
# Blank course lists are missing information, not evidence of zero courses.
missing_courses <- is.na(background$courses) | str_trim(background$courses) == ''
print(background %>% summarise(across(all_of(required), ~ sum(is.na(.x)))))
message('Missing or blank course lists: ', sum(missing_courses))

# Validate statistics comfort against the documented metadata categories
metadata <- read.csv('data/survey-metadata.csv', stringsAsFactors = FALSE)
comfort_notes <- metadata$values.notes[metadata$variable.name == 'stat.comf']
# Metadata reports observed categories, not explicit questionnaire bounds.
stopifnot(length(comfort_notes) == 1,
          str_detect(comfort_notes, fixed('observed values are 2, 3, 4, 5')))
stopifnot(is.numeric(background$stat.comf),
          all(is.na(background$stat.comf) | background$stat.comf %in% 2:5))

# Extract distinct upper-division PSTAT courses for each student
# Normalize a copy; the original courses column remains unchanged.
course_lists <- str_split(str_to_upper(background$courses), ',')
ud_courses <- lapply(course_lists, function(labels) {
  labels <- str_squish(labels)
  labels <- labels[!is.na(labels) & str_detect(labels, '^PSTAT\\b')]
  # Match the entire label; keep letter suffixes as distinct course codes.
  parts <- str_match(labels, '^PSTAT\\s*([0-9]+)\\s*([A-Z]?)$')
  if (anyNA(parts[, 1])) stop('Unrecognized PSTAT label: ', paste(labels, collapse = ', '))
  numbers <- as.integer(parts[, 2])
  unique(paste0('PSTAT ', numbers, parts[, 3])[numbers >= 100])
})
background <- background %>%
  mutate(ud_pstat_courses = vapply(ud_courses, paste, character(1), collapse = ', '),
         n_ud_pstat = lengths(ud_courses),
         ud_pstat_courses = if_else(missing_courses, NA_character_, ud_pstat_courses),
         n_ud_pstat = if_else(missing_courses, NA_integer_, n_ud_pstat))
stopifnot(all(is.na(background$n_ud_pstat) | background$n_ud_pstat >= 0))

# Check calculated counts against the source column
discrepancies <- background %>%
  filter(is.na(n_ud_pstat) != is.na(pstat_class_count) |
           (!is.na(n_ud_pstat) & !is.na(pstat_class_count) &
              n_ud_pstat != pstat_class_count)) %>%
  select(response_id, courses, pstat_class_count, n_ud_pstat)
message('Course-count discrepancies: ', nrow(discrepancies))
print(discrepancies, n = Inf, width = Inf)
if (nrow(discrepancies) > 0) stop('Resolve course-count discrepancies before exporting.')
# Check the count directly against the exported course list.
list_counts <- if_else(background$ud_pstat_courses == '', 0L,
                      str_count(background$ud_pstat_courses, ',') + 1L)
stopifnot(all(is.na(background$n_ud_pstat) == is.na(list_counts)),
          all(background$n_ud_pstat == list_counts, na.rm = TRUE))

# Save the cleaned Q1 dataset without removing or imputing observations
analysis <- background %>%
  select(response_id, courses, ud_pstat_courses, n_ud_pstat,
         stat_comfort = stat.comf)
dir.create('data/processed/q1', recursive = TRUE, showWarnings = FALSE)
write.csv(analysis, 'data/processed/q1/q1-analysis.csv', row.names = FALSE, na = '')
message('Saved ', nrow(analysis), ' respondents; original course text retained.')
