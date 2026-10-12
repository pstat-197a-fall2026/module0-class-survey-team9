# Question 1 visualization plan

Our research question is: “Is the number of upper-division PSTAT courses a student has taken associated with their self-reported comfort level in statistics?”

These four figures will help us explore that question. This file is a plan for the teammate handling visualizations; it contains no plotting code. All data paths are relative to the project root.

## Figure 1 — UD PSTAT Course Count Distribution

- **Plot type:** Bar chart.
- **Data source:** `results/q1/tables/course_frequency.csv`.
- **Axes:** UD PSTAT course count (`n_ud_pstat`) on the x-axis and number of students (`n_students`) on the y-axis.
- **Why use it:** Before comparing comfort across course counts, we need to know where most of our students fall. This chart shows the sample's course-count distribution and makes groups with very few students easy to spot.
- **What to look for:** Check which course counts are most common and which have fewer than five students. Keep those small groups in mind when interpreting the relationship in the next figures.

## Figure 2 — UD PSTAT Courses vs. Statistics Comfort

- **Plot type:** Bubble plot.
- **Data source:** `data/processed/q1/q1-analysis.csv`.
- **Axes and size:** `n_ud_pstat` on the x-axis, `stat_comfort` on the y-axis, and bubble area proportional to the number of students at each course-count and comfort-rating combination. Include a size legend.
- **Why use it:** This directly compares the two variables in our research question. Both variables take discrete values, so students with the same answers would overlap in a regular scatterplot. Counting each combination and using bubble sizes lets us see how many students share those answers.
- **What to look for:** See whether larger bubbles tend to appear at higher comfort ratings as course counts increase, or whether similar ratings appear across course counts. Also look for exceptions. Sparse bubbles at the ends of the course-count range give us less information about a pattern.

## Figure 3 — Comfort Ratings by Course Count

- **Plot type:** 100% stacked bar chart.
- **Data source:** `data/processed/q1/q1-analysis.csv`.
- **Axes and fill:** `n_ud_pstat` on the x-axis, percentage of students within each course-count group on the y-axis, and `stat_comfort` as the fill. Each bar should total 100%. Keep rating colors in ascending order and consistent across figures.
- **Why use it:** Comparing percentages lets us compare comfort distributions even when course-count groups have different numbers of students. We can see how much of each group reports lower or higher comfort instead of looking only at an average.
- **What to look for:** Check whether the share reporting comfort 4 or 5 tends to increase with course count, and whether the full distribution shifts. Display each group's sample size above its bar: a 100% share based on one student should not carry the same weight as a share based on many students.

## Figure 4 — Mean Comfort by Course Count

- **Plot type:** Point plot.
- **Data source:** `results/q1/tables/group_summary.csv`.
- **Axes and labels:** `n_ud_pstat` on the x-axis and `mean_comfort` on the y-axis. Label each point with the group's `n_students`.
- **Why use it:** This gives a simple view of whether average comfort tends to change as students take more UD PSTAT courses. Use Figure 3 alongside it, since groups with similar averages can still have different comfort distributions.
- **What to look for:** Look for an upward, downward, or fairly flat pattern, including any uneven changes. Avoid overinterpreting groups with fewer than five students; one student's rating can strongly affect their mean. Comfort is an ordinal rating, so the mean also assumes equal spacing between rating categories.

Together, these figures describe patterns in this sample. They do not establish statistical significance or show that taking more courses causes greater comfort.
