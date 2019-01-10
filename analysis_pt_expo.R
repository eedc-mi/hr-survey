#'---
#'title: EEDC Venue Operations Employee Engagement Survey
#'author: 2018 Results - Edmonton Expo Centre
#'date:
#'output: 
#'  beamer_presentation
#'---
#'
#'### 2018 Employee Engagement Survey
#'- 18 "quantitative" questions, categorized into 11 engagement drivers
#'- Some questions associated with more than one driver
#'- 11 engagement drivers:
#'    1. Affinity: I like the place and people
#'    2. Communication: I get information when I need it and  am able to share my thoughts, opinions and ideas
#'    3. Compensation: Total rewards including pay, benefits, retirement plan, perks
#'    4. Development: Coaching, training and other developmental opportunities aimed at improving personal and professional career growth opportunities
#'    5. Empowerment: I feel supported to make decisions
#'    6. Performance: Execution and accomplishment of work
#'        
#'### 2018 Employee Engagement Survey
#'- 11 engagement drivers (cont.):
#'    7. Recognition: My efforts and accomplishments are acknowledged
#'    8. Relations: Relationship between the employer and employee, based on foundation of trust and respect
#'    9. Teamwork: Cooperative and coordinated efforts of a group working together to achieve common objectives
#'    10. Change Leadership: Influence and enthuse others through personal advocacy, vision and drive, and to access resources to build a solid platform for change
#'    11. Direct Manager Support: Empowering others to act, foster collaboration, and build trust
#'    
#'### Analysis
#'- Engagement Score:
#'    - Percentage of responses that are **Agree** or higher
#'    - All questions weighted equally
#'- Participation Rate:
#'    - Percentage of respondents who answered at least one survey question past venue/department
#'    - Employee counts are current as of the survey launch date

#+ code, include = FALSE
library(tidyverse)
library(here)
library(scales)
library(stringr)
library(rmarkdown)

source("shared_fns.R")

setwd(here())

data_path <- file.path(
  "K:",
  "Corporate",
  "Human Resources",
  "Employee Engagement Survey Results",
  "data"
)

data_new <- read_csv(file.path(data_path, "2018", "pt", "responses_pt_2018.csv"))
questions_new <- read_csv(file.path(data_path, "2018", "pt", "questions_pt_2018.csv"))
employees_new <- read_csv(file.path(data_path, "2018", "pt", "employees_pt_2018.csv"))

dept_name_lvls <- list(
  Kitchen = "Kitchen and Stewarding",
  Stewarding = "Kitchen and Stewarding",
  "Facility Trades" = "Security, Trades, and Ops",
  "Building Operations" = "Security, Trades, and Ops",
  Security = "Security, Trades, and Ops",
  Bars = "Bars and Concessions",
  Concessions = "Bars and Concessions",
  "Event & Building Operations" = "Building Operations",
  "Facilities & Site Services" = "Security, Trades, and Ops",
  "Guest Services" = "Guest Experience"
)

clean_data_new <- data_new %>%
  clean_data_pt(venue_name_expo, 2018) %>%
  update_names(department, dept_name_lvls)

by_driver <- clean_data_new %>%
  to_by_driver_pt(questions_new)

employee_count_new <- employees_new %>%
  update_names(division, venue_name_lvls) %>%
  filter(division == venue_name_expo) %>%
  update_names(department, dept_name_lvls) %>%
  count_employees(department, 2018)

summary_table <- clean_data_new %>%
  make_summary_table_by(department)

participation_table <- clean_data_new %>%
  make_participation_table_by(employee_count_new, department, date)

to_summary_heatmap <- summary_heatmap_transform(by_driver, department, 2018)
to_yoy_plot <- yoy_plot_transform(by_driver, department)
to_facet_plot <- bar_facet_transform(by_driver, department)
to_detailed_heatmap <- detail_heatmap_transform(by_driver, department, 2018)

#'### Engagement Score Results
#+ echo=FALSE
knitr::kable(
  summary_table,
  col.names = c("Department", "2018"),
  caption = "Engagement Score (% \'Agree\' Responses or Higher)",
  align = c('l', 'r'),
  padding = 12)

#'### Participation Results
#+ echo=FALSE
knitr::kable(
  participation_table %>% select(-date, -count),
  col.names = c("Department", "Responses", "Rate"),
  caption = "Participation Rate (Answered Past First Survey Page)",
  align = c('l', 'r', 'r'),
  padding = 12)

#'### Engagement Score by Driver and Division
#+ echo = FALSE, warning = FALSE, message = FALSE
make_summary_heatmap(to_summary_heatmap, department, 2018)

#+ echo=FALSE, message=FALSE, warning=FALSE, results='asis'
for (d in unique(to_yoy_plot$department)) {
  cat("\n###", " Engagement by Driver - ", d, "  \n")
  print(make_bar_plot(to_yoy_plot, department, d))
  cat("  \n")
}

#+ echo=FALSE, message=FALSE, warning=FALSE, results='asis'
for (d in unique(to_facet_plot$department)) {
  cat("\n###", " Response Summary -  ", d, "  \n")
  print(make_bar_facet(to_facet_plot, department, d))
  cat("  \n")
}

#+ echo=FALSE, message=FALSE, warning=FALSE, results='asis'
for (dr in rev(unique(to_detailed_heatmap$driver_all))) {
  if (! dr == "All Drivers") {
    cat("\n###", " Engagement by Question and Division",  "\n")
    print(make_detail_heatmap(to_detailed_heatmap, department, dr))
    cat("  \n")  
  }
}
