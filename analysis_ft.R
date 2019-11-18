#'---
#'title: EEDC Employee Engagement Survey
#'author: 2018 Results
#'date:
#'output: 
#'  beamer_presentation:
#'    keep_tex: true
#'---
#'
#'### 2018 Employee Engagement Survey
#'- 34 "quantitative" questions, categorized into 9 engagement drivers and two focus areas
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
#'    - Percentage of respondents who answered at least one survey question
#'    - Employee counts are current as of the survey launch date
#'    - This year we were much more rigourous with the survey distribution list. Take 2017 participation rates with a grain of salt.

#+ code, include = FALSE
library(tidyverse)
library(here)
library(scales)
library(stringr)
library(rmarkdown)

setwd(here())

data_path <- file.path(
  "C:",
  "Users",
  "PReid",
  "Edmonton Economic Development Corporation",
  "Market Intelligence - Project Files",
  "Corporate",
  "Human Resources",
  "Employee Engagement Survey Results",
  "data"
)

data_2019 <- read_csv(file.path(data_path, "2019", "ft", "responses_2019.csv")) %>% mutate(date = 2019)
data_2018 <- read_csv(file.path(data_path, "2018", "ft", "responses_2018.csv")) %>% mutate(date = 2018)
data_2017 <- read_csv(file.path(data_path, "nov_2017", "responses_nov_2017.csv")) %>% mutate(date = 2017)
questions_2019 <- read_csv(file.path(data_path, "2019", "ft", "questions_2019.csv"))
questions_2018 <- read_csv(file.path(data_path, "2018", "ft", "questions_2018.csv"))
questions_2017 <- read_csv(file.path(data_path, "nov_2017", "questions_nov_2017.csv"))
employees_2019 <- read_csv(file.path(data_path, "2019", "ft", "employees_2019.csv")) %>% mutate(date = 2019)
employees_2018 <- read_csv(file.path(data_path, "2018", "ft", "employees_2018.csv")) %>% mutate(date = 2018)
employee_count_2017 <- read_csv(file.path(data_path, "nov_2017", "employees_nov_2017.csv")) %>% mutate(date = 2017)

update_names <- function(tbl_df, recode_var, levels) {
  recode_var <- enquo(recode_var)
  
  tbl_df %>%
    mutate(!!recode_var := recode(!!recode_var, !!!levels))
}

division_name_lvls <- list(
  "Corporate (includes Corporate Services and Executive Office)" = "Corporate",
  "Corporate (includes Corporate Services and Corporate Relations)" = "Corporate",
  "SCC" = "Edmonton Convention Centre",
  "Shaw Conference Centre" = "Edmonton Convention Centre",
  "Expo" = "Edmonton Expo Centre",
  "ECC" = "Edmonton Convention Centre",
  "Edmonton EXPO Centre" = "Edmonton Expo Centre",
  "Trade and Investment" = "Research and Strategy",
  "Enterprise Edmonton" = "Research and Strategy",
  "Urban Economy" = "Innovate Edmonton",
  "Tourism" = "Edmonton Tourism"
)

# update_division_names <- function(tbl_df) {
#   tbl_df %>% mutate(
#     division = case_when(
#       grepl("Corporate", division) ~ "Corporate",
#       division == "SCC" ~ "Shaw Conference Centre",
#       division == "Expo" ~ "Edmonton Expo Centre",
#       division == "Trade and Investment" ~ "Enterprise Edmonton",
#       division == "Urban Economy" ~ "Innovate Edmonton",
#       division == "Tourism" ~ "Edmonton Tourism",
#       TRUE ~ division
#     )
#   )  
# }

count_employees_by <- function(tbl_df, group_var) {
  group_var <- enquo(group_var)
  
  tbl_df %>% 
    group_by(!!group_var, date) %>%
    count() %>%
    rename(count = n) %>%
    ungroup() %>%
    add_row(!!group_var := "Overall", date = unique(.$date), count = sum(.$count))
}

# employee_count_new <- count_employees(employees_new)
# employee_count_old <- update_division_names(employee_count_old) 

clean_data <- function(tbl_df) {
  tbl_df <- tbl_df %>% 
    select(
      -starts_with("Please tell us"),
      -starts_with("Collector"),
      -starts_with("Start"), 
      -starts_with("End"),
      division = starts_with("Please identify"),
      id = "Respondent ID") %>%
    select(id, division, date, everything()) %>%
    select(1:(ncol(.) - 4))
  
  tbl_df <- tbl_df %>% update_names(division, division_name_lvls)
  
  date_index <- which(grepl("date", names(tbl_df)))
  tbl_df <- tbl_df %>% drop_na(date_index + 1)
  
  lvls_upper <- c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree")
  lvls_lower <- c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")

  tbl_df <- tbl_df %>% 
    gather(key = "question", value = "response", -id, -division, -date) %>%
    mutate(response = case_when(
      response == "Strongly Disagree" ~ "Strongly disagree",
      response == "Strongly Agree" ~ "Strongly agree",
      response == "Neither Agree nor Disagree" ~ "Neither agree nor disagree",
      TRUE ~ response
    ))
  
  tbl_df$response <- factor(tbl_df$response, levels = lvls_lower, ordered = TRUE)
  
  tbl_df %>% bind_rows(tbl_df %>% mutate(division = "Overall"))
}

driver_transform <- function(tbl_df, questions) {
  driver_list <- unique(
    c(
      unique(questions$driver_1[! is.na(questions$driver_1)]),
      unique(questions$driver_2[! is.na(questions$driver_2)]), 
      unique(questions$driver_3[! is.na(questions$driver_3)])
    )
  )
  
  # Leadership removed at HR request
  driver_list <- driver_list[driver_list != "Leadership"]
  
  tbl_df <- tbl_df %>% left_join(questions)
  
  by_driver <- tbl_df %>%
    mutate(driver_all = "All Drivers")
  
  for (d in driver_list) {
    by_driver <- bind_rows(
      by_driver,
      tbl_df %>%
        filter(driver_1 == d | driver_2 == d | driver_3 == d) %>%
        mutate(driver_all = d)
    )
  }
  
  by_driver$driver_all <- factor(
    by_driver$driver_all,
    levels = rev(c(
      "All Drivers",
      "Affinity",
      "Communication",
      "Compensation",
      "Development",
      "Empowerment",
      "Performance",
      "Recognition",
      "Relations",
      "Teamwork",
      "Change Leadership",
      "Direct Manager Support"
    )),
    ordered = TRUE
  )
  
  by_driver
}

# Old code for dealing with Jan. 2017 survey
# data_old <- bind_cols(
#   data_old %>% select("Respondent ID", "Division"),
#   data_old %>% select(-1, -2) %>% mutate_all(
#     funs(recode(
#       .,
#       `5` = "Strongly Agree",          
#       `4` = "Agree",
#       `3` = "Neither Agree nor Disagree",
#       `2` = "Disagree",
#       `1` = "Strongly Disagree",
#       .default = NA_character_
#     ))
#   )
# )

# clean_data_new <- clean_data(data_new, 2018)
# clean_data_old <- clean_data(data_old, 2017)
# 
# clean_data_all <- bind_rows(
#   clean_data_new,
#   clean_data_old
# )
# 
# clean_data_all <- bind_rows(
#   clean_data_all, 
#   clean_data_all %>% mutate(division = "All Divisions")
# )
#   
# by_driver <- bind_rows(
#   to_by_driver(clean_data_new, questions_new),
#   to_by_driver(clean_data_old, questions_old)
# )
# 
# by_driver <- bind_rows(
#   by_driver,
#   by_driver %>% mutate(division = "All Divisions")
# )

# by_driver$date <- factor(
#   by_driver$date, c(2017, 2018), ordered = TRUE
# )

# employee_count_all <- bind_rows(
#   employee_count_new %>% mutate(date = 2018),
#   employee_count_old %>% mutate(date = 2017)
# )

make_response_dataset <- function(responses, questions) {
  ls <- map(responses, clean_data)
  bind_rows(map2(ls, questions, driver_transform)) 
}

make_employee_dataset <- function(employees, extra = list()) {
  ls <- map(employees, count_employees_by, group_var = division)
  tbl_df <- bind_rows(ls, extra) %>%
    update_names(division, division_name_lvls)
}

calc_engagement_by <- function(tbl_df, ...) {
  group_vars <- quos(...)
  
  tbl_df %>%
    filter(include_in_engagement_score) %>%
    group_by(!!!group_vars) %>%
    count(response) %>%
    complete(response, fill = list(n = 0)) %>%
    filter(! is.na(response)) %>%
    mutate(freq = n / sum(n)) %>%
    ungroup() %>%
    select(-n) %>%
    spread(key = response, value = freq) %>%
    mutate(engagement = `Strongly agree` + `Agree`) 
}

response_data <- make_response_dataset(
  list(data_2017, data_2018, data_2019),
  list(questions_2017, questions_2018, questions_2019)
)

employee_data <- make_employee_dataset(
  list(employees_2018, employees_2019), 
  extra = employee_count_2017
)

summary_table <- response_data %>%
  filter(driver_all == "All Drivers") %>%
  calc_engagement_by(division, date) %>%
  select(division, date, engagement) %>%
  mutate(engagement = percent(engagement, 1))

participation_table <- response_data %>%
  filter(driver_all == "All Drivers") %>%
  select(id, division, date, question, response) %>%
  spread(key = question, value = response) %>%
  group_by(division, date) %>%
  count() %>%
  ungroup() %>%
  left_join(employee_data, by = c("division", "date")) %>%
  mutate(participation = percent(n / count, 1)) %>%
  select(-count, -n) %>%
  spread(date, participation)

# to_yoy_plot <- by_driver %>%
#   calc_engagement_by(division, driver_all, date) %>%
#   select(division, driver_all, date, engagement) %>%
#   complete(division, date, driver_all) %>%
#   mutate(engagement = engagement * 100)

to_yoy_plot <- by_driver %>%
  calc_engagement_by(division, driver_all, date) %>%
  select(division, driver_all, date, engagement) %>%
  spread(date, engagement) %>%
  mutate(is_negative = factor(`2018`- `2017` < 0)) %>%
  gather(date, engagement, -division, -driver_all, -is_negative) %>%
  complete(division, date, driver_all) %>%
  mutate(engagement = engagement * 100)

to_detailed_heatmap <- by_driver %>%
  filter(date == 2018) %>%
  calc_engagement_by(division, driver_all, question) %>%
  select(division, driver_all, question, engagement) %>%
  mutate(engagement = engagement * 100)

to_yoy_facet <- by_driver %>%
  calc_engagement_by(date, division, driver_all, question_id) %>%
  select(date, division, driver_all, question_id, engagement) %>%
  spread(date, engagement) %>%
  mutate(
    diff = `2018`- `2017`,
    change = case_when(
      diff > 0 ~ "pos",
      diff == 0 ~ "zero",
      diff < 0 ~ "neg"
    )) %>%
  select(-diff) %>%
  gather(date, engagement, -division, -driver_all, -change, -question_id) %>%
  mutate(engagement = engagement * 100, change = replace_na(change, "zero")) %>%
  left_join(questions_new, by = "question_id")

to_facet_plot <- by_driver %>%
  group_by(division, driver_all, date) %>%
  count(response) %>%
  complete(response, fill = list(n = 0)) %>%
  filter(! is.na(response)) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(freq = ifelse(response == "Disagree", -freq, freq)) %>%
  mutate(freq = ifelse(response == "Strongly disagree", -freq, freq)) %>%
  filter(! response == "Neither agree nor disagree") %>%
  ungroup() %>%
  select(-n)
lvls <- c("Strongly disagree", "Disagree", "Strongly agree", "Agree")
to_facet_plot$response <- factor(to_facet_plot$response, levels = lvls)

colour_lowest <- "#e74a4e"
colour_low <- "#df7081"
colour_high <- "#5e94d0"
colour_highest <- "#7caadc"
  
#c("#e74a4e", "#df7081", "#5e94d0", "#7caadc")

make_yoy_plot <- function(tbl_df, div) {
  ggplot(
    tbl_df %>% filter(division == div), 
    aes(x = driver_all, y = engagement, fill = date, group = driver_all)) +
    geom_point(shape = 21, size = 5, colour = "black") +
    geom_line(aes(colour = is_negative), arrow = arrow(length=unit(0.30,"cm"), type = "closed"), show.legend = FALSE) +
    scale_fill_manual(values = c("white", "darkgrey")) +
    scale_colour_manual(values = c(colour_highest, colour_lowest), drop = FALSE) +
    ylim(c(25, 100)) +
    labs(
      title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
      subtitle = div) +
    coord_flip() +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line = element_line(color = "black"),
      legend.title = element_blank(),
      legend.position = "top",
      axis.title = element_blank())
}

make_heatmap <- ggplot(
  to_yoy_plot %>% filter(date == 2018) %>%
    mutate(engagement = round(engagement)) %>%
    mutate(bin = cut(engagement, breaks = c(0, 69, 80, 100))), 
  aes(x = division, y = driver_all, fill = bin)) +
  geom_tile(color = "black") +
  geom_text(aes(label = engagement)) +
  scale_fill_manual(
    #values = c("#d7191c", "#ffffbf", "#1a9641"),
    values = c(colour_lowest, "white", colour_highest),
    labels = c("0 - 69", "70 - 80", "81 - 100")) + 
  labs(
    fill = "Engagement\nScore",
    title = "Percentage of \'Agree\' responses or higher") +
  scale_x_discrete(position = "top", labels = function(x) str_wrap(x, width = 10)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank())

make_detail_heatmap <- function(tbl_df, driver) {
  ggplot(
    tbl_df %>%
      filter(driver_all == driver) %>%
      mutate(engagement = round(engagement)) %>%
      mutate(bin = cut(engagement, breaks = c(0, 69, 80, 100))),
    aes(x = division, y = question, fill = bin)) +
    geom_tile(color = "black") +
    geom_text(aes(label = engagement)) +
    scale_fill_manual(
      values = c(colour_lowest, "white", colour_highest),
      labels = c("0 - 69", "70 - 80", "81 - 100")) + 
    labs(
      fill = "Engagement\nScore",
      title = driver,
      subtitle = "Percentage of \'Agree\' responses or higher") +
    scale_x_discrete(position = "top", labels = function(x) str_wrap(x, width = 10)) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 35)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank())
}

make_facet_plot <- function(tbl_df, div) {
  palette <- c(colour_lowest, colour_low, colour_high, colour_highest)
  
  ggplot(tbl_df %>% filter(division == div) %>% mutate(freq = round(freq, 4)),
         aes(x = driver_all, y = freq)) + 
    geom_bar(width = 0.75, aes(fill = response), stat = "identity")+
    scale_fill_manual(
      values = palette,
      breaks = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))  +
    labs(
      title = "Frequency of Response Type by Driver", 
      subtitle = div) + 
    facet_wrap(~ date) +
    coord_flip() +
    scale_y_continuous(limits = c(-0.5, 1)) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.spacing = unit(1, "lines"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line = element_line(color = "black"),
      legend.title = element_blank(),
      legend.position = "top",
      axis.title = element_blank())
}

make_yoy_facet <- function(tbl_df, driver) {
  format_label <- function(width) {
    function(strings) {
      str_wrap(strings, width = width)
    }
  }
  
  ggplot(tbl_df %>% filter(driver_all == driver) %>%
           mutate(engagement = round(engagement)), 
         aes(x = factor(date), y = engagement, 
             group = division, label = engagement, colour = change)) + 
    facet_grid(
      question ~ division, 
      switch = "y", 
      labeller = labeller(
        question = format_label(35),
        division = format_label(10))) + 
    geom_point(show.legend = FALSE) +
    geom_text(nudge_y = 3, hjust = "center", vjust = "bottom", size = 3, show.legend = FALSE) +
    geom_line(show.legend = FALSE) +
    scale_colour_manual(values = c(colour_lowest, colour_highest, "darkgrey")) +
    scale_y_continuous(expand = expand_scale(mult = c(0.4, 0.4))) +
    labs(
      title = driver,
      subtitle = "Percentage of \'Agree\' responses or higher") +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      strip.text.y = element_text(angle = 180, colour = "grey30"),
      strip.text.x = element_text(colour = "grey30"),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      strip.background = element_blank())
}

#'### Engagement Score Results
#'- Included "All Divisions (excl. Expo)" for comparison to last year, not much difference so will not
#'include in following analysis
#+ echo=FALSE
knitr::kable(
  summary_table %>% spread(date, engagement),
  col.names = c("Division", "2017", "2018"),
  caption = "Engagement Score (% \'Agree\' Responses or Higher)",
  align = c('l', 'r', 'r'),
  padding = 12)

#'### Participation Results
#'- Aware of anomaly in Tourism participation rate, unfortunately not enough information to find the cause
#+ echo=FALSE
knitr::kable(
  participation_table,
  col.names = c("Division", "2017", "2018"),
  caption = "Participation Rate (Answered Past First Survey Page)",
  align = c('l', 'r', 'r'),
  padding = 12)

#'### Engagement Score by Driver and Division
#+ echo = FALSE, warning = FALSE, message = FALSE
make_heatmap

#+ echo=FALSE, message=FALSE, warning=FALSE, results='asis'
for (d in unique(to_yoy_plot$division)) {
  cat("\n###", " Year to Year Comparison - ", d, "  \n")
  print(make_yoy_plot(to_yoy_plot, d))
  cat("  \n")
}

#+ echo=FALSE, message=FALSE, warning=FALSE, results='asis'
for (d in unique(to_facet_plot$division)) {
  cat("\n###", " Response Summary -  ", d, "  \n")
  print(make_facet_plot(to_facet_plot, d))
  cat("  \n")
}


#+ echo=FALSE, message=FALSE, warning=FALSE, results='asis'
for (d in rev(unique(to_detailed_heatmap$driver_all))) {
  if (! d == "All Drivers") {
    cat("\n###", " Engagement by Question and Division",  "\n")
    print(make_detail_heatmap(to_detailed_heatmap, d))
    cat("  \n")  
  }
}

#+ echo=FALSE, message=FALSE, warning=FALSE, results='asis'
for (d in rev(unique(to_yoy_facet$driver_all))) {
  if (! d == "All Drivers") {
    cat("\n###", " Year to Year Comparison - Engagement by Question",  "\n")
    print(make_yoy_facet(to_yoy_facet, d))
    cat("  \n")  
  }
}