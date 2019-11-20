library(tidyverse)
library(here)

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
employees_2019 <- read_csv(file.path(data_path, "2019", "ft", "employees_2019_update.csv")) %>% mutate(date = 2019)
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
  "Tourism" = "Edmonton Tourism",
  "Research & Strategy" = "Research and Strategy"
)

count_employees_by <- function(tbl_df, group_var) {
  group_var <- enquo(group_var)
  
  tbl_df %>% 
    group_by(!!group_var, date) %>%
    count() %>%
    rename(count = n) %>%
    ungroup() %>%
    add_row(!!group_var := "All", date = unique(.$date), count = sum(.$count))
}

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
  
  tbl_df %>% 
    bind_rows(tbl_df %>% mutate(division = "All"))
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
  
  response_data <- tbl_df %>%
    mutate(driver_all = "All Drivers")
  
  for (d in driver_list) {
    response_data <- bind_rows(
      response_data,
      tbl_df %>%
        filter(driver_1 == d | driver_2 == d | driver_3 == d) %>%
        mutate(driver_all = d)
    )
  }
  
  response_data$driver_all <- factor(
    response_data$driver_all,
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
  
  response_data
}

make_response_dataset <- function(responses, questions) {
  ls <- map(responses, clean_data)
  bind_rows(map2(ls, questions, driver_transform)) 
}

make_employee_dataset <- function(employees, extra = list()) {
  ls <- map(employees, count_employees_by, group_var = division)
  tbl_df <- bind_rows(ls, extra) %>%
    update_names(division, division_name_lvls)
}

calc_engagement_by <- function(tbl_df, ..., exclude = TRUE) {
  group_vars <- quos(...)
  
  if (exclude) {
    tbl_df <- filter(tbl_df, include_in_engagement_score)
  }
  
  tbl_df %>%
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