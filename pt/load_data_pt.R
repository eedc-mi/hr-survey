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

data_2019 <- read_csv(file.path(data_path, "2019", "pt", "responses_pt_2019.csv")) %>% mutate(date = 2019)
data_2018 <- read_csv(file.path(data_path, "2018", "pt", "responses_pt_2018.csv")) %>% mutate(date = 2018)

questions_2019 <- read_csv(file.path(data_path, "2019", "pt", "questions_pt_2019.csv"))
questions_2018 <- read_csv(file.path(data_path, "2018", "pt", "questions_pt_2018.csv"))

employees_2019 <- read_csv(file.path(data_path, "2019", "pt", "employees_pt_2019.csv")) %>% mutate(date = 2019)
employees_2018 <- read_csv(file.path(data_path, "2018", "pt", "employees_pt_2018.csv")) %>% mutate(date = 2018)

update_names <- function(tbl_df, recode_var, levels) {
  recode_var <- enquo(recode_var)
  
  tbl_df %>%
    mutate(!!recode_var := recode(!!recode_var, !!!levels))
}

division_name_lvls <- list(
  "ECC" = "Edmonton Convention Centre",
  "Expo" = "Edmonton Expo Centre",
  "Shaw Conference Centre" = "Edmonton Convention Centre"
)

dept_name_lvls_ecc <- list(
  "Housekeeping" = "Event Services and Housekeeping",
  "Event Services" = "Event Services and Housekeeping",
  "Maintenance" = "Engineering and Maintenance",
  "Administration" = "Security",
  "Guest Services" = "Guest Experience",
  "Banquets" = "Banquets and Bars",
  "Bars" = "Banquets and Bars"
)

dept_name_lvls_expo <- list(
  "Facility Trades" = "Facility Trades and Building Operations",
  "Building Operations" = "Facility Trades and Building Operations",
  "Bars" = "Bars and Concessions",
  "Concessions" = "Bars and Concessions",
  "Guest Services" = "Guest Experience",
  "Client Services" = "Guest Experience",
  "Event and Building Operations" = "Facility Trades and Building Operations",
  "Event & Building Operations" = "Facility Trades and Building Operations",
  "Facilities & Site Services" = "Facility Trades and Building Operations"
)

employees_2018 <- bind_rows(
  employees_2018 %>% 
    filter(division == "Edmonton Expo Centre") %>%
    update_names(department, dept_name_lvls_expo),
  employees_2018 %>%
    filter(division == "Shaw Conference Centre") %>%
    update_names(department, dept_name_lvls_ecc) %>%
    update_names(division, division_name_lvls)
)

employees_2019 <- bind_rows(
  employees_2019 %>%
    update_names(division, division_name_lvls) %>%
    filter(division == "Edmonton Expo Centre") %>%
    update_names(department, dept_name_lvls_expo),
  employees_2019 %>%
    update_names(division, division_name_lvls) %>%
    filter(division == "Edmonton Convention Centre") %>%
    update_names(department, dept_name_lvls_ecc)
)

count_employees_by <- function(tbl_df, ...) {
  group_vars <- enquos(...)
  
  tbl_df %>% 
    group_by(!!!group_vars, date) %>%
    count() %>%
    rename(count = n) %>%
    ungroup()
}

clean_data <- function(tbl_df) {
  tbl_df <- tbl_df %>% 
    select(
      -starts_with("What should"),
      -starts_with("Do you have"),
      -starts_with("Collector"),
      -starts_with("Start"), 
      -starts_with("End"),
      division = starts_with("What venue"),
      department_ecc = starts_with("What department"),
      department_expo = starts_with("What is your home"),
      id = "Respondent ID") %>%
    select(id, division, department_ecc, department_expo, date, everything())
  
  tbl_df <- tbl_df %>% 
    update_names(division, division_name_lvls) %>%
    update_names(department_ecc, dept_name_lvls_ecc) %>%
    update_names(department_expo, dept_name_lvls_expo)
  
  questions_start_index <- which(grepl("date", names(tbl_df))) + 1
  tbl_df <- tbl_df %>% 
    filter_at(questions_start_index:ncol(.), any_vars(!is.na(.)))
  
  # fix an error where some expo employees picked both 
  # department questions in surveymonkey
  tbl_df <- tbl_df %>%
    mutate(
      department_ecc = case_when(
        (division == "Edmonton Expo Centre" & !is.na(department_ecc)) ~ NA_character_,
        TRUE ~ department_ecc
      )
    )
  
  tbl_df <- tbl_df %>% 
    replace_na(list(department_ecc = "", department_expo = "")) %>% 
    unite("department", department_ecc, department_expo, sep = "")
  
  tbl_df <- tbl_df %>% 
    gather(key = "question", value = "response", -id, -division, -department, -date) %>%
    mutate(response = case_when(
      response == "Strongly Disagree" ~ "Strongly disagree",
      response == "Strongly Agree" ~ "Strongly agree",
      response == "Neither Agree nor Disagree" ~ "Neither agree nor disagree",
      TRUE ~ response
    ))
  
  lvls_lower <- c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")
  tbl_df$response <- factor(tbl_df$response, levels = lvls_lower, ordered = TRUE)
  
  tbl_df %>% 
    bind_rows(tbl_df %>% mutate(department = "All"))
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
  driver_list <- driver_list[
    driver_list != "Leadership" & driver_list != "Change Leadership"
  ]
  
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
      "Safety",
      "Teamwork",
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
  ls <- map(
    employees, 
    function(x) bind_rows(x, mutate(x, department = "All"))
  )
  
  ls <- map(ls, count_employees_by, division, department)
  bind_rows(ls, extra)
}

response_data <- make_response_dataset(
  list(data_2018, data_2019),
  list(questions_2018, questions_2019)
)

employee_data <- make_employee_dataset(
  list(employees_2018, employees_2019)
)
