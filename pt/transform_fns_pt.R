library(scales)

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

transform_summary_table <- function(tbl_df) {
  tbl_df %>%
    filter(driver_all == "All Drivers") %>%
    calc_engagement_by(department, date) %>%
    select(department, date, engagement) %>%
    mutate(engagement = percent(engagement, 1)) %>%
    spread(date, engagement)
}

transform_participation_table <- function(response_df, employee_df) {
  response_df %>%
    filter(driver_all == "All Drivers") %>%
    select(id, department, date, question, response) %>%
    spread(key = question, value = response) %>%
    group_by(department, date) %>%
    count() %>%
    ungroup() %>%
    left_join(employee_df, by = c("department", "date")) %>%
    mutate(participation = percent(n / count, 1)) %>%
    select(-count, -n, -division) %>%
    spread(date, participation)
}

transform_yoy_plot <- function(tbl_df) {
  tbl_df %>%
    filter(date %in% c(2018, 2019)) %>%
    calc_engagement_by(department, driver_all, date) %>%
    select(department, driver_all, date, engagement) %>%
    spread(date, engagement) %>%
    mutate(is_negative = factor(`2019`- `2018` < 0)) %>%
    gather(date, engagement, -department, -driver_all, -is_negative) %>%
    complete(department, date, driver_all) %>%
    mutate(engagement = engagement * 100)
}

transform_detailed_heatmap <- function(tbl_df) {
  tbl_df %>%
    filter(date == 2019) %>%
    calc_engagement_by(department, driver_all, question) %>%
    select(department, driver_all, question, engagement) %>%
    mutate(engagement = engagement * 100)
}

transform_yoy_facet <- function(tbl_df) {
  tbl_df %>%
    calc_engagement_by(date, department, driver_all, question_id) %>%
    select(date, department, driver_all, question_id, engagement) %>%
    spread(date, engagement) %>%
    mutate(
      diff = `2019`- `2018`,
      change = case_when(
        diff > 0 ~ "pos",
        diff == 0 ~ "zero",
        diff < 0 ~ "neg"
      )) %>%
    select(-diff) %>%
    gather(date, engagement, -department, -driver_all, -change, -question_id) %>%
    mutate(engagement = engagement * 100, change = replace_na(change, "zero")) %>%
    left_join(questions_2019, by = "question_id")
}

transform_facet_plot <- function(tbl_df) {
  tbl_df %>%
    group_by(department, driver_all, date) %>%
    count(response) %>%
    complete(response, fill = list(n = 0)) %>%
    filter(! is.na(response)) %>%
    mutate(freq = n / sum(n)) %>%
    mutate(freq = ifelse(response == "Disagree", -freq, freq)) %>%
    mutate(freq = ifelse(response == "Strongly disagree", -freq, freq)) %>%
    filter(! response == "Neither agree nor disagree") %>%
    ungroup() %>%
    select(-n) %>%
    mutate(response = fct_relevel(
      response, c("Strongly disagree", "Disagree", "Strongly agree", "Agree")))
}

transform_category_facet_plot <- function(tbl_df) {
  tbl_df %>%
    filter(include_in_engagement_score, driver_all == "All Drivers") %>%
    group_by(department, category, date) %>%
    count(response) %>%
    complete(response, fill = list(n = 0)) %>%
    filter(! is.na(response)) %>%
    mutate(freq = n / sum(n)) %>%
    mutate(freq = ifelse(response == "Disagree", -freq, freq)) %>%
    mutate(freq = ifelse(response == "Strongly disagree", -freq, freq)) %>%
    filter(! response == "Neither agree nor disagree") %>%
    ungroup() %>%
    select(-n) %>%
    mutate(response = fct_relevel(
      response, c("Strongly disagree", "Disagree", "Strongly agree", "Agree")))
}

transform_category_yoy_facet <- function(tbl_df) {
  tbl_df %>%
    filter(driver_all == "All Drivers") %>%
    calc_engagement_by(date, department, category) %>%
    select(date, department, category, engagement) %>%
    spread(date, engagement) %>%
    mutate(
      diff = `2019`- `2018`,
      change = case_when(
        diff > 0 ~ "pos",
        diff == 0 ~ "zero",
        diff < 0 ~ "neg"
      )) %>%
    select(-diff) %>%
    gather(date, engagement, -department, -category, -change) %>%
    mutate(engagement = engagement * 100, change = replace_na(change, "zero"))
}

transform_category_heatmap <- function(tbl_df) {
  tbl_df %>%
    filter(driver_all == "All Drivers") %>%
    calc_engagement_by(date, department, category) %>%
    select(date, department, category, engagement) %>%
    mutate(engagement = engagement * 100)
}