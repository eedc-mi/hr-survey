library(scales)

source("load_data_ft.R")

summary_table <- response_data %>%
  filter(driver_all == "All Drivers") %>%
  calc_engagement_by(division, date) %>%
  select(division, date, engagement) %>%
  mutate(engagement = percent(engagement, 1)) %>%
  spread(date, engagement)

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

to_yoy_plot <- response_data %>%
  filter(date %in% c(2018, 2019)) %>%
  calc_engagement_by(division, driver_all, date) %>%
  select(division, driver_all, date, engagement) %>%
  spread(date, engagement) %>%
  mutate(is_negative = factor(`2019`- `2018` < 0)) %>%
  gather(date, engagement, -division, -driver_all, -is_negative) %>%
  complete(division, date, driver_all) %>%
  mutate(engagement = engagement * 100)

to_detailed_heatmap <- response_data %>%
  filter(date == 2019) %>%
  calc_engagement_by(division, driver_all, question) %>%
  select(division, driver_all, question, engagement) %>%
  mutate(engagement = engagement * 100)

to_yoy_facet <- response_data %>%
  calc_engagement_by(date, division, driver_all, question_id) %>%
  select(date, division, driver_all, question_id, engagement) %>%
  spread(date, engagement) %>%
  mutate(
    diff = `2019`- `2018`,
    change = case_when(
      diff > 0 ~ "pos",
      diff == 0 ~ "zero",
      diff < 0 ~ "neg"
    )) %>%
  select(-diff) %>%
  gather(date, engagement, -division, -driver_all, -change, -question_id) %>%
  mutate(engagement = engagement * 100, change = replace_na(change, "zero")) %>%
  left_join(questions_2019, by = "question_id")

to_facet_plot <- response_data %>%
  group_by(division, driver_all, date) %>%
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

to_category_facet_plot <- response_data %>%
  filter(include_in_engagement_score) %>%
  group_by(division, category, date) %>%
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

to_pulse_bar_plot <- response_data %>%
  filter(category == "2019 Pulse Check Question") %>%
  calc_engagement_by(division, date, question, exclude = FALSE) %>%
  select(division, date, question, engagement)

to_pulse_facet_plot <- response_data %>%
  filter(category == "2019 Pulse Check Question") %>%
  group_by(division, question, date) %>%
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

to_category_yoy_facet <- response_data %>%
  calc_engagement_by(date, division, category) %>%
  select(date, division, category, engagement) %>%
  spread(date, engagement) %>%
  mutate(
    diff = `2019`- `2018`,
    change = case_when(
      diff > 0 ~ "pos",
      diff == 0 ~ "zero",
      diff < 0 ~ "neg"
    )) %>%
  select(-diff) %>%
  gather(date, engagement, -division, -category, -change) %>%
  mutate(engagement = engagement * 100, change = replace_na(change, "zero"))

to_category_heatmap <- response_data %>%
  calc_engagement_by(date, division, category) %>%
  select(date, division, category, engagement) %>%
  mutate(engagement = engagement * 100)