library(tidyverse)
library(here)
library(scales)

setwd(here())

dataPath <- file.path(
  "V:",
  "Economic Intelligence",
  "Corporate",
  "Human Resources",
  "Employee Engagement Survey Results",
  "Nov 2017"
)

dataNew <- read_csv(file.path(dataPath, "nov2017.csv"))
dataOld <- read_csv(file.path(dataPath, "jan2017.csv"))
qsNov <- read_csv(file.path(dataPath, "questions2017.csv"))
qsJan <- read_csv(file.path(dataPath, "questionsJan2017.csv"))

cleanData <- function(tib, qs, date) {
  tib <- tib %>% 
    select(
      -contains("Please tell us"),
      division = 2,
      id = "Respondent ID"
    )
  
  tib <- tib %>% mutate(
    division = case_when(
      grepl("Corporate", division) ~ "Corporate",
      grepl("SCC", division) ~ "Shaw Conference Centre",
      TRUE ~ division
    )
  )
  
  tib <- tib %>% gather(key = "question", value = "response", -id, -division)
  
  lvls <- c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree")
  tib$response <- factor(tib$response, levels = lvls, ordered = TRUE)
  
  driverList <- unique(
    c(
      unique(qs$driver_1[! is.na(qs$driver_1)]),
      unique(qs$driver_2[! is.na(qs$driver_2)]), 
      unique(qs$driver_3[! is.na(qs$driver_3)])
    )
  )
  
  tib <- tib %>% left_join(qs)
  
  byDriver <- tibble()
  
  for (d in driverList) {
    byDriver <- bind_rows(
      byDriver,
      tib %>%
        filter(driver_1 == d | driver_2 == d | driver_3 == d) %>%
        mutate(driver_all = d)
    )
  }
  
  byDriver %>% mutate(date = date)
}

dataOld <- bind_cols(
  dataOld %>% select("Respondent ID", "Division"),
  dataOld %>% select(-1, -2) %>% mutate_all(
    funs(recode(
      .,
      `5` = "Strongly Agree",          
      `4` = "Agree",
      `3` = "Neither Agree nor Disagree",
      `2` = "Disagree",
      `1` = "Strongly Disagree",
      .default = NA_character_
    ))
  )
)
  
tib <- bind_rows(
  cleanData(dataNew, qsNov, "Nov. 2017"), 
  cleanData(dataOld, qsJan, "Jan. 2017") 
)

toPlot <- tib %>%
  group_by(division, driver_all, date) %>%
  count(response) %>%
  complete(response, fill = list(n = 0)) %>%
  filter(! is.na(response)) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  spread(key = response, value = freq) %>%
  mutate(engagement = `Strongly Agree` + `Agree`) %>% #back-quoting is bad
  select(division, driver_all, date, engagement) %>%
  complete(division, date, driver_all)

ggplot(toPlot %>% filter(division == "Corporate"), aes(x = driver_all, y = engagement, fill = date)) + 
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()

ggplot(toPlot %>% filter(date == "Nov. 2017"), 
       aes(x = division, y = driver_all, fill = engagement)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(engagement, 2) * 100)) +
  scale_fill_gradient2(
    limits = c(0, 1), 
    low = "#d7191c", 
    high = "#1a9641", 
    mid = "#ffffbf", 
    midpoint = mean(toPlot$engagement, na.rm = TRUE))
