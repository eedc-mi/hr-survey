library(tidyverse)
library(here)
library(scales)
library(officer)
library(flextable)
library(rvg)
library(stringr)

setwd(here())

dataPath <- file.path(
  "V:",
  "Economic Intelligence",
  "Corporate",
  "Human Resources",
  "Employee Engagement Survey Results",
  "Nov 2017",
  "data"
)

dataNew <- read_csv(file.path(dataPath, "nov2017.csv"))
dataOld <- read_csv(file.path(dataPath, "jan2017.csv"))
qsNov <- read_csv(file.path(dataPath, "questions2017.csv"))
qsJan <- read_csv(file.path(dataPath, "questionsJan2017.csv"))
empCountNov <- read_csv(file.path(dataPath, "empCountNov212017.csv"))

cleanData <- function(tib, date) {
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
  
  tib <- tib %>% drop_na(3)

  tib <- tib %>% gather(key = "question", value = "response", -id, -division)

  lvls <- c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree", "Agree", "Strongly Agree")
  tib$response <- factor(tib$response, levels = lvls, ordered = TRUE)

  tib %>% mutate(date = date)
}

toByDriver <- function(tib, qs) {
  driverList <- unique(
    c(
      unique(qs$driver_1[! is.na(qs$driver_1)]),
      unique(qs$driver_2[! is.na(qs$driver_2)]), 
      unique(qs$driver_3[! is.na(qs$driver_3)])
    )
  )
  
  # Leadership removed at HR request
  driverList <- driverList[driverList != "Leadership"]
  
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
  
  byDriver
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

cleanDataNew <- cleanData(dataNew, "Nov. 2017")
cleanDataOld <- cleanData(dataOld, "Jan. 2017")

cleanDataAll <- bind_rows(
  cleanDataNew,
  cleanDataOld
)

cleanDataAll <- bind_rows(
  cleanDataAll, 
  cleanDataAll %>% mutate(division = "All Divisions")
)
  
byDriver <- bind_rows(
  toByDriver(cleanDataNew, qsNov),
  toByDriver(cleanDataOld, qsJan)
)

byDriver <- bind_rows(
  byDriver,
  byDriver %>% mutate(division = "All Divisions")
)

toTable <- cleanDataAll %>%
  group_by(division, date) %>%
  count(response) %>%
  complete(response, fill = list(n = 0)) %>%
  filter(! is.na(response)) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  spread(key = response, value = freq) %>%
  mutate(engagement = `Strongly Agree` + `Agree`) %>% #back-quoting is bad
  select(division, date, engagement) %>%
  mutate(engagement = percent(engagement))

# Participation Table Creation

participateTable <- cleanDataNew %>%
  spread(key = question, value = response) %>%
  group_by(division) %>%
  count() %>%
  ungroup() %>%
  add_row(division = "All Divisions", n = sum(.$n)) %>%
  left_join(
    empCountNov %>% add_row(division = "All Divisions", count = sum(empCountNov$count))) %>%
  mutate(participation = percent(n / count)) %>%
  select(division, participation, n)

participateTable %>%
  left_join(
    cleanDataOld %>%
      spread(key = question, value = response) %>%
      group_by(division) %>%
      count() %>%
      ungroup() %>%
      add_row(division = "All Divisions", n = sum(.$n)),
    by = "division"
  )

# Input for graphs

toPlot <- byDriver %>%
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
  complete(division, date, driver_all) %>%
  mutate(engagement = engagement * 100)

toPlot$driver_all <- factor(
  toPlot$driver_all,
  levels = rev(c(
    "Affinity",
    "Communication",
    "Compensation",
    "Development",
    "Empowerment",
    "Leadership",
    "Performance",
    "Recognition",
    "Relations",
    "Teamwork",
    "Change Leadership",
    "Direct Manager Support"
  )),
  ordered = TRUE
)

toDetailedHeatMap <- byDriver %>%
  filter(date == "Nov. 2017") %>%
  group_by(division, driver_all, question) %>%
  count(response) %>%
  complete(response, fill = list(n = 0)) %>%
  filter(! is.na(response)) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  spread(key = response, value = freq) %>%
  mutate(engagement = `Strongly Agree` + `Agree`) %>%
  select(division, driver_all, question, engagement) %>%
  mutate(engagement = engagement * 100)

colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02")

results_all <- ggplot(toPlot %>% filter(division == "All Divisions"), aes(x = driver_all, y = engagement, alpha = date)) + 
  geom_bar(width = 0.75, stat = "identity", position = "dodge", fill = colors[1]) +
  scale_alpha_discrete(range = c(0.4, 1)) +
  ylim(c(0, 100)) +
  labs(
    title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
    subtitle = "All Divisions") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black"),
    legend.title = element_blank(),
    legend.position = "top",
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

results_corporate <- ggplot(toPlot %>% filter(division == "Corporate"), aes(x = driver_all, y = engagement, alpha = date)) + 
  geom_bar(width = 0.75, stat = "identity", position = "dodge", fill = colors[2]) +
  scale_alpha_discrete(range = c(0.4, 1)) +
  ylim(c(0, 100)) +
  labs(
    title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
    subtitle = "Corporate") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black"),
    legend.title = element_blank(),
    legend.position = "top",
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

results_scc <- ggplot(toPlot %>% filter(division == "Shaw Conference Centre"), aes(x = driver_all, y = engagement, alpha = date)) + 
  geom_bar(width = 0.75, stat = "identity", position = "dodge", fill = colors[3]) +
  scale_alpha_discrete(range = c(0.4, 1)) +
  ylim(c(0, 100)) +
  labs(
    title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
    subtitle = "Shaw Conference Centre") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black"),
    legend.title = element_blank(),
    legend.position = "top",
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

results_tourism <- ggplot(toPlot %>% filter(division == "Tourism"), aes(x = driver_all, y = engagement, alpha = date)) + 
  geom_bar(width = 0.75, stat = "identity", position = "dodge", fill = colors[4]) +
  scale_alpha_discrete(range = c(0.4, 1)) +
  ylim(c(0, 100)) +
  labs(
    title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
    subtitle = "Tourism") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black"),
    legend.title = element_blank(),
    legend.position = "top",
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

results_TI <- ggplot(toPlot %>% filter(division == "Trade and Investment"), aes(x = driver_all, y = engagement, alpha = date)) + 
  geom_bar(width = 0.75, stat = "identity", position = "dodge", fill = colors[5]) +
  scale_alpha_discrete(range = c(0.4, 1)) +
  ylim(c(0, 100)) +
  labs(
    title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
    subtitle = "Trade and Investment") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black"),
    legend.title = element_blank(),
    legend.position = "top",
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

results_urban <- ggplot(toPlot %>% filter(division == "Urban Economy"), aes(x = driver_all, y = engagement, alpha = date)) + 
  geom_bar(width = 0.75, stat = "identity", position = "dodge", fill = colors[6]) +
  scale_alpha_discrete(range = c(0.4, 1)) +
  ylim(c(0, 100)) +
  labs(
    title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
    subtitle = "Urban Economy") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line(color = "black"),
    legend.title = element_blank(),
    legend.position = "top",
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

heatmap <- ggplot(
  toPlot %>% filter(date == "Nov. 2017") %>%
    mutate(bin = cut(engagement, breaks = c(0, 69, 81, 100))), 
  aes(x = division, y = driver_all, fill = bin)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(engagement, 0))) +
  scale_fill_manual(
    values = c("#d7191c", "#ffffbf", "#1a9641"),
    labels = c("0 - 69", "70 - 80", "81 - 100")) + 
  labs(
    fill = "Engagement\nScore",
    title = "Engagement Score by Key Driver and Division",
    subtitle = "Percentage of \'Agree\' responses or higher") +
  scale_x_discrete(position = "top", labels = function(x) str_wrap(x, width = 10)) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank())

makeDetailedHeatMap <- function(tib, driver) {
  ggplot(
    tib %>%
      filter(driver_all == driver) %>%
      mutate(bin = cut(engagement, breaks = c(0, 69, 81, 100))),
    aes(x = division, y = question, fill = bin)) +
    geom_tile(color = "black") +
    geom_text(aes(label = round(engagement, 0))) +
    scale_fill_manual(
      values = c("#d7191c", "#ffffbf", "#1a9641"),
      labels = c("0 - 69", "70 - 80", "81 - 100")) + 
    labs(
      fill = "Engagement\nScore",
      title = paste("Engagement Score by Question and Division -", driver),
      subtitle = "Percentage of \'Agree\' responses or higher") +
    scale_x_discrete(position = "top", labels = function(x) str_wrap(x, width = 10)) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 35)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank())
}

# Stacked bar chart attempt

lvls <- c("Strongly Disagree", "Disagree", "Strongly Agree", "Agree")

stackdata <- byDriver %>%
  group_by(division, driver_all, date) %>%
  count(response) %>%
  complete(response, fill = list(n = 0)) %>%
  filter(! is.na(response)) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(freq = ifelse(response == "Disagree", -freq, freq)) %>%
  mutate(freq = ifelse(response == "Strongly Disagree", -freq, freq)) %>%
  filter(! response == "Neither Agree nor Disagree") %>%
  ungroup() %>%
  select(-n)

stackdata$response <- factor(stackdata$response, levels = lvls)

stackdata$driver_all <- factor(
  stackdata$driver_all,
  levels = rev(c(
    "Affinity",
    "Communication",
    "Compensation",
    "Development",
    "Empowerment",
    "Leadership",
    "Performance",
    "Recognition",
    "Relations",
    "Teamwork",
    "Change Leadership",
    "Direct Manager Support"
  )),
  ordered = TRUE
)

colour_palette <- c("#e74a4e", "#df7081", "#5e94d0", "#7caadc")

ALLstack_chart <- ggplot(stackdata %>%
                          filter(division == "All Divisions"),
                        aes(x = driver_all, y = freq)) + 
  geom_bar(width = 0.75, aes(fill = response), stat = "identity") +
  scale_fill_manual(
    values = colour_palette,
    breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")) +
  labs(
    title = "Frequency of Response Type by Driver", 
    subtitle = "All Divisions") +
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
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

UEstack_chart <- ggplot(stackdata %>%
                        filter(division == "Urban Economy"),
                      aes(x = driver_all, y = freq)) + 
  geom_bar(width = 0.75, aes(fill = response), stat = "identity") +
  scale_fill_manual(
    values = colour_palette,
    breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")) +
  labs(
    title = "Frequency of Response Type by Driver", 
    subtitle = "Urban Economy") +
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
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

CORstack_chart <- ggplot(stackdata %>%
                          filter(division == "Corporate"),
                        aes(x = driver_all, y = freq)) + 
  geom_bar(width = 0.75, aes(fill = response), stat = "identity") +
  scale_fill_manual(
    values = colour_palette,
    breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")) +
  labs(
    title = "Frequency of Response Type by Driver", 
    subtitle = "Corporate") +
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
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

TOURstack_chart <- ggplot(stackdata %>%
                          filter(division == "Tourism"),
                        aes(x = driver_all, y = freq)) + 
  geom_bar(width = 0.75, aes(fill = response), stat = "identity") +
  scale_fill_manual(
    values = colour_palette,
    breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")) +
  labs(
    title = "Frequency of Response Type by Driver", 
    subtitle = "Tourism") +
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
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

SCCstack_chart <- ggplot(stackdata %>%
                          filter(division == "Shaw Conference Centre"),
                        aes(x = driver_all, y = freq)) + 
  geom_bar(width = 0.75, aes(fill = response), stat = "identity") +
  scale_fill_manual(
    values = colour_palette,
    breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")) +
  labs(
    title = "Frequency of Response Type by Driver", 
    subtitle = "Shaw Conference Centre") +
  coord_flip() +
  scale_y_continuous(limits = c(-0.5, 1)) +
  facet_wrap(~ date) +
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
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

TIstack_chart <- ggplot(stackdata %>%
                          filter(division == "Trade and Investment"),
                        aes(x = driver_all, y = freq)) + 
  geom_bar(width = 0.75, aes(fill = response), stat = "identity") +
  scale_fill_manual(
    values = colour_palette,
    breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")) +
  labs(
    title = "Frequency of Response Type by Driver", 
    subtitle = "Trade and Investment") +
  coord_flip() +
  scale_y_continuous(limits = c(-0.5, 1)) +
  facet_wrap(~ date) +
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
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Participation Rates Table

PResults2017 <- participateTable %>%
  select(-n) %>%
  spread(key = division, value = participation) %>%
  add_column(Date = "Nov. 2017", .before = "All Divisions") %>%
  add_row(Date = "Jan. 2017", `All Divisions` = percent(0.84), `Corporate` = percent(0.86),
          `Shaw Conference Centre` = percent(0.71), `Tourism` = percent(0.86),
          `Trade and Investment` = percent(0.92), `Urban Economy` = percent(1.0), .before = 1)


# Engagement Results Table
# tibble (will not convert to data frame for use with flextable)

EResults2017 <- toTable %>%
  spread(key = division, value = engagement)


EResults2017 <- as.data.frame(EResults2017)

#------------------------------------------------------------------------

ppt <- read_pptx()

ppt %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_table(value = EResults2017, type = "body", index = 1) %>%
  ph_with_text(str = "2017 Engagement Results", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_table(value = PResults2017, type = "body", index = 1) %>%
  ph_with_text(str = "2017 Participation Results", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(heatmap), type = "body") %>%
  ph_with_text(str = "Results by Division - Summary", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(results_all), type = "body") %>%
  ph_with_text(str = "Overall Results", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(results_corporate), type = "body") %>%
  ph_with_text(str = "Results by Division", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(results_scc), type = "body") %>%
  ph_with_text(str = "Results by Division", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(results_tourism), type = "body") %>%
  ph_with_text(str = "Results by Division", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(results_TI), type = "body") %>%
  ph_with_text(str = "Results by Division", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(results_urban), type = "body") %>%
  ph_with_text(str = "Results by Division", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(ALLstack_chart), type = "body") %>%
  ph_with_text(str = "Response Summary by Division", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(UEstack_chart), type = "body") %>%
  ph_with_text(str = "Response Summary by Division", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(SCCstack_chart), type = "body") %>%
  ph_with_text(str = "Response Summary by Division", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(TIstack_chart), type = "body") %>%
  ph_with_text(str = "Response Summary by Division", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(CORstack_chart), type = "body") %>%
  ph_with_text(str = "Response Summary by Division", type = "title") %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(TOURstack_chart), type = "body") %>%
  ph_with_text(str = "Response Summary by Division", type = "title")

# Appendix - Heatmap by Question and Driver

drivers <- unique(toPlot$driver_all)

plotList <- lapply(drivers, makeDetailedHeatMap, tib = toDetailedHeatMap)

for (plot in plotList) {
  ppt %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_vg(code = print(plot), type = "body") %>%
    ph_with_text(str = "Results by Division - Detailed", type = "title")
}

print(ppt, target = "test.pptx") %>%
  invisible()


