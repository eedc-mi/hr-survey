#'---
#'title: EEDC Employee Engagement Survey
#'author: Market Intelligence
#'date: Nov 5, 2018
#'output:
#'  beamer_presentation
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

#+ code, include = FALSE
library(tidyverse)
library(here)
library(scales)
library(flextable)
library(stringr)
library(rmarkdown)

setwd(here())

data_path <- file.path(
  "K:",
  "Corporate",
  "Human Resources",
  "Employee Engagement Survey Results",
  "Nov 2017",
  "data"
)

data_new <- read_csv(file.path(data_path, "nov2017.csv"))
data_old <- read_csv(file.path(data_path, "jan2017.csv"))
questions_new <- read_csv(file.path(data_path, "questions2017.csv"))
questions_old <- read_csv(file.path(data_path, "questionsJan2017.csv"))
employee_count_new <- read_csv(file.path(data_path, "empCountNov212017.csv"))

clean_data <- function(tib, date) {
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

to_by_driver <- function(tib, qs) {
  driver_list <- unique(
    c(
      unique(qs$driver_1[! is.na(qs$driver_1)]),
      unique(qs$driver_2[! is.na(qs$driver_2)]), 
      unique(qs$driver_3[! is.na(qs$driver_3)])
    )
  )
  
  # Leadership removed at HR request
  driver_list <- driver_list[driver_list != "Leadership"]
  
  tib <- tib %>% left_join(qs)
  
  by_driver <- tibble()
  
  for (d in driver_list) {
    by_driver <- bind_rows(
      by_driver,
      tib %>%
        filter(driver_1 == d | driver_2 == d | driver_3 == d) %>%
        mutate(driver_all = d)
    )
  }
  
  by_driver
}

data_old <- bind_cols(
  data_old %>% select("Respondent ID", "Division"),
  data_old %>% select(-1, -2) %>% mutate_all(
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

clean_data_new <- clean_data(data_new, "Nov. 2017")
clean_data_old <- clean_data(data_old, "Jan. 2017")

clean_data_all <- bind_rows(
  clean_data_new,
  clean_data_old
)

clean_data_all <- bind_rows(
  clean_data_all, 
  clean_data_all %>% mutate(division = "All Divisions")
)
  
by_driver <- bind_rows(
  to_by_driver(clean_data_new, questions_new),
  to_by_driver(clean_data_old, questions_old)
)

by_driver <- bind_rows(
  by_driver,
  by_driver %>% mutate(division = "All Divisions")
)

summary_table <- clean_data_all %>%
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

participation_table <- clean_data_new %>%
  spread(key = question, value = response) %>%
  group_by(division) %>%
  count() %>%
  ungroup() %>%
  add_row(division = "All Divisions", n = sum(.$n)) %>%
  left_join(
    employee_count_new %>% add_row(division = "All Divisions", count = sum(employee_count_new$count))) %>%
  mutate(participation = percent(n / count)) %>%
  select(division, participation, n)

participation_table %>%
  left_join(
    clean_data_old %>%
      spread(key = question, value = response) %>%
      group_by(division) %>%
      count() %>%
      ungroup() %>%
      add_row(division = "All Divisions", n = sum(.$n)),
    by = "division"
  )

to_yoy_plot <- by_driver %>%
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

to_yoy_plot$driver_all <- factor(
  to_yoy_plot$driver_all,
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

to_detailed_heatmap <- by_driver %>%
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

to_facet_plot <- by_driver %>%
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

lvls <- c("Strongly Disagree", "Disagree", "Strongly Agree", "Agree")
to_facet_plot$response <- factor(to_facet_plot$response, levels = lvls)

to_facet_plot$driver_all <- factor(
  to_facet_plot$driver_all,
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

#colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02")

make_results_plot <- function(data, div) {
  ggplot(
    data %>% filter(division == div), 
    aes(x = driver_all, y = engagement, alpha = date)
  ) + 
  geom_bar(width = 0.75, stat = "identity", position = "dodge",  fill = "#1b9e77") +
  scale_alpha_discrete(range = c(0.4, 1)) +
  ylim(c(0, 100)) +
  labs(
    title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
    subtitle = div) +
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
}

make_heatmap <- ggplot(
  to_yoy_plot %>% filter(date == "Nov. 2017") %>%
    mutate(bin = cut(engagement, breaks = c(0, 69, 81, 100))), 
  aes(x = division, y = driver_all, fill = bin)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(engagement, 0))) +
  scale_fill_manual(
    values = c("#d7191c", "#ffffbf", "#1a9641"),
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

make_detail_heatmap <- function(tib, driver) {
  ggplot(
    tib %>%
      filter(driver_all == driver) %>%
      mutate(
        engagement = round(engagement),
        bin = cut(engagement, breaks = c(0, 69, 81, 100))),
    aes(x = division, y = question, fill = bin)) +
    geom_tile(color = "black") +
    geom_text(aes(label = round(engagement, 0))) +
    scale_fill_manual(
      values = c("#d7191c", "#ffffbf", "#1a9641"),
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

make_facet_plot <- function(data, div) {
  colour_palette <- c("#e74a4e", "#df7081", "#5e94d0", "#7caadc")
  
  ggplot(data %>% filter(division == div),
         aes(x = driver_all, y = freq)) + 
    geom_bar(width = 0.75, aes(fill = response), stat = "identity")+
    scale_fill_manual(
      values = colour_palette,
      breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree"))  +
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
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

# # Participation Rates Table
# 
# PResults2017 <- participation_table %>%
#   select(-n) %>%
#   spread(key = division, value = participation) %>%
#   add_column(Date = "Nov. 2017", .before = "All Divisions") %>%
#   add_row(Date = "Jan. 2017", `All Divisions` = percent(0.84), `Corporate` = percent(0.86),
#           `Shaw Conference Centre` = percent(0.71), `Tourism` = percent(0.86),
#           `Trade and Investment` = percent(0.92), `Urban Economy` = percent(1.0), .before = 1)
# 
# 
# # Engagement Results Table
# # tibble (will not convert to data frame for use with flextable)
# 
# EResults2017 <- toTable %>%
#   spread(key = division, value = engagement)
# 
# 
# EResults2017 <- as.data.frame(EResults2017)


#'        
#'### Engagement Score by Driver and Division
#+ echo = FALSE, warning = FALSE, message = FALSE
make_heatmap

#+ echo=FALSE, message=FALSE, warning=FALSE, results='asis'
for (d in unique(to_yoy_plot$division)) {
  cat("\n###", " Year to Year Comparison - ", d, "  \n")
  print(make_results_plot(to_yoy_plot, d))
  cat("  \n")
}

#+ echo=FALSE, message=FALSE, warning=FALSE, results='asis'
for (d in unique(to_facet_plot$division)) {
  cat("\n###", " Response Summary -  ", d, "  \n")
  print(make_facet_plot(to_facet_plot, d))
  cat("  \n")
}

#+ echo=FALSE, message=FALSE, warning=FALSE, results='asis'
for (d in unique(to_detailed_heatmap$driver_all)) {
  cat("\n###", " Engagement by Question and Division",  "\n")
  print(make_detail_heatmap(to_detailed_heatmap, d))
  cat("  \n")
}


#+ old, include = FALSE
# ppt <- read_pptx()
# 
# ppt %>%
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_table(value = EResults2017, type = "body", index = 1) %>%
#   ph_with_text(str = "2017 Engagement Results", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_table(value = PResults2017, type = "body", index = 1) %>%
#   ph_with_text(str = "2017 Participation Results", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(heatmap), type = "body") %>%
#   ph_with_text(str = "Results by Division - Summary", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(results_all), type = "body") %>%
#   ph_with_text(str = "Overall Results", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(results_corporate), type = "body") %>%
#   ph_with_text(str = "Results by Division", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(results_scc), type = "body") %>%
#   ph_with_text(str = "Results by Division", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(results_tourism), type = "body") %>%
#   ph_with_text(str = "Results by Division", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(results_TI), type = "body") %>%
#   ph_with_text(str = "Results by Division", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(results_urban), type = "body") %>%
#   ph_with_text(str = "Results by Division", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(ALLstack_chart), type = "body") %>%
#   ph_with_text(str = "Response Summary by Division", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(UEstack_chart), type = "body") %>%
#   ph_with_text(str = "Response Summary by Division", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(SCCstack_chart), type = "body") %>%
#   ph_with_text(str = "Response Summary by Division", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(TIstack_chart), type = "body") %>%
#   ph_with_text(str = "Response Summary by Division", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(CORstack_chart), type = "body") %>%
#   ph_with_text(str = "Response Summary by Division", type = "title") %>%
#   
#   add_slide(layout = "Title and Content", master = "Office Theme") %>%
#   ph_with_vg(code = print(TOURstack_chart), type = "body") %>%
#   ph_with_text(str = "Response Summary by Division", type = "title")
# 
# # Appendix - Heatmap by Question and Driver
# 
# drivers <- unique(to_yoy_plot$driver_all)
# 
# plotList <- lapply(drivers, make_detail_heatmap, tib = to_detailed_heatmap)
# 
# for (plot in plotList) {
#   ppt %>%
#     add_slide(layout = "Title and Content", master = "Office Theme") %>%
#     ph_with_vg(code = print(plot), type = "body") %>%
#     ph_with_text(str = "Results by Division - Detailed", type = "title")
# }
# 
# print(ppt, target = "test.pptx") %>%
#   invisible()




# ALLstack_chart <- ggplot(to_facet_plot %>%
#                            filter(division == "All Divisions"),
#                          aes(x = driver_all, y = freq)) + 
#   geom_bar(width = 0.75, aes(fill = response), stat = "identity") +
#   scale_fill_manual(
#     values = colour_palette,
#     breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")) +
#   labs(
#     title = "Frequency of Response Type by Driver", 
#     subtitle = "All Divisions") +
#   facet_wrap(~ date) +
#   coord_flip() +
#   scale_y_continuous(limits = c(-0.5, 1)) +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.spacing = unit(1, "lines"),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line = element_line(color = "black"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# UEstack_chart <- ggplot(to_facet_plot %>%
#                           filter(division == "Urban Economy"),
#                         aes(x = driver_all, y = freq)) + 
#   geom_bar(width = 0.75, aes(fill = response), stat = "identity") +
#   scale_fill_manual(
#     values = colour_palette,
#     breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")) +
#   labs(
#     title = "Frequency of Response Type by Driver", 
#     subtitle = "Urban Economy") +
#   facet_wrap(~ date) +
#   coord_flip() +
#   scale_y_continuous(limits = c(-0.5, 1)) +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.spacing = unit(1, "lines"),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line = element_line(color = "black"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# CORstack_chart <- ggplot(to_facet_plot %>%
#                            filter(division == "Corporate"),
#                          aes(x = driver_all, y = freq)) + 
#   geom_bar(width = 0.75, aes(fill = response), stat = "identity") +
#   scale_fill_manual(
#     values = colour_palette,
#     breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")) +
#   labs(
#     title = "Frequency of Response Type by Driver", 
#     subtitle = "Corporate") +
#   facet_wrap(~ date) +
#   coord_flip() +
#   scale_y_continuous(limits = c(-0.5, 1)) +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.spacing = unit(1, "lines"),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line = element_line(color = "black"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# TOURstack_chart <- ggplot(to_facet_plot %>%
#                             filter(division == "Tourism"),
#                           aes(x = driver_all, y = freq)) + 
#   geom_bar(width = 0.75, aes(fill = response), stat = "identity") +
#   scale_fill_manual(
#     values = colour_palette,
#     breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")) +
#   labs(
#     title = "Frequency of Response Type by Driver", 
#     subtitle = "Tourism") +
#   facet_wrap(~ date) +
#   coord_flip() +
#   scale_y_continuous(limits = c(-0.5, 1)) +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.spacing = unit(1, "lines"),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line = element_line(color = "black"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# SCCstack_chart <- ggplot(to_facet_plot %>%
#                            filter(division == "Shaw Conference Centre"),
#                          aes(x = driver_all, y = freq)) + 
#   geom_bar(width = 0.75, aes(fill = response), stat = "identity") +
#   scale_fill_manual(
#     values = colour_palette,
#     breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")) +
#   labs(
#     title = "Frequency of Response Type by Driver", 
#     subtitle = "Shaw Conference Centre") +
#   coord_flip() +
#   scale_y_continuous(limits = c(-0.5, 1)) +
#   facet_wrap(~ date) +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.spacing = unit(1, "lines"),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line = element_line(color = "black"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# TIstack_chart <- ggplot(to_facet_plot %>%
#                           filter(division == "Trade and Investment"),
#                         aes(x = driver_all, y = freq)) + 
#   geom_bar(width = 0.75, aes(fill = response), stat = "identity") +
#   scale_fill_manual(
#     values = colour_palette,
#     breaks = c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")) +
#   labs(
#     title = "Frequency of Response Type by Driver", 
#     subtitle = "Trade and Investment") +
#   coord_flip() +
#   scale_y_continuous(limits = c(-0.5, 1)) +
#   facet_wrap(~ date) +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.spacing = unit(1, "lines"),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line = element_line(color = "black"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )

# results_all <- ggplot(to_yoy_plot %>% filter(division == "All Divisions"), aes(x = driver_all, y = engagement, alpha = date)) + 
#   geom_bar(width = 0.75, stat = "identity", position = "dodge", fill = colors[1]) +
#   scale_alpha_discrete(range = c(0.4, 1)) +
#   ylim(c(0, 100)) +
#   labs(
#     title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
#     subtitle = "All Divisions") +
#   coord_flip() +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line = element_line(color = "black"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# results_corporate <- ggplot(to_yoy_plot %>% filter(division == "Corporate"), aes(x = driver_all, y = engagement, alpha = date)) + 
#   geom_bar(width = 0.75, stat = "identity", position = "dodge", fill = colors[2]) +
#   scale_alpha_discrete(range = c(0.4, 1)) +
#   ylim(c(0, 100)) +
#   labs(
#     title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
#     subtitle = "Corporate") +
#   coord_flip() +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line = element_line(color = "black"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# results_scc <- ggplot(to_yoy_plot %>% filter(division == "Shaw Conference Centre"), aes(x = driver_all, y = engagement, alpha = date)) + 
#   geom_bar(width = 0.75, stat = "identity", position = "dodge", fill = colors[3]) +
#   scale_alpha_discrete(range = c(0.4, 1)) +
#   ylim(c(0, 100)) +
#   labs(
#     title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
#     subtitle = "Shaw Conference Centre") +
#   coord_flip() +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line = element_line(color = "black"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# results_tourism <- ggplot(to_yoy_plot %>% filter(division == "Tourism"), aes(x = driver_all, y = engagement, alpha = date)) + 
#   geom_bar(width = 0.75, stat = "identity", position = "dodge", fill = colors[4]) +
#   scale_alpha_discrete(range = c(0.4, 1)) +
#   ylim(c(0, 100)) +
#   labs(
#     title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
#     subtitle = "Tourism") +
#   coord_flip() +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line = element_line(color = "black"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# results_TI <- ggplot(to_yoy_plot %>% filter(division == "Trade and Investment"), aes(x = driver_all, y = engagement, alpha = date)) + 
#   geom_bar(width = 0.75, stat = "identity", position = "dodge", fill = colors[5]) +
#   scale_alpha_discrete(range = c(0.4, 1)) +
#   ylim(c(0, 100)) +
#   labs(
#     title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
#     subtitle = "Trade and Investment") +
#   coord_flip() +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line = element_line(color = "black"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
# 
# results_urban <- ggplot(to_yoy_plot %>% filter(division == "Urban Economy"), aes(x = driver_all, y = engagement, alpha = date)) + 
#   geom_bar(width = 0.75, stat = "identity", position = "dodge", fill = colors[6]) +
#   scale_alpha_discrete(range = c(0.4, 1)) +
#   ylim(c(0, 100)) +
#   labs(
#     title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
#     subtitle = "Urban Economy") +
#   coord_flip() +
#   theme_bw() +
#   theme(
#     panel.border = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.line = element_line(color = "black"),
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title = element_blank(),
#     plot.title = element_text(hjust = 0.5),
#     plot.subtitle = element_text(hjust = 0.5)
#   )
