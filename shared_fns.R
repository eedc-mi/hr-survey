library(tidyverse)
library(rlang)

venue_name_cc <- "Shaw Conference Centre"
venue_name_expo <- "Edmonton Expo Centre"
colour_lowest <- "#e74a4e"
colour_low <- "#df7081"
colour_high <- "#5e94d0"
colour_highest <- "#7caadc"

count_employees <- function(tbl_df, group_var, date) {
  group_var <- enquo(group_var)
  
  tbl_df %>% 
    group_by(!!group_var) %>%
    count() %>%
    rename(count = n) %>%
    ungroup() %>%
    add_row(!!group_var := "All", count = sum(.$count)) %>%
    mutate(date = date)
}

clean_data_pt <- function(tbl_df, venue, date) {
  tbl_df <- tbl_df %>%
    rename(department_expo = 6, department_scc = 7) %>%
    replace_na(list(department_expo = "", department_scc = "")) %>% 
    unite("department", c(department_expo, department_scc), sep = "") %>%
    na_if("") %>% 
    select(
      -starts_with("Collector"),
      -starts_with("Start"), 
      -starts_with("End"),
      division = starts_with("What venue"),
      id = "Respondent ID") %>%
    select(1:(ncol(.) - 4))
  
  drop_index <- if_else(venue == "Shaw Conference Centre", 4, 6)
  tbl_df <- tbl_df %>% 
    drop_na(drop_index) %>% 
    gather(key = "question", value = "response", -id, -division, -department) %>%
    mutate(date = date)
  
  lvls_lower <- c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")
  tbl_df$response <- factor(tbl_df$response, levels = lvls_lower, ordered = TRUE)
  
  tbl_df %>% 
    filter(division == !!venue) %>%
    bind_rows(tbl_df %>% mutate(department = "All"))
}

to_by_driver_pt <- function(tbl_df, qs) {
  driver_list <- unique(
    c(
      unique(qs$driver_1[! is.na(qs$driver_1)]),
      unique(qs$driver_2[! is.na(qs$driver_2)]), 
      unique(qs$driver_3[! is.na(qs$driver_3)])
    )
  )
  
  # Leadership removed at HR request
  driver_list <- driver_list[driver_list != "Leadership"]
  
  tbl_df <- tbl_df %>% left_join(qs)
  
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
  
  by_driver %>% bind_rows(
    by_driver %>% mutate(department = "All")
  )
}

calc_engagement_by <- function(tbl_df, ...) {
  group_vars <- quos(...)
  
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

make_summary_table_by <- function(tbl_df, ...) {
  group_vars <- quos(...)
  
  tbl_df %>%
    calc_engagement_by(!!!group_vars) %>%
    select(!!!group_vars, engagement) %>%
    mutate(engagement = percent(engagement, 1))
}

make_participation_table_by <- function(tbl_df, emp_count, ...) {
  group_vars <- quos(...)
  
  tbl_df %>%
    spread(key = question, value = response) %>%
    group_by(!!!group_vars) %>%
    count() %>%
    ungroup() %>%
    left_join(emp_count) %>%
    mutate(participation = percent(n / count, 1)) %>%
    select(-count, -n) %>%
    spread(date, participation)
}

yoy_plot_transform <- function(tbl_df, group_var, neg_exp = "FALSE") {
  group_var <- enquo(group_var)
  neg_exp <- rlang::parse_expr(neg_exp)
  
  tbl_df %>%
    calc_engagement_by(!!group_var, driver_all, date) %>%
    select(!!group_var, driver_all, date, engagement) %>%
    spread(date, engagement) %>%
    mutate(is_negative = factor(!!neg_exp)) %>%
    gather(date, engagement, -(!!group_var), -driver_all, -is_negative) %>%
    complete(!!group_var, date, driver_all) %>%
    mutate(engagement = engagement * 100)
}

detail_heatmap_transform <- function(tbl_df, date, group_var) {
  group_var <- enquo(group_var)
  
  tbl_df %>%
    filter(date == 2018) %>%
    calc_engagement_by(!!group_var, driver_all, question) %>%
    select(!!group_var, driver_all, question, engagement) %>%
    mutate(engagement = engagement * 100)
}

yoy_facet_transform <- function(tbl_df, group_var, questions, diff_exp = "0") {
  group_var <- enquo(group_var)
  diff_exp <- rlang::parse_expr(diff_exp)
  
  tbl_df %>%
    calc_engagement_by(date, !!group_var, driver_all, question_id) %>%
    select(date, !!group_var, driver_all, question_id, engagement) %>%
    spread(date, engagement) %>%
    mutate(
      diff = !!diff_exp,
      change = case_when(
        diff > 0 ~ "pos",
        diff == 0 ~ "zero",
        diff < 0 ~ "neg"
      )) %>%
    select(-diff) %>%
    gather(date, engagement, -(!!group_var), -driver_all, -change, -question_id) %>%
    mutate(engagement = engagement * 100, change = replace_na(change, "zero")) %>%
    left_join(questions, by = "question_id")
}

bar_facet_transform <- function(tbl_df, group_var) {
  
  tbl_df <- tbl_df %>%
    group_by(!!group_var, driver_all, date) %>%
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
  tbl_df$response <- factor(to_facet_plot$response, levels = lvls)
  
  tbl_df
}

make_yoy_plot <- function(tbl_df, group_var, group) {
  group_var <- enquo(group_var)
  
  fill_vals <- c("darkgrey")
  if (length(unique(tbl_df$date)) > 1) {
    fill_vals <- c("white", fill_vals)
  }
  
  ggplot(
    tbl_df %>% filter(!!group_var == group), 
    aes(x = driver_all, y = engagement, fill = date, group = driver_all)) +
    geom_point(shape = 21, size = 5, colour = "black") +
    geom_line(aes(colour = is_negative), arrow = arrow(length=unit(0.30,"cm"), type = "closed"), show.legend = FALSE) +
    scale_fill_manual(values = fill_vals) +
    scale_colour_manual(values = c(colour_highest, colour_lowest), drop = FALSE) +
    ylim(c(25, 100)) +
    labs(
      title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
      subtitle = group) +
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

make_summary_heatmap <- function(tbl_df, group_var, date) {
  group_var <- enquo(group_var)
  
  ggplot(
    tbl_df %>% filter(date == date) %>%
      mutate(engagement = round(engagement)) %>%
      mutate(bin = cut(engagement, breaks = c(0, 69, 81, 100))), 
    aes(x = !!group_var, y = driver_all, fill = bin)) +
    geom_tile(color = "black") +
    geom_text(aes(label = engagement)) +
    scale_fill_manual(
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
}

make_detail_heatmap <- function(tbl_df, group_var, driver) {
  group_var <- enquo(group_var)
  
  ggplot(
    tbl_df %>%
      filter(driver_all == driver) %>%
      mutate(engagement = round(engagement)) %>%
      mutate(bin = cut(engagement, breaks = c(0, 69, 81, 100))),
    aes(x = !!group_var, y = question, fill = bin)) +
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

make_bar_facet <- function(tbl_df, group_var, group) {
  group_var <- enquo(group_var)
  
  palette <- c(colour_lowest, colour_low, colour_high, colour_highest)
  
  ggplot(tbl_df %>% 
           filter(group_var == group) %>% 
           mutate(freq = round(freq, 4)),
         aes(x = driver_all, y = freq)) + 
    geom_bar(width = 0.75, aes(fill = response), stat = "identity")+
    scale_fill_manual(
      values = palette,
      breaks = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))  +
    labs(
      title = "Frequency of Response Type by Driver", 
      subtitle = group) + 
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

make_yoy_facet <- function(tbl_df, group_var, driver) {
  format_label <- function(width) {
    function(strings) {
      str_wrap(strings, width = width)
    }
  }
  
  group_var <- enquo(group_var)
  nm <- quo_name(group_var)
  
  ggplot(tbl_df %>% 
           filter(driver_all == driver) %>%
           mutate(engagement = round(engagement)), 
         aes(x = factor(date), y = engagement, group = !!group_var, 
             label = engagement, colour = change)) + 
    facet_grid(
      vars(question),
      vars(!!group_var),
      switch = "y", 
      labeller = labeller(
        .rows = format_label(35),
        .cols = format_label(10))) + 
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
