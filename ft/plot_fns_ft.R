library(stringr)

colour_lowest <- "#e74a4e"
colour_low <- "#df7081"
colour_high <- "#5e94d0"
colour_highest <- "#7caadc"

format_label <- function(width) {
  function(strings) {
    str_wrap(strings, width = width)
  }
}

make_yoy_plot <- function(tbl_df, transform_fn, div) {
  tbl_df <- transform_fn(tbl_df)
  
  ggplot(
    tbl_df %>% filter(division == div), 
    aes(x = driver_all, y = engagement, fill = date, group = driver_all)) +
    geom_point(shape = 21, size = 5, colour = "black") +
    geom_line(
      aes(colour = is_negative), 
      arrow = arrow(length=unit(0.30, "cm"), type = "closed"), show.legend = FALSE) +
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

make_heatmap <- function(tbl_df, transform_fn, x, y) {
  tbl_df <- transform_fn(tbl_df)
  
  x <- enquo(x)
  y <- enquo(y)
  
  ggplot(
    tbl_df %>% filter(date == 2019) %>%
      mutate(engagement = round(engagement)) %>%
      mutate(bin = cut(engagement, breaks = c(0, 69, 80, 100))), 
    aes(x = !!x, y = !!y, fill = bin)) +
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

make_detail_heatmap <- function(tbl_df, transform_fn, driver) {
  tbl_df <- transform_fn(tbl_df)
  
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

make_facet_plot <- function(tbl_df, transform_fn, div, x, by) {
  tbl_df <- transform_fn(tbl_df)
  
  x <- enquo(x)
  palette <- c(colour_lowest, colour_low, colour_high, colour_highest)
  
  ggplot(tbl_df %>% filter(division == div) %>% mutate(freq = round(freq, 4)),
         aes(x = !!x, y = freq)) + 
    geom_bar(width = 0.75, aes(fill = response), stat = "identity")+
    scale_fill_manual(
      values = palette,
      breaks = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))  +
    labs(
      title = paste("Frequency of Response Type by", by), 
      subtitle = div) + 
    facet_wrap(~ date) +
    geom_hline(yintercept = 0) +
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

make_yoy_facet <- function(tbl_df, transform_fn, driver) {
  tbl_df <- transform_fn(tbl_df)
  
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

make_category_yoy_facet <- function(tbl_df, transform_fn) {
  tbl_df <- transform_fn(tbl_df)
  
  ggplot(
    tbl_df %>% mutate(engagement = round(engagement)), 
    aes(x = factor(date), 
        y = engagement, 
        group = division, 
        label = engagement, 
        colour = change)) + 
    facet_grid(
      category ~ division, 
      switch = "y", 
      labeller = labeller(division = format_label(10))) + 
    geom_point(show.legend = FALSE) +
    geom_text(nudge_y = 3, hjust = "center", vjust = "bottom", size = 3, show.legend = FALSE) +
    geom_line(show.legend = FALSE) +
    scale_colour_manual(values = c(colour_lowest, colour_highest, "darkgrey")) +
    scale_y_continuous(expand = expand_scale(mult = c(0.4, 0.4))) +
    labs(
      title = "Percentage of \'Agree\' responses or higher") +
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

make_pulse_bar_plot <- function(tbl_df, transform_fn) {
  tbl_df <- transform_fn(tbl_df)
  
  ggplot(
    tbl_df %>% mutate(division = fct_rev(division)),
    aes(x = division, y = engagement, label = round(engagement))) +
    geom_bar(stat = "identity") +
    geom_text(hjust = -1) +
    scale_x_discrete(limits = rev(levels(tbl_df$division))) +
    scale_y_continuous(limits = c(0, 100)) +
    coord_flip() +
    facet_grid(
      cols = vars(question),
      labeller = labeller(
        question = format_label(45)
      )) +
    labs(
      title = "Engagement Score (Percentage of \'Agree\' Responses or Higher)"
    ) +
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

make_pulse_facet_plot <- function(tbl_df, transform_fn) {
  tbl_df <- transform_fn(tbl_df)
  
  palette <- c(colour_lowest, colour_low, colour_high, colour_highest)
  
  ggplot(
    tbl_df %>% 
      mutate(freq = round(freq, 4), division = fct_rev(division)),
    aes(x = division, y = freq)) + 
    geom_bar(width = 0.75, aes(fill = response), stat = "identity")+
    scale_fill_manual(
      values = palette,
      breaks = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))  +
    labs(
      title = "Frequency of Response Type by Division") + 
    facet_wrap(
      ~ question,
      labeller = labeller(
        question = format_label(45)
      )) +
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