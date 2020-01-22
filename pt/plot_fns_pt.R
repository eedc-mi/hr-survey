library(stringr)

colour_lowest <- "#e74a4e"
colour_low <- "#df7081"
colour_high <- "#5e94d0"
colour_highest <- "#7caadc"

heatmap_lvls <- c(
  "0 - 69" = "(-Inf,69]", 
  "70 - 80" = "(69,80]", 
  "81 - 100" = "(80,100]")

heatmap_colours = c(colour_lowest, "white", colour_highest)
names(heatmap_colours) = c("0 - 69", "70 - 80", "81 - 100")

format_label <- function(width) {
  function(strings) {
    str_wrap(strings, width = width)
  }
}

make_yoy_plot <- function(tbl_df, transform_fn, dept) {
  tbl_df <- transform_fn(tbl_df)
  
  ggplot(
    tbl_df %>% filter(department == dept), 
    aes(x = driver_all, y = engagement, fill = date, group = driver_all)) +
    geom_point(shape = 21, size = 5, colour = "black") +
    geom_line(
      aes(colour = is_negative), 
      arrow = arrow(length=unit(0.30, "cm"), type = "closed"), show.legend = FALSE) +
    scale_fill_manual(values = c("white", "darkgrey")) +
    scale_colour_manual(values = c(colour_highest, colour_lowest), drop = FALSE) +
    ylim(c(0, 100)) +
    labs(
      title = "Engagement Score (Percentage of \'Agree\' Responses or Higher) by Driver", 
      subtitle = dept) +
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
      mutate(bin = cut(engagement, breaks = c(-Inf, 69, 80, 100))) %>%
      mutate(bin = fct_recode(bin, !!!heatmap_lvls)), 
    aes(x = !!x, y = !!y, fill = bin)) +
    geom_tile(color = "black") +
    geom_text(aes(label = engagement)) +
    scale_fill_manual(values = heatmap_colours) + 
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
      mutate(bin = cut(engagement, breaks = c(-Inf, 69, 80, 100))) %>%
      mutate(bin = fct_recode(bin, !!!heatmap_lvls)),
    aes(x = department, y = question, fill = bin)) +
    geom_tile(color = "black") +
    geom_text(aes(label = engagement)) +
    scale_fill_manual(values = heatmap_colours) + 
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

make_facet_plot <- function(tbl_df, transform_fn, dept, x, by) {
  tbl_df <- transform_fn(tbl_df)
  
  x <- enquo(x)
  palette <- c(colour_lowest, colour_low, colour_high, colour_highest)
  
  ggplot(tbl_df %>% filter(department == dept) %>% mutate(freq = round(freq, 4)),
         aes(x = !!x, y = freq)) + 
    geom_bar(width = 0.75, aes(fill = response), stat = "identity")+
    scale_fill_manual(
      values = palette,
      breaks = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))  +
    labs(
      title = paste("Frequency of Response Type by", by), 
      subtitle = dept) + 
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
             group = department, label = engagement, colour = change)) + 
    facet_grid(
      question ~ department, 
      switch = "y", 
      labeller = labeller(
        question = format_label(35),
        department = format_label(10))) + 
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
        group = department, 
        label = engagement, 
        colour = change)) + 
    facet_grid(
      category ~ department, 
      switch = "y", 
      labeller = labeller(department = format_label(10))) + 
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