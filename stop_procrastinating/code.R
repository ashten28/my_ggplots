# title: charting boxes as number of weeks we have in a lifetime
# author: ashten anthony
# date: 20200222

library(dplyr)
library(lubridate)
library(forcats)
library(ggplot2)
library(scales)

# your birth year
birth_year <- 1994

# expected number of years you will live (mortality)
n_total_years <- 85

# age
age_in_years  <- year(Sys.Date()) - birth_year
age_in_months <- age_in_years*12 + month(Sys.Date())

# theme pallete
theme_pallete <- 
  c("#c7821b", "#f5c228", "#fad20e", "#87ffff", "#333333", "#212121", "#777b7e50")

scales::show_col(theme_pallete)

# create data set for plotting
n_months_data <-
  tibble(
    month = seq(from = 1, to = n_total_years*12)
  ) %>% 
  mutate(
    # axis_x = rep_len(1:12, length.out = n()),
    # axis_y = ceiling(month/12),
    axis_x = rep_len(1:24, length.out = n()),
    axis_y = ceiling(month/24),
    period = case_when(
      month <= age_in_months -12 -1 ~ "Past",
      month == age_in_months -12 ~ "Present",
      month  > age_in_months -12 ~ "Future"
    )
  ) 

# plot months as tiles
n_months_plot <- 
  n_months_data %>% 
  ggplot(aes(y = axis_x, x = axis_y, fill = fct_relevel(period, c("Past", "Present", "Future")))) +
  geom_tile(width = 0.7, height = 0.7, colour = "black") +
  scale_fill_manual(values = c(theme_pallete[7], theme_pallete[6], theme_pallete[3]))+
  scale_x_continuous(breaks = seq(from = 0, to = n_total_years), labels = seq(from = 0, to = n_total_years)*2-1, expand = c(0,0)) +
  scale_y_continuous(breaks = c(12,24), expand = c(0,0)) +
  coord_equal() +
  labs(
    x = "\nAge", 
    y = "Months\n"
  ) + 
  # theme_bw() +
  # ggtitle("Stop procrasting") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    # axis.text.y = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
    # axis.line.y = element_blank()
    # axis.line = element_line(colour = "black")
  )

# plot
n_months_plot

# save plot
ggsave(filename = "stop_procrastinating/stop_procrastinating.png", plot = n_months_plot, width = 16, height = 9)



