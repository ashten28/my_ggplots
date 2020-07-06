library(readr)
library(tidyr)
library(dplyr)
library(rvest)
library(ggplot2)
library(ggtext)
library(rnaturalearth)
library(sf)

# read malaysia covid19 cases by district
malaysia_covid19_cases <- 
  read_csv("covid19/data/malaysia_district_cases.csv")

# use 'rvest' to scrape html table containing malaysia's states centroid
states_centroid_raw <- 
  read_html("https://www.distancelatlong.com/country/malaysia") %>% 
  html_node(xpath = "/html/body/div/div[6]/div[4]/div/div/table") %>% 
  html_table()

# clean up the centroid table and add in Putrajaya and Labuan centroids
states_centroid <- 
  states_centroid_raw %>% 
  setNames(c("state", "latitude", "longitude")) %>% 
  mutate(state = gsub("\\s*\\([^\\)]+\\)", "", state)) %>% 
  mutate(state = replace(state, state == "Trengganu", "Terengganu")) %>% 
  add_row(
    state    = c("Putrajaya", "Labuan"),
    latitude  = c(2.9262, 5.2831),
    longitude = c(101.6964, 115.2308)
  )

# get sf for malaysia
malaysia_sf <- 
  ne_countries(country = c("Malaysia"), scale = "large", returnclass = "sf")

# join cases data, sf
malaysia_map <-
  left_join(
    x = malaysia_covid19_cases,
    y = states_centroid,
    by = c("state")
  )

# Dataset for red "arrows" (to draw with geom_polygon)
price_arrows <- 
  # malaysia_map %>% 
  malaysia_covid19_cases %>% 
  filter(state == "Selangor") %>% 
  select(district, cum_cases, latitude, longitude) %>% 
  group_by(district) %>% 
  mutate(
    cases_sum = sum(cum_cases),
    arrow_x   = list(c(longitude - 0.15, longitude, longitude + 0.15, longitude)),
    arrow_y   = list(c(latitude - 0.03, latitude, latitude - 0.03, latitude + cases_sum/500))
  ) %>%
  unnest(c(arrow_x, arrow_y))

ggplot() +
  # map
  geom_sf(data = malaysia_sf, aes(geometry = geometry), fill = "#EBE9E1", colour = "grey70", size = 0.25) +
  # country name
  # geom_text(data = price_arrows, aes(x = longitude - 0.2, y = latitude - 0.4, label = state), check_overlap = TRUE, hjust = 0, vjust = 1, size = 3.5) +
  # red price, over 10M
  # geom_text(data = price_arrows, aes(x = longitude - 0.2, y = latitude - 1.1, label = positive_cases), check_overlap = TRUE, hjust = 0, vjust = 1, size = 3.5, colour = "#BA4E35")  +
  # black price, under 10M
  # geom_text(data = price_arrows, aes(x = longitude - 0.2, y = latitude - 1.1, label = positive_cases), check_overlap = TRUE, hjust = 0, vjust = 1, size = 3.5, colour = "black")  +
  # red arrows
  geom_polygon(data = price_arrows, aes(x = arrow_x, y = arrow_y, group = district), fill = "#BA4E35", colour = NA, alpha = 0.8) +
  # title and caption
  # annotate("richtext", x = -26, y = 80, hjust = 0, vjust = 1,
  #          label = "**Total amount of GDPR fines<br>by country**<br><span style = 'font-size:12pt'>Fine prices rounded to nearest million or thousand euro</span><br><span style = 'font-size:8pt'>Source: Privacy Affairs | Graphic: Georgios Karamanis</span>",
  #          family = "IBM Plex Serif", size = 8, lineheight = 1.1, fill = NA, label.color = NA) +
  theme_void() +
  theme(
    plot.margin = margin(20, 20, 20, 20)
  ) +
  # coord_sf(xlim = c(100, 110), ylim = c(1, 8), expand = FALSE)
  coord_sf(ylim = c(2.5, 5), xlim = c(99, 103), expand = FALSE)
# +
#   ggsave(here::here("2020-week17", "plots", "temp", paste0("gdpr-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320, width = 14, height = 11)
