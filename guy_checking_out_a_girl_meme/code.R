# load libraries
library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(patchwork)

# read in data - manually created
data <-
  read_csv("guy_checking_out_a_girl_meme/data.csv") %>% 
  mutate(
    paint = fct_relevel(paint, c("black", "brown", "beige", "white", "black_2", "grey_2",  "red", "blue_2", "beige_3", "blue", "beige_2", "grey"))
    
  )

# start ggplot
p1 <- 
  ggplot(data) +
  # using geom_bar, so didnt cheat (though using geom_tile was much easier)
  geom_bar(
    mapping = aes(x = x, fill = paint),
    width = 1
  ) +
  # set colours for bars
  scale_fill_manual(
    values = c("#24211a", "#593326","#e4a095", "#ffffff", "#24211a", "#93a9b6", "#fa0107", "#0b59b3", "#e4a095", "#0b59b3", "#e4a095","#93a9b6")
  ) +
  # make grid squares
  coord_equal() +
  # add themes
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text  = element_blank(),
    panel.grid = element_blank()
  )
  
# customized legend - create data to be plotted to look like a legend (not recommended)
legend_data <-
  data.frame(
    x = 0,
    y = c(4, 6, 8, 10, 12),
    label = c("Making ggplot\nwhen Hadley\nasks", "", "Me", "", "Doing useful\ndata analysis")
  )

# start ggplot for custom legend
p2 <- 
  ggplot(
    data = legend_data,
    mapping = aes(x = x, y = y)
  ) +
  # using geom_tile to create filled boxes
  geom_tile(
    mapping = aes( fill = label),
    width = 0.4, height = 0.4,
  ) + 
  # using geom_text to place text beside tiles
  geom_text(
    mapping = aes(label = label),
    size = 6,
    nudge_x = 0.4,
    nudge_y = 0,
    hjust = 0,
    vjust = 0.5
  ) + 
  scale_fill_manual(
    values = c("#ffffff", "#93a9b6", "#fa0107", "#0b59b3")
  ) +
  # setting scales to look better/ coord_fixed to make plot narrower
  scale_x_continuous(limits = c(-0.5,3)) +
  scale_y_continuous(limits = c(1, 15)) +
  coord_fixed(ratio = 0.8) +
  # add themes
  theme_void() +
  theme(
    legend.position = "none"
  )

# using patchwork to combine main plot and custom legend
p <- p1 + p2
p

# save plot
ggsave(filename = "guy_checking_out_a_girl_meme/plot.png", plot = p, width = 16, height = 9)




# R version 3.6.3 (2020-02-29)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 7 x64 (build 7601) Service Pack 1
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_Australia.1252  LC_CTYPE=English_Australia.1252    LC_MONETARY=English_Australia.1252 LC_NUMERIC=C                      
# [5] LC_TIME=English_Australia.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] patchwork_1.0.0 ggplot2_3.3.0   forcats_0.5.0   dplyr_0.8.5     readr_1.3.1    
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.4.6     rstudioapi_0.11  magrittr_1.5     hms_0.5.3        tidyselect_1.0.0 munsell_0.5.0    colorspace_1.4-1 R6_2.4.1        
# [9] rlang_0.4.5      fansi_0.4.1      tools_3.6.3      grid_3.6.3       packrat_0.5.0    gtable_0.3.0     cli_2.0.2        withr_2.2.0     
# [17] ellipsis_0.3.0   digest_0.6.25    assertthat_0.2.1 tibble_3.0.0     lifecycle_0.2.0  crayon_1.3.4     farver_2.0.3     purrr_0.3.4     
# [25] vctrs_0.2.4      glue_1.4.0       labeling_0.3     compiler_3.6.3   pillar_1.4.3     scales_1.1.0     pkgconfig_2.0.


