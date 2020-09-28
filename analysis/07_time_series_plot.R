# plot time series for publication

# load libraries
library(tidyverse)

# some example maps
data <- read.table("data/pbi_summary_time_series.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = FALSE)

summary_data <- read.table("data/pbi_summary_stats.csv",
                           header = TRUE,
                           sep = ",",
                           stringsAsFactors = FALSE)

# join meta-data
data <- full_join(data, summary_data)

# filter data, only showing the first
# season with locations with more than
# 150 values (~ AFM paper selection)
data_1 <- data %>%
  filter(total_nr_values > 150,
         project_id == 1) %>%
  mutate(spatial_unit = as.factor(spatial_unit))

levels(data_1$spatial_unit) <- c("SEDAC", "WorldClim 2.5", "WorldClim 5")

# panel plot setup
p <- ggplot(data_1) +
  geom_line(aes(x = as.Date(date),
                 y = smooth_gcc_90,
                 group = spatial_location),
             colour = rgb(0,0,0,0.3)) +
  geom_point(data = data_1,
             aes(x = as.Date(rising), y = 0.73),
             col = "black",
             pch = 3) +
  geom_point(data = data_1,
             aes(x = as.Date(falling), y = 0.83),
             col = "black",
             pch = 15) +
  facet_wrap( ~ spatial_unit) +
  xlab("Month") +
  ylab("Smoothed normalized Gcc") +
  theme_classic() +
  theme(
    strip.text = element_text(
      size = 12,
      color = "white"
    ),
    strip.background = element_rect(
      color = "white",
      fill = "grey50",
      size=1.5, linetype="solid"
    ),
    legend.position = "bottom",
    plot.title = element_text(
      family = "Merriweather", color="grey10"),
    plot.caption = element_text(color="grey50"),
    text = element_text(
      family = "Merriweather", color = "grey30"),
    panel.grid = element_line(colour = "grey96"),
    panel.grid.major.y = element_line(colour = "grey96")
  )

# save data
ggsave("manuscript/figures/summary_time_series.png", width = 11, height = 3)

# filter data, only showing the first
# season with locations with more than
# 150 values (~ AFM paper selection)
data_2 <- data %>%
  filter(total_nr_values > 150) %>%
  mutate(
    spatial_unit = as.factor(spatial_unit),
    doy = ifelse(project_id == 1,
                  as.numeric(as.Date(date) - as.Date("2017-01-01")),
                 as.numeric(as.Date(date) - as.Date("2018-01-01"))
         ),
    project_id = as.factor(project_id)
    ) %>%
  group_by(project_id, spatial_unit, doy) %>%
  summarize(
    gcc_90 = mean(smooth_gcc_90, na.rm = TRUE)
  )

levels(data_2$spatial_unit) <- c("SEDAC", "WorldClim 2.5", "WorldClim 5")

# panel plot setup
p <- ggplot(data_2) +
  geom_path(aes(
    x = doy,
    y = gcc_90,
    linetype = project_id
    )
    ) +
  facet_wrap( ~ spatial_unit) +
  xlab(expression("Days from January 1" ^ "th")) +
  ylab("mean smoothed normalized Gcc") +
  guides(
    linetype = guide_legend(title="Season")
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(
      size = 12,
      color = "white"
    ),
    strip.background = element_rect(
      color = "white",
      fill = "grey50",
      size=1.5, linetype="solid"
    ),
    legend.position = "right",
    plot.title = element_text(
      family = "Merriweather", color="grey10"),
    plot.caption = element_text(color="grey50"),
    text = element_text(
      family = "Merriweather", color = "grey30"),
    panel.grid = element_line(colour = "grey96"),
    panel.grid.major.y = element_line(colour = "grey96")
  )

# save data
ggsave("manuscript/figures/summary_time_series_2.png", width = 11, height = 3)


