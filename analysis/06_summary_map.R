# create summary map
library("tidyverse")
library("viridis")
library("sf")
library("raster")
library("ggmap")

#---- set map themes ----

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "#22211d"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.5),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA), 
      panel.background = element_rect(fill = "#ffffff", color = NA), 
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_blank(),
      ...
    )
}

theme_inset <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Arial", color = "#22211d"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA), 
      panel.background = element_rect(fill = "#ffffff", color = NA), 
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_blank(),
      ...
    )
}

#---- read in data ----

df <- read.table("data/pbi_master_image_stats.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = FALSE)

df <- df %>%
  filter(as.numeric(format(as.Date(sowing_date), "%j")) > 180)

bbox <- st_bbox(
  st_as_sf(x = df, 
           coords = c("lon", "lat"),
           crs = "+proj=longlat +datum=WGS84"))

world <- ne_countries(scale='medium', returnclass = 'sf')
india <- subset(world, admin == "India")

image_count <- df %>%
  filter(spatial_unit == "worldclim_5") %>%
  dplyr::group_by(spatial_location) %>%
  dplyr::select(farmer) %>%
  count() %>%
  ungroup()

sowing_dates <- df %>%
  mutate(sowing_date = as.Date(sowing_date)) %>%
  filter(spatial_unit == "sedac") %>%
  group_by(spatial_location) %>%
  summarize(
    sowing_mean = mean(as.numeric(format(sowing_date, "%j")), na.rm = TRUE),
    sowing_spread = max(as.numeric(format(sowing_date, "%j")), na.rm = TRUE) -
      min(as.numeric(format(sowing_date, "%j")), na.rm = TRUE)
    ) %>%
  ungroup()

# download SEDAC data
sedac <- readRDS("data/sedac_indian_villages.rds")
  
# convert to sf object (from sp)
# and select town name feature
villages <- sedac %>%
  dplyr::select(UID) %>%
  rename("spatial_location" = "UID")

sowing_dates <- villages %>%
  right_join(sowing_dates,
             by = "spatial_location")

# WorldClim 5 minute grid (~10 km)
wc <- suppressMessages(raster::getData('worldclim',
                                       var = 'prec',
                                       res = 5,
                                       path = tempdir())$prec1)

cells <- 1:raster::ncell(wc$prec1)

# fill with numeric values (row wise)
wc[] <- cells
wc <- crop(wc, extent(c(bbox["xmin"],
                        bbox["xmax"],
                        bbox["ymin"],
                        bbox["ymax"])))
names(wc) <- "spatial_location"
wc <- rasterToPolygons(wc)
wc <- st_as_sf(wc)

image_count <- wc %>%
  right_join(image_count,
             by = "spatial_location")

#---- download background maps ----

background <- c(bottom = 28.5,
        right = 78,
        left = as.numeric(bbox["xmin"]-0.5),
        top = as.numeric(bbox["ymax"])+0.5)

terrain <- get_stamenmap(
  background,
  zoom = 10,
  maptype = "terrain-background")

#---- format map parts ----

india_inset <- ggplot(data = india) +
  geom_sf(fill = "#22211d",
          lwd = 0) +
  theme_inset() +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  geom_rect(xmin = bbox["xmin"],
            xmax = bbox["xmax"],
            ymin = bbox["ymin"],
            ymax = bbox["ymax"], 
            fill = NA,
            colour = "white",
            size = 0.5)

#---- sowing dates map ---- 

image_count_map <- ggmap(terrain) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = image_count,
          aes(fill = n),
          color = "white",
          size = 0.1,
          inherit.aes = FALSE) +
  labs(x = NULL,
       y = NULL,
       title = "A.", 
       subtitle = "number of images") +
  scale_fill_viridis(
    option = "magma",
    direction = -1,
    name = "# images",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5
    )) +
  theme_map() +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = "#ebebe5", size = 0.8))

#---- image count map ---- 

sowing_dates_map <- ggmap(terrain) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = sowing_dates,
          aes(fill = sowing_spread),
          color = "white",
          size = 0.1,
          inherit.aes = FALSE) +
  labs(x = NULL, 
       y = NULL, 
       title = "B.", 
       subtitle = "difference in self-reported sowing dates") +
  scale_fill_viridis(
    #limits = c(0, 15),
    option = "magma", 
    direction = -1,
    name = "sowing date (day-of-year)",
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 0.5
    )) +
  theme_map() +
  theme(
    legend.position = "bottom",
    axis.title.y = element_blank(),
    axis.text.y = element_text(colour = "white")
    )

sowing_dates_map <- sowing_dates_map +
  annotation_custom(
    grob = ggplotGrob(india_inset),
    xmin = 76.8,
    xmax = 78.8,
    ymin = 30.5,
    ymax = 31.6 
  )

#---- save the map ----

image_count_map | sowing_dates_map

ggsave("manuscript/figures/summary_map.png",
       width = 15,
       height = 7)