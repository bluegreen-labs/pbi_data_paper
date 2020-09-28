#' Summary stats mapping function
#'
#' @param data cgiar big data summary file, subset by season
#' @param spatial_unit which spatial unit to use (default = worldclim_2.5)
#' @param value which value to plot
#' @param inset plot an inset of India on the graph
#' @param background location of a background raster (DEM)
#' @param title plot title
#' @param subtitle plot subtitle
#' @param legend plot legend
#' @param filename filename where to export the resulting png
#'
#' @return a plot of pbi data
#' @export

map_summary_values <- function(
  data,
  spatial_unit = "worldclim_2.5",
  value = "nr_fields",
  inset = TRUE,
  background,
  title = "title",
  subtitle = "subtitle",
  legend = "legend",
  filename
){
  
  # load libraries (to be sure)
  library("tidyverse", quietly = TRUE)
  library("viridis", quietly = TRUE)
  library("sf", quietly = TRUE)
  library("raster", quietly = TRUE)
  library("leaflet", quietly = TRUE)
  library("rnaturalearth", quietly = TRUE)
  library("rnaturalearthdata", quietly = TRUE)
  
  # Define map theme
  theme_map <- function(...) {
    theme_minimal() +
      theme(
        text = element_text(family = "Arial", color = "#22211d"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = "#ebebe5", size = 0.1),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#ffffff", color = NA), 
        panel.background = element_rect(fill = "#ffffff", color = NA), 
        legend.background = element_rect(fill = "#ffffff", color = NA),
        panel.border = element_blank(),
        ...
      )
  }
  
  world <- ne_countries(scale='medium', returnclass = 'sf')
  india <- subset(world, admin == "India")
  
  df <- data %>%
    dplyr::filter(spatial_unit == !!spatial_unit)
  
  bbox <- st_bbox(
    st_as_sf(x = df, 
             coords = c("lon", "lat"),
             crs = "+proj=longlat +datum=WGS84"))
  
  if(!missing(background)){
    r <- raster(background)
    
    r <- crop(r, extent(c(74,78,29,31.5)))
    
    shaded_relief <- as.data.frame(as(r,
                                      "SpatialPixelsDataFrame"))
    colnames(shaded_relief) <- c("value", "x", "y")
  }
  
  # grab grid and match output with polygon names
  if(spatial_unit == "worldclim_2.5"){
    wc <- suppressMessages(raster::getData('worldclim',
                                           var = 'prec',
                                           res = 2.5,
                                           path = tempdir())$prec1)
  }
  
  if(spatial_unit == "worldclim_5"){
    wc <- suppressMessages(raster::getData('worldclim',
                                           var = 'prec',
                                           res = 5,
                                           path = tempdir())$prec1)
  }
  
  if(spatial_unit != "gadm"){
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
  }
  
  if(spatial_unit == "gadm"){
    # download GADM data
    gadm <- raster::getData('GADM',
                            country = 'IND',
                            level=3,
                            path = tempdir())
    
    # convert to sf object (from sp)
    # and select town name feature
    wc <- st_as_sf(gadm) %>%
      dplyr::select(NAME_3) %>%
      rename("spatial_location" = "NAME_3")
    
    wc <- st_crop(wc, c(bbox["xmin"] - 0.5,
                        bbox["xmax"] + 0.5,
                        bbox["ymin"] - 0.5,
                        bbox["ymax"] + 0.5))
  }
  
  if(spatial_unit != "gadm"){
    df$spatial_location <- as.numeric(df$spatial_location)
  }
  
  wc_sf <- wc %>%
    right_join(df,
               by = "spatial_location")
  
  # plotting
  p <- ggplot()
  
  if(!missing(background)){
    p <- p + 
      geom_raster(data=shaded_relief,
                  aes(x=x,
                      y=y,
                      alpha = value),
                  interpolate = TRUE) +
      scale_alpha(name = "", range = c(0.6, 0), guide = F)
  }
  
  p <- p +
    geom_sf(data = wc_sf,
            aes_string(fill = value),
            color = "white",
            size = 0.1
    ) +
    scale_fill_viridis(
      option = "magma", 
      direction = -1,
      name = legend,
      guide = guide_colorbar(
        direction = "horizontal",
        barheight = unit(2, units = "mm"),
        barwidth = unit(50, units = "mm"),
        draw.ulim = F,
        title.position = 'top',
        title.hjust = 0.5,
        label.hjust = 0.5
      )) +
    labs(x = NULL, 
         y = NULL, 
         title = title, 
         subtitle = subtitle) +
    xlim(c(74,78)) +
    ylim(c(29,31.5)) +
    theme_map() +
    theme(legend.position = "bottom",
          panel.grid.major = element_line(color = "#ebebe5", size = 0.2))
  
  if(inset){
    india_inset <- ggplot(data = india) +
      geom_sf(fill = "grey50",
              lwd = 0) +
      theme_map() +
      theme(legend.position = "none",
            plot.background = element_blank(),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) +
      geom_rect(xmin = bbox["xmin"],
                xmax = bbox["xmax"],
                ymin = bbox["ymin"],
                ymax = bbox["ymax"], 
                fill = NA,
                colour = "white",
                size = 0.5)
    
    p +
      annotation_custom(
        grob = ggplotGrob(india_inset),
        xmin = 74.2,
        xmax = 75.1,
        ymin = 30.5,
        ymax = 31.4 
      )
  }
  
  if(!missing(filename)){
    ggsave(filename, width = 9, height = 7)
  } else {
    return(p)
  }
}