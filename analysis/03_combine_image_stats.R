# Combine the processed and anonymized data and
# provide quality control and phenology statistics
# on a field by field basis.

# load libraries
library(tidyverse)
library(pbir)

# read data
df1 <- read.table("data-raw/pbi_0001_image_stats.csv",
                  sep = ",",
                  header = TRUE,
                  stringsAsFactors = FALSE)
df1$report_id <- as.character(df1$report_id)

df2 <- read.table("data-raw/pbi_0002_image_stats.csv",
                  sep = ",",
                  header = TRUE,
                  stringsAsFactors = FALSE)
df2$growth_stage <- as.character(df2$growth_stage)

# do a column merge
df <- bind_rows(df1, df2)

# calculate a QA / QC metrics & smooth time series
df <- df %>%
  dplyr::select(-filename) %>%
  mutate(farmer_field = paste(farmer, field)) %>%
  group_by(spatial_unit, farmer_field) %>%
  mutate(duration = as.numeric(as.Date(max(date)) - as.Date(min(date))),
         nr_values = length(gcc_90),
         spread = mean(as.numeric(diff(sort(as.Date(date)))),
                       na.rm = TRUE),
         qa = duration/spread,
         smooth_gcc_90 = 
           smooth_ts(x = as.numeric(as.Date(date)),
                     y = scales::rescale(gcc_90, c(0,1))))

# calculate phenophases (phenology metrics)
field_phenology <-  df %>%
  group_by(farmer_field) %>%
  do(phenophases(
    date = as.numeric(as.Date(.$date)),
    value = .$gcc_90,
    long_format = FALSE)
  )

# join phenology data with original data
df <- full_join(df, field_phenology)

# remove the unique farmer / field column
df <- df %>%
  ungroup() %>%
  dplyr::select(-farmer_field)

# write data to file
# this will be the final master data file
# to work from
write.table(df,
            "data/pbi_master_image_stats.csv",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE,
            sep = ",")