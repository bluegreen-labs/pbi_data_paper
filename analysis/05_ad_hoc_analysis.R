# informative statistics

# read in master data file
df <- read.table("data/pbi_master_image_stats.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = FALSE)

# reporting
message("unique farmers:")
message(length(unique(df$farmer)))
message("average by village:")
df %>%
  filter(spatial_unit == "sedac") %>%
  group_by(spatial_location) %>%
  summarize(farmers = length(unique(farmer))) %>%
  summarize(mean_farmer = mean(farmers),
            sd_farmer = sd(farmers)) %>%
  print()

# calculate global summary statistics across the whole
# dataset, split by season
global_summary <- df %>%
  filter(spatial_unit == "sedac") %>%
  mutate(farmer_field = paste(farmer, field)) %>%
  group_by(farmer_field, project_id) %>%
  
  # split out single values
  summarize(
    nr_values = nr_values[1],
    sowing_date = sowing_date[1],
    rising = rising[1]
  ) %>%
  group_by(project_id ) %>%
  # take the mean across all fields
  summarize(
    mean_nr_values = mean(nr_values, na.rm = TRUE),
    total_nr_values = sum(nr_values, na.rm = TRUE),
    sd_nr_values = sd(nr_values, na.rm = TRUE),
    mean_sowing_date = mean(as.Date(sowing_date), na.rm = TRUE),
    sd_sowing_date = sd(as.Date(sowing_date), na.rm = TRUE),
    mean_rising_date = mean(as.Date(rising), na.rm = TRUE),
    sd_rising_date = sd(as.Date(rising), na.rm = TRUE)
  )
print(global_summary)

# summary data on irrigation
df <- read.table("data/pbi_summary_stats.csv",
                 header = TRUE,
                 sep = ",",
                 stringsAsFactors = FALSE)

df %>% filter(spatial_unit == "sedac") %>%
  group_by(project_id) %>%
  summarize(mean_ratio = mean(nr_fields_irrigated/nr_fields),
            sd_ratio = sd(nr_fields_irrigated/nr_fields)) %>%
  print()