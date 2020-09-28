# Anonymize raw data files by aggregating geographic coordinates
# to polygon or grid cell centroids, in addition integrate
# ancillary data about management practices and crop development
# Obviously the source folder (data-raw) is not visible in the github project,
# as are  some other intermediary files

# load libraries
library(tidyverse)
library(pbir)
library(sf)

#--- Anonymize season 1 raw data file by aggregating to various spatial units

# SEDAC village outlines (vector shapes, arbitrary resolution)
sedac <- pbi_anonymize_data(
  df = "./data-raw/rabi_2016_2017/pbi_0001_gcc_data.csv",
  method = "sedac")

# WorldClim 2.5 minute grid (~5 km)
wc_2 <- pbi_anonymize_data(
  df = "./data-raw/rabi_2016_2017/pbi_0001_gcc_data.csv",
  method = "grid",
  resolution = 2.5)

# WorldClim 5 minute grid (~10 km)
wc_5 <- pbi_anonymize_data(
  df = "./data-raw/rabi_2016_2017/pbi_0001_gcc_data.csv",
  method = "grid",
  resolution = 5)

#--- Combine all spatial units aggregations
df <- bind_rows(sedac, wc_2, wc_5)

#--- Add manual generated growth stage labels

# read image labels of crop stages
growth_labels <- read.table("./data-raw/rabi_2016_2017/pbi_growth_labels_2016_2017.csv",
                            header = TRUE,
                            sep = ",",
                            stringsAsFactors = FALSE)

growth_labels <- growth_labels %>%
  mutate_at("growth_stage",
            funs(recode(.,
                        `LPG` = '0',
                        `EVS` = '2',
                        `MVS` = '3',
                        `LVS` = '4',
                        `FRS` = '5',
                        `RMS` = '7',
                        `PHS` = '10',
                        .default = 'NA')))

# exact codes
# LPG: Land Preparation Phase	0
# EVS: Early Vegetative Phase	2
# MVS: Mid Vegetative Phase	3
# LVS: Late Vegetative Phase	4
# FRS: Flowering and Reproductive Phase	5
# RMS: Ripening and Maturity Phase	7
# PHS: post harvest stage 10 (<- non zadok)

# merge data tables
df <- merge(df, growth_labels, by = "report_id", all.x = TRUE)
df$date <- as.Date(df$date)
df$farmerfield <- paste(df$farmer, df$field)

df <- df %>%
  group_by(farmerfield) %>%
  mutate(growth_stage = ifelse(date > mean(df$date[which(df$growth_stage == 7)], na.rm = TRUE) & is.na(growth_stage),
                               "7+",
                               growth_stage))
df <- df %>% ungroup() %>%
  dplyr::select(-c("farmerfield"))

#--- Add manual crop lodging labels

# read image labels of crop stages
lodging_labels <- read.table("./data-raw/rabi_2016_2017/pbi_lodging_labels_2016_2017.csv",
                             header = TRUE,
                             sep = ",",
                             stringsAsFactors = FALSE)

# merge data tables
df <- merge(df, lodging_labels, by = "report_id", all.x = TRUE)

#--- Add seed varieties

# read image labels of crop stages
seed_varieties <- read.table("./data-raw/rabi_2016_2017/pbi_seed_varieties_2016_2017.csv",
                             header = TRUE,
                             sep = ",",
                             stringsAsFactors = FALSE)

# merge data tables
df <- merge(df, seed_varieties, by = "farmer", all.x = TRUE)

#--- Add curated farmer survey data

# read image labels of crop stages
farmer_surveys <- readstata13::read.dta13("./data-raw/rabi_2016_2017/pbi_farmer_surveys_2016_2017.dta")
farmer_surveys <- farmer_surveys %>%
  dplyr::select(reportid, dam_rain, dam_hail, dam_htemp, dam_ltemp, dam_pest, dam_lodge,
                dam_wild, dam_fire, dam_other, did_harvest, did_irrigate,
                did_till, did_sow, did_weed, urea_kg_WCquest, dap_kg_WCquest,
                potash_kg_WCquest, zinc_kg_WCquest, app_fung_WCquest,
                app_herb_WCquest, app_pest_WCquest, app_other_WCquest)

colnames(farmer_surveys) <- c(
  "report_id",
  paste("dam", c("rain", "hail", "high_temp","low_temp",
                 "pest", "lodging","wildlife","fire","unclassified"), sep = "_"),
  paste("man", c("harvest","irrigate","till","sow","weed","urea_kg_acre","dap_kg_acre",
                 "potash_kg_acre","zinc_kg_acre","fungicide_kg_acre",
                 "herbicide_kg_acre","pesticide_kg_acre",
                 "unclassified"), sep = "_")
)

# merge data tables
df <- merge(df, farmer_surveys, by = "report_id", all.x = TRUE)

#--- Write raw data files to disk
write.table(df, "data-raw/pbi_0001_image_stats.csv",
            col.names = TRUE,
            quote = FALSE,
            row.names = FALSE,
            sep = ",")


#--- Anonymize season 2 raw data by aggregating to various spatial units
rm(list = ls())

# SEDAC village outlines (vector shapes, arbitrary resolution)
sedac <- pbi_anonymize_data(
  df = "./data-raw/rabi_2017_2018/pbi_0002_gcc_data.csv",
  method = "sedac",
  country = "IND")

# WorldClim 2.5 minute grid (~5 km)
wc_2 <- pbi_anonymize_data(
  df = "./data-raw/rabi_2017_2018/pbi_0002_gcc_data.csv",
  method = "grid",
  resolution = 2.5)

# WorldClim 5 minute grid (~10 km)
wc_5 <- pbi_anonymize_data(
  df = "./data-raw/rabi_2017_2018/pbi_0002_gcc_data.csv",
  method = "grid",
  resolution = 5)

#--- Combine all spatial units aggregations
df <- bind_rows(sedac, wc_2, wc_5)

#--- Read meta-data
meta_data <- readstata13::read.dta13("data-raw/rabi_2017_2018/pbi_rabi_2017_2018_database.dta")
meta_data$report_id <- meta_data$picid

# rename variables
meta_data <- meta_data %>% 
  rename('soil_type' = 'soiltype',
         'drainage' = 'drainagetype',
         'sowing_date' = 'sowingdate',
         'harvest_quantity' = 'harvestqty',
         'yield_expectation' = 'yieldexpectation',
         'growth_stage' = 'growthstage',
         'dam_unclassified' = 'damagedcrop')

# reclass variables
meta_data <- meta_data %>%
  mutate_at("soil_type",
            funs(recode(.,
                        `1` = 'sandy',
                        `2` = 'loam sandy',
                        `3` = 'loam',
                        `4` = 'clay',
                        `5` = 'clay loam',
                        `6` = 'problematic (saline/alkaline)',
                        `7` = 'other',
                        .default = 'NA')))

meta_data <- meta_data %>%
  mutate_at("drainage",
            funs(recode(.,
                        `1` = 'good',
                        `2` = 'poor',
                        .default = 'NA')))

meta_data <- meta_data %>%
  mutate_at("growth_stage",
            funs(recode(.,
                        `1` = '1',
                        `2` = '2',
                        `3` = '4',
                        `4` = '5',
                        `5` = '6',
                        `6` = '7',
                        .default = 'NA')))

# Exact names in survey + zadok scale:
# THIS IS SELF REPORTED??
# 1 - Crown root (cor dena)	10
# 2 - Tillering (kalle futna)	20
# 3 - Booting (gabha aana)	40
# 4 - Heading (bali nikalna)	50
# 5 - Anthesis (flowering)	60
# 6 - Milking (dudh padna)	70

# trim meta-data
meta_data <- meta_data %>%
  dplyr::select("report_id",
                "soil_type",
                "drainage",
                "growth_stage",
                "dam_unclassified",
                "sowing_date",
                "harvest_quantity",
                "yield_expectation")

# merge meta_data and gcc data
df <- merge(df, meta_data, by = "report_id", all.x = TRUE)

#--- Write raw data files to disk
write.table(df, "data-raw/pbi_0002_image_stats.csv",
            col.names = TRUE,
            quote = FALSE,
            row.names = FALSE,
            sep = ",")
