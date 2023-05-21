
library(tidyverse)
library(patchwork)
library(terra)

# load taxon-trait matrix

traits = read_csv("data/230403_summarised_austraits_traits.csv")


gbif_path <- "data/filtered_aus_obs_28feb2023_limited_cols.csv"

# downlaod data if needed
if(!file.exists(gbif_path)) {
  
  message("Downloading first 100000 rows. Download full dataset from https://www.dropbox.com/s/byqor7tke9b19iy/filtered_aus_obs_28feb2023_limited_cols.csv?dl=1 and save in folder `downloads`")
  
  read_csv("https://www.dropbox.com/s/byqor7tke9b19iy/filtered_aus_obs_28feb2023_limited_cols.csv?dl=1",  n_max=15000000) %>% write_csv(gbif_path)
}

gbif <- read_csv(gbif_path, n_max=15000000)

vars <- c("_1.tif", "_12.tif")

# Australia
australia <- terra::rast("data/australia.tif")


# Stacked raster of climate predictors, cropped to Australia
stacked_rasters <- 
  #list.files(wc_path, full.names = TRUE, pattern = ".bil") %>%
  paste0("data/wc2.1_2.5m_bio", vars) %>%
  terra::rast() %>%
  terra::crop(australia) 

# Resample if needed 
#  terra::resample(...)

# Takes a while....
occurrence_data_vect <- 
  terra::vect(
    gbif, 
    geom = c("decimalLongitude","decimalLatitude"), 
    crs = terra::crs(stacked_rasters)
  )

# Add on climate data
# Also takes a while..
gbif_climate <- 
  terra::extract(x = stacked_rasters,  y = occurrence_data_vect, method = "simple", bind = TRUE, xy = T, cells = T, ID = FALSE) 

# 
com = cbind(gbif, gbif_climate)

# only take taxa in the matrix created from Austraits data
# You should only lose about 5% of the occurence observations without fuzzy matching etc
com = com %>% filter(species %in% unique(traits$taxon_name))

com = com %>% select(species, cell, wc2.1_2.5m_bio_1, wc2.1_2.5m_bio_12) %>% unique()

# Take the average climate in the cells that the species occurs to reflect the native range
out = com %>% group_by(species) %>% summarise(MAT = mean(wc2.1_2.5m_bio_1, na.rm = T), MAP = mean(wc2.1_2.5m_bio_12, na.rm = T), n_cells = n()) %>% arrange(species)

out = left_join(traits, out, by = c("taxon_name"= "species"))

# 
write.csv(out, "data/climate_vars.csv", row.names = F)

########################

### Note, while Ausflora is down, we may get fewer climate variables. Missing about 4, 500


