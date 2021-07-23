# https://docs.google.com/document/d/1kAXh00E4blsHRWw5VyhxeKtbq1kEzCxqChO12qoPNZ0/edit#heading=h.5cd1aedbdpsg

set.seed(123)
library(sp)
library(bohemia)
library(tidyverse)
library(yaml)
library(databrew)
library(leaflet)
library(kableExtra)
library(ggplot2)
theme_set(theme_simple())

# Read in objects created in monte_carlo.R
# (simulation number 9 is the final one)
cluster_level <- read_csv('cluster_level.csv')
hamlet_level <- read_csv('hamlet_level.csv')
hh_level <- read_csv('hh_level.csv')
status_level <- read_csv('status_level.csv')
load('image.RData')
load('../data.RData')


# Get the subset spatial objects
if(!'final_spatial_data.RData' %in% dir()){
  set.seed(123)
  master_poly <- master_poly[master_poly@data$sim == 9 &
                               master_poly@data$iter_buffer_distance == 400 &
                               master_poly@data$iter_n_children == 20,]
  master_buf <- master_buf[master_buf@data$sim == 9 &
                             master_buf@data$iter_buffer_distance == 400 &
                             master_buf@data$iter_n_children == 20,]
  master_hull <- master_hull[master_hull@data$sim == 9 &
                               master_hull@data$iter_buffer_distance == 400 &
                               master_hull@data$iter_n_children == 20,]
  save(master_poly,
       master_buf,
       master_hull,
       file = 'final_spatial_data.RData')
} else {
  load('final_spatial_data.RData')
}

# Create arms (just once)
if(!'arm_assignments.csv' %in% dir()){
  set.seed(123)
  arm_assignments <- tibble(cluster = cluster_level$cluster)
  assig_helper <- rep(1:3, each = 53)
  assig_helper <- sample(assig_helper, length(assig_helper))
  arm_assignments$assignment <- assig_helper
  write_csv(arm_assignments, 'arm_assignments.csv')
} else {
  arm_assignments <- read_csv('arm_assignments.csv')
}

# Use the ento deliverables document to generate sentinel clusters
if(!'ento_sentinel_clusters.csv' %in% dir()){
  set.seed(123)
  out_list <- list()
  for(i in 1:3){
    these <- sample(arm_assignments$cluster[arm_assignments$assignment == i], size = 5)
    out <- tibble(arm = i,
                  cluster = these,
                  type = 'Sentinel cluster')
    out_list[[i]] <- out
  }
  ento_sentinel_clusters <- bind_rows(out_list) %>%
    arrange(arm, cluster)
  # Merge with neighborhood information
  ento_sentinel_clusters <- left_join(ento_sentinel_clusters,
                                      cluster_level %>% dplyr::select(cluster, hamlets))
  write_csv(ento_sentinel_clusters, 'ento_sentinel_clusters.csv')
} else {
  ento_sentinel_clusters <- read_csv('ento_sentinel_clusters.csv')
}

# Sentinel CDC light trap
# From each of the 15 sentinel clusters, sample 4 households (ie, 60 total households), plus 2 "backup" households, clearly indicated as such (ie, 90 total households).
# Deliverable 1: A table in which each row is one qualified non-refusing household, with columns indicating household ID, cluster, treatment arm, randomization number, randomization activity = "Sentinel CDC light trap", backup, Household head, list of members.
if('sentinel_cdc_light_trap.csv' %in% dir()){
  sentinel_cdc_light_trap <- read_csv('sentinel_cdc_light_trap.csv')
  sentinel_cdc_light_trap_only_6 <- read_csv('sentinel_cdc_light_trap_only_6.csv')
  
} else {
  out_list <- list()
  set.seed(123)
  for(i in 1:nrow(ento_sentinel_clusters)){
    this_cluster <- ento_sentinel_clusters[i,]
    this_cluster_number <- this_cluster$cluster
    these_hh <- master_poly[master_poly@data$cluster == this_cluster_number,]
    # randomly sample 6 households
    sample_index <- sample(1:nrow(these_hh), nrow(these_hh)) # doing all instead of just 6
    sample_hh <- these_hh[sample_index,]
    sample_hh$sample_number <- 1:nrow(these_hh)
    sample_hh$sample_type <- ifelse(sample_hh$sample_number %in% 1:4,
                                    'Sentinel CDC light trap household',
                                    'Sentinel CDC light trap backup')
    sample_hh <- sample_hh@data
    sample_hh <- sample_hh %>%
      dplyr::select(cluster, randomization_number = sample_number,
                    sample_type, lng, lat, n_members, n_adults, instance_id)
    out_list[[i]] <- sample_hh
  }
  sample_hh <- bind_rows(out_list)
  # Join with census data
  census <- pd_moz$minicensus_main
  people <- pd_moz$minicensus_people
  sample_hh <- left_join(sample_hh,
                         census %>% dplyr::select(instance_id, hh_head_id, hh_id))
  sample_hh <- left_join(sample_hh,
                         people %>% 
                           mutate(num = as.character(num)) %>%
                           dplyr::select(instance_id,
                                         hh_head_id = num,
                                         first_name,
                                         last_name))
  sample_hh <- sample_hh %>% dplyr::select(-hh_head_id,
                                           -instance_id)
  sentinel_cdc_light_trap <- sample_hh
  sentinel_cdc_light_trap <- left_join(
    sentinel_cdc_light_trap,
    arm_assignments
  )
  sentinel_cdc_light_trap_only_6 <- sentinel_cdc_light_trap %>%
    filter(randomization_number <= 6)
  write_csv(sentinel_cdc_light_trap, 'sentinel_cdc_light_trap.csv')
  write_csv(sentinel_cdc_light_trap_only_6, 'sentinel_cdc_light_trap_only_6.csv')
}


# Random CDC Light trap households
# (not to be confused with sentinel CDC light trap households)
# -For each treatment arm, randomly sample 3 clusters (ie, 9 total clusters), but do not sample any of the sentinel clusters.
# From each sampled cluster, randomly sample 2 qualified non-refusing households (ie, 18 total households) plus 2 backups (ie, 36 total households) clearly indicated as such.
# Repeat the above 15 times, so that there is a randomization for each of the 15 study months.
# Deliverable: A table in which each row is one household-month combination, with columns indicating month_number, household ID, cluster, treatment arm, randomization number, randomization activity = "Random CDC Light trap households", backup.
if('random_cdc_light_trap_households.csv' %in% dir()){
  random_cdc_light_trap_households <- read_csv('random_cdc_light_trap_households.csv')
} else {
  set.seed(123)
  
  # From each sampled cluster, randomly sample 2 qualified non-refusing households (ie, 18 total households) plus 2 backups (ie, 36 total households) clearly indicated as such.
  arms <- 1:3; out_list <- list(); counter <- 0; random_cdc_light_trap_clusters_list <- list(); 
  for(month_number in 1:15){
    
    # # -For each treatment arm, randomly sample 3 clusters (ie, 9 total clusters), but do not sample any households from the sentinel clusters.
    # Get eligible clusters
    this_month_list <- list()
    month_counter <- 0
    for(a in arms){
      month_counter <- month_counter + 1
      this_month_list[[month_counter]] <- arm_assignments %>%
        filter(assignment == a) %>%
        filter(!cluster %in% ento_sentinel_clusters$cluster) %>%
        sample_n(3)
    }
    random_cdc_light_trap_clusters <- bind_rows(this_month_list)
    random_cdc_light_trap_clusters$month_number <- month_number
    random_cdc_light_trap_clusters_list[[month_number]] <- random_cdc_light_trap_clusters
    
    for(i in 1:nrow(random_cdc_light_trap_clusters)){
      counter <- counter + 1
      this_cluster <- random_cdc_light_trap_clusters[i,]
      this_cluster_number <- this_cluster$cluster
      these_hh <- master_poly[master_poly@data$cluster == this_cluster_number,]
      these_hh <- these_hh[these_hh$instance_id %in% pd_moz$minicensus_main$instance_id,]
      # randomly sample 2+2 households
      sample_index <- sample(1:nrow(these_hh), nrow(these_hh)) # doing all instead of just 4
      sample_hh <- these_hh[sample_index,]
      sample_hh$sample_number <- 1:nrow(these_hh)
      sample_hh$sample_type <- ifelse(sample_hh$sample_number %in% 1:2,
                                      'Random CDC light trap household',
                                      'Random CDC light trap backup')
      sample_hh <- sample_hh@data
      sample_hh <- sample_hh %>%
        dplyr::select(cluster, instance_id, randomization_number = sample_number,
                      sample_type, lng, lat)
      sample_hh$month_number <- month_number
      out_list[[counter]] <- sample_hh
    }
  }
  random_cdc_light_trap_clusters <- bind_rows(random_cdc_light_trap_clusters_list)
  write_csv(random_cdc_light_trap_clusters, 'random_cdc_light_trap_clusters.csv')
  
  sample_hh <- bind_rows(out_list)
  sample_hh <- left_join(
    sample_hh,
      pd_moz$minicensus_main %>% dplyr::select(instance_id, hh_id) 
    ) %>% dplyr::select(-instance_id)
  
  
  random_cdc_light_trap_households <- sample_hh
  random_cdc_light_trap_households <- left_join(
    random_cdc_light_trap_households,
    arm_assignments
  )
  random_cdc_light_trap_households_only_4 <- random_cdc_light_trap_households %>%
    filter(randomization_number <= 4)
  write_csv(random_cdc_light_trap_households, 'random_cdc_light_trap_households.csv')
  write_csv(random_cdc_light_trap_households_only_4, 'random_cdc_light_trap_households_only_4.csv')
}

make_map <- FALSE
if(make_map){
  # Show the sentinenl clusters
  xx <- spTransform(master_hull, proj4string(bohemia::mop2))
  yy <- spTransform(master_buf, proj4string(bohemia::mop2))
  
  
  l <- leaflet() %>%
    addProviderTiles(providers$Stamen.Toner)
  
  cols <- c('red', 'blue', 'green')
  for(i in 1:3){
    this <- ento_sentinel_clusters %>%
      filter(arm == i)
    z <- xx[xx$cluster %in% this$cluster,]
    zz <- yy[yy$cluster %in% this$cluster,]
    l <- l %>%
      addPolygons(data = z,
                  color = cols[i],
                  popup = paste0('Cluster: ', z@data$cluster, '. Assignment arm: ', i)) %>%
      addPolylines(data = zz,
                   color = cols[i])
  }
  l
  library(htmltools)
  save_html(html = l, file = '~/Desktop/sentinelclustersmoz.html')
}

# Make geographic files for use in maps.me
if(!dir.exists('geographic_files')){
  dir.create('geographic_files')
}

# Define function for extracting geolocation
extract_ll <- function(x){
  splat <- strsplit(x, ' ')
  lat <- as.numeric(unlist(lapply(splat, function(z){z[1]})))
  lng <- as.numeric(unlist(lapply(splat, function(z){z[2]})))
  tibble(lng, lat)
}

# BODIES OF WATER
# 1 point for every household reporting any body of water, as per minicensus. 21 and 21a
water <- pd_moz$minicensus_repeat_water %>%
  left_join(pd_moz$minicensus_main %>% 
              dplyr::select(instance_id, water_bodies_how_many, wid, hh_id,
                            hh_geo_location)) %>%
  mutate(hamlet_code = substr(hh_id, 1, 3))
locs <- extract_ll(water$hh_geo_location)
water <- bind_cols(water, locs)
water$description <- water$water_bodies_type

# LIVESTOCK ENCLSORUES
# 1 point for every household reporting a livestock enclosure, as per minicensus. questions 19a and 19b in minicensus
livestock <- pd_moz$minicensus_main %>%
  mutate(hamlet_code = substr(hh_id, 1, 3)) %>%
  dplyr::select(instance_id,
                hamlet_code,
                wid,
                hh_id,
                hh_geo_location,
                # Cattle
                hh_animals_distance_cattle_dry_season,
                hh_animals_distance_cattle_rainy_season,
                hh_animals_where_cattle_dry_season,
                hh_animals_where_cattle_rainy_season,
                # Pigs
                hh_animals_dry_season_distance_pigs, 
                hh_animals_rainy_season_distance_pigs,
                hh_animals_rainy_season_pigs,
                hh_animals_dry_season_pigs)
locs <- extract_ll(livestock$hh_geo_location)
livestock <- bind_cols(livestock, locs)
livestock <- livestock %>%
  filter(!is.na(hh_animals_rainy_season_pigs) |
           !is.na(hh_animals_dry_season_pigs) |
           !is.na(hh_animals_where_cattle_dry_season) |
           !is.na(hh_animals_where_cattle_rainy_season)) %>%
  mutate(description = 
           paste0(
             ifelse(!is.na(hh_animals_dry_season_pigs),
                    paste0('Pigs in dry season: ', hh_animals_dry_season_pigs, '. '),
                    ''),
             ifelse(!is.na(hh_animals_rainy_season_pigs),
                           paste0('Pigs in rainy season: ', hh_animals_dry_season_pigs, '. '),
                           ''),
             ifelse(!is.na(hh_animals_where_cattle_rainy_season),
                    paste0('Cattle in rainy season: ', hh_animals_where_cattle_rainy_season, '. '),
                    ''),
             ifelse(!is.na(hh_animals_where_cattle_dry_season),
                    paste0('Cattle in dry season: ', hh_animals_where_cattle_dry_season, '. '),
                    '')
           ))

# Combine the livestock and water
livestock <- livestock %>%
  dplyr::select(hh_id,
                description,
                hamlet_code,
                wid,
                lng,
                lat) %>%
  mutate(type = 'Livestock enclosure')
water <- water %>%
  dplyr::select(description,
                hamlet_code,
                wid,
                hh_id,
                lng,
                lat) %>%
  mutate(type = 'Body of water')
households <- 
  sentinel_cdc_light_trap_only_6 %>%
  mutate(month_number = 'not applicable (sentinel)') %>%
  dplyr::select(hh_id,
                lng,
                lat,
                month_number,
                randomization_number) %>%
  mutate(type = 'Household: Sentinel CDC light trap') %>%
  bind_rows(
    random_cdc_light_trap_households_only_4 %>%
      mutate(month_number = as.character(month_number)) %>%
      dplyr::select(hh_id,
                    lng,
                    lat,
                    month_number,
                    randomization_number) %>%
      mutate(type = 'Household: Random CDC light trap') 
  )
# Write locations files
write_kml <- function(df, file_path,
                      layer){
  require(sp)
  require(rgdal)
  df <- df %>% filter(!is.na(lng),
                      !is.na(lat))
  coordinates(df) <- ~lng+lat
  proj4string(df) <- CRS("+proj=longlat +datum=WGS84")
  writeOGR(df, file_path, layer=layer, driver="KML") 
}
# write_kml(df = livestock,
#           file_path = 'geographic_files/livestock.kml',
#           layer = 'livestock')
write_csv(livestock, 'geographic_files/livestock.csv')
write_csv(households, 'geographic_files/households.csv')
write_csv(water, 'geographic_files/water.csv')
# These are manually converted to kmls at https://www.google.com/maps/d/u/0/
