# https://docs.google.com/document/d/1kAXh00E4blsHRWw5VyhxeKtbq1kEzCxqChO12qoPNZ0/edit#heading=h.5cd1aedbdpsg

library(sp)
library(bohemia)
library(tidyverse)
library(yaml)
library(databrew)
library(leaflet)
library(kableExtra)
library(ggplot2)
library(readr)
theme_set(theme_simple())

# Create the 3 level treatment assignments

if(!'treatment_assignments.csv' %in% dir()){
  set.seed(as.numeric(Sys.time())) # intentionally unreproducible
  grps <- c('Control', 'Human', 'Human+Animal')
  treatment_assignments <- 
    tibble(assignment = 1:3,
           intervention = sample(grps))
  write_csv(treatment_assignments, 'treatment_assignments.csv')
} else {
  treatment_assignments <- read_csv('treatment_assignments.csv')
}

set.seed(123)

# Read in objects created in monte_carlo.R
# (simulation number 9 is the final one)
cluster_level <- read_csv('cluster_level.csv')
hamlet_level <- read_csv('hamlet_level.csv')
hh_level <- read_csv('hh_level.csv')
status_level <- read_csv('status_level.csv')
load('image.RData')
load('../data.RData')

# # Join household id to hh level data
# hh_level <- left_join(
#   hh_level,
#   pd_moz$minicensus_main %>% dplyr::select(instance_id, hh_id)
# )
# write_csv(hh_level, 'hh_level.csv')

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
  
  # Reconfigure columns as per August 6th email request
  subs <- pd_moz$minicensus_repeat_hh_sub
  subs <- left_join(subs, pd_moz$minicensus_main %>% dplyr::select(instance_id, hh_id))
  subs <- left_join(subs, pd_moz$minicensus_people %>%
                      dplyr::rename(hh_sub_id = num) %>%
                      dplyr::select(instance_id, hh_sub_id, 
                                    sub_first_name = first_name,
                                    sub_last_name = last_name))
  subs <- subs %>%
    mutate(hh_head_substitute = paste0(sub_first_name, ' ', sub_last_name)) %>%
    group_by(hh_id) %>%
    summarise(hh_head_substitute = paste0(hh_head_substitute, collapse = '; '))
  sentinel_cdc_light_trap <- sentinel_cdc_light_trap %>%
    mutate(is_the_hh_id_correct = ' ',
           is_the_hh_head_name_correct = ' ',
           code = substr(hh_id, 1, 3)) %>%
    left_join(locations %>% dplyr::select(code, Ward, Village, Hamlet)) %>%
    left_join(subs) %>%
    left_join(pd_moz$minicensus_main %>% dplyr::select(contact = hh_contact_info_number, hh_id))
  
  # Re arrange a bit as per request
  sentinel_cdc_light_trap <- sentinel_cdc_light_trap %>%
    mutate(No = 1:nrow(sentinel_cdc_light_trap)) %>%
    mutate(hh_head_name = paste0(first_name, ' ', last_name)) %>%
    dplyr::select(No, cluster, hh_id,
                  is_the_hh_id_correct,
                  hh_head_name,
                  is_the_hh_head_name_correct,
                  hh_head_substitute,
                  Ward, Village, Hamlet, contact,
                  sample_type, randomization_number) %>%
    mutate(observation = ' ') 
  
  sentinel_cdc_light_trap_only_6 <- sentinel_cdc_light_trap %>%
    filter(randomization_number <= 6)
  write_csv(sentinel_cdc_light_trap, 'sentinel_cdc_light_trap.csv')
  write_csv(sentinel_cdc_light_trap_only_6, 'sentinel_cdc_light_trap_only_6.csv')
  
  # Make deliverable Sentinel CDC light trap, deliverable 2
  # Deliverable 2: An operational list, ie a copy of the above table but without is_the_hh_id_correct, is_the_hh_head_name_correct. This table may need to be re-generated periodically as households can be dropped out and replaced by backups.
  sentinel_cdc_light_trap_deliverable_2 <- sentinel_cdc_light_trap %>%
    dplyr::select(-is_the_hh_id_correct,
                  -is_the_hh_head_name_correct)
  write_csv(sentinel_cdc_light_trap_deliverable_2, 'sentinel_cdc_light_trap_deliverable_2.csv')
}

# Resting household indoor
# From each of the 15 sentinel clusters, sample 4 households (ie, 60 total households), plus 2 backup households clearly indicated as such (ie, 90 total households).
# Ensure no overlap with those c sampled from the "Sentinel CDC light trap" selection (above)
if('resting_household_indoor.csv' %in% dir()){
  message('reading')
  resting_household_indoor <- read_csv('resting_household_indoor.csv')
} else {
  # Sample 4 households (plus 2 backups) for each of the 15 sentinenl clusters, with no overlap with the sentinel cdc light trap
  # households. In order to avoid overlap, we can just exploit the randomness in the previous sample, but do inverse
  resting_household_indoor <- sentinel_cdc_light_trap %>%
    dplyr::select(-No) %>%
    group_by(cluster) %>%
    mutate(max_n = max(randomization_number)) %>%
    ungroup %>%
    mutate(new_randomization_number = 1 + (max_n - randomization_number))
  resting_household_indoor <- resting_household_indoor %>%
    mutate(No = 1:nrow(resting_household_indoor)) %>%
    mutate(sample_type = 'Resting household indoor') %>%
    dplyr::select(No, cluster, hh_id,
                  is_the_hh_id_correct,
                  hh_head_name,
                  is_the_hh_head_name_correct,
                  hh_head_substitute,
                  Ward, Village, Hamlet,
                  contact,
                  sample_type,
                  randomization_number,
                  observation)
  write_csv(resting_household_indoor, 'resting_household_indoor.csv')
  resting_household_indoor_only_6 <- resting_household_indoor %>%
    filter(randomization_number <= 6)
  write_csv(resting_household_indoor_only_6, 'resting_household_indoor_only_6.csv')
  resting_household_indoor_deliverable_2 <- 
    resting_household_indoor %>% dplyr::select(-is_the_hh_id_correct,
                                               -is_the_hh_head_name_correct)
  write_csv(resting_household_indoor_deliverable_2, 'resting_household_indoor_deliverable_2.csv')
  message('writing')
}

# Resting household pit shelter
# From each of the 15 sentinel clusters, sample 1 qualified non-refusing household (ie, 15 total households).
# Ensure no overlap with those households sampled from the "Sentinel CDC light trap" selection (far above)
if('resting_household_pit_shelter.csv' %in% dir()){
  message('reading')
  resting_household_pit_shelter <- read_csv('resting_household_pit_shelter.csv')
} else {
  message('writing')
  set.seed(123)
  resting_household_pit_shelter <- sentinel_cdc_light_trap %>%
    # no overlap
    filter(randomization_number > 6) %>%
    dplyr::select(-No, -randomization_number) %>%
    mutate(sample_type = 'Resting household pit shelter')
  resting_household_pit_shelter <- resting_household_pit_shelter %>% dplyr::sample_n(nrow(resting_household_pit_shelter))
  resting_household_pit_shelter <- resting_household_pit_shelter %>%
    mutate(dummy = 1) %>%
    group_by(cluster) %>%
    mutate(randomization_number = cumsum(dummy)) %>%
    ungroup %>%
    filter(randomization_number <= 3) %>%
    mutate(sample_type = ifelse(randomization_number > 1,
                                paste0(sample_type, ' (backup)'),
                                sample_type)) 
  resting_household_pit_shelter$No <- 1:nrow(resting_household_pit_shelter)
  resting_household_pit_shelter <- resting_household_pit_shelter %>%
    dplyr::select(No, cluster, hh_id, is_the_hh_id_correct, hh_head_name, is_the_hh_head_name_correct, hh_head_substitute, Ward, Village, Hamlet, contact, sample_type, randomization_number, observation)
  write_csv(resting_household_pit_shelter, 'resting_household_pit_shelter.csv')
  # Deliverable 2: Deliverable 2: An operational list,. ie a copy of the above table but without is_the_hh_id_correct, is_the_hh_head_name_correct.
  resting_household_pit_shelter_deliverable_2 <- resting_household_pit_shelter %>%
    dplyr::select(-is_the_hh_id_correct,
                    -is_the_hh_head_name_correct)
  write_csv(resting_household_pit_shelter_deliverable_2, 'resting_household_pit_shelter_deliverable_2.csv')
}


# Random CDC Light trap households
# (not to be confused with sentinel CDC light trap households)
# -For each treatment arm, randomly sample 3 clusters (ie, 9 total clusters), but do not sample any of the sentinel clusters.
# From each sampled cluster, randomly sample 2 qualified non-refusing households (ie, 18 total households) plus 2 backups (ie, 36 total households) clearly indicated as such.
# Repeat the above 15 times, so that there is a randomization for each of the 15 study months.
# Deliverable: A table in which each row is one household-month combination, with columns indicating month_number, household ID, cluster, treatment arm, randomization number, randomization activity = "Random CDC Light trap households", backup.
if('random_cdc_light_trap_households.csv' %in% dir()){
  random_cdc_light_trap_households <- read_csv('random_cdc_light_trap_households.csv')
  random_cdc_light_trap_clusters <- read_csv('random_cdc_light_trap_clusters.csv')
  random_cdc_light_trap_households_only_4 <- read_csv('random_cdc_light_trap_households_only_4.csv')
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
  # random_cdc_light_trap_households_deliverable_2 <- random_cdc_light_trap_households %>%
  #   dplyr::select(-is_the_hh_id_correct,
  #                 -is_the_hh_head_name_correct)
  # write_csv(random_cdc_light_trap_households_deliverable_2, 'random_cdc_light_trap_households_deliverable_2.csv')
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


# CDC Light Trap Livestock Enclosures
# From the 15 sentinel clusters, randomly sample 2 clusters for each arm (ie, 6 total clusters).
# From each randomly sampled cluster, sample 2 livestock enclosures (ie, 12 total livestock enclosures).
if('cdc_light_trap_livestock_enclosures_clusters.csv' %in% dir()){
  message('reading')
  cdc_light_trap_livestock_enclosures_clusters <- read_csv('cdc_light_trap_livestock_enclosures_clusters.csv')
} else {
  set.seed(123)
  message('writing')
  # Get the 15 sentinel clusters and randomly sample 2 per arm
  cdc_light_trap_livestock_enclosures_clusters <- ento_sentinel_clusters %>%
    dplyr::sample_n(nrow(ento_sentinel_clusters)) %>%
    mutate(dummy = 1) %>%
    group_by(arm) %>%
    mutate(cs = cumsum(dummy)) %>%
    ungroup %>%
    filter(cs %in% 1:2) %>%
    arrange(arm) %>%
    dplyr::select(-dummy, -cs) %>%
    mutate(type = 'CDC light trap livestock enclosure cluster')
  write_csv(cdc_light_trap_livestock_enclosures_clusters, 'cdc_light_trap_livestock_enclosures_clusters.csv')
} 
# Having established the 6 clusters, get all the livestock-holding households in those clusters
if('cdc_light_trap_livestock_enclosures_clusters_all_hh.csv' %in% dir()){
  cdc_light_trap_livestock_enclosures_clusters_all_hh <- 
    read_csv('cdc_light_trap_livestock_enclosures_clusters_all_hh.csv')
  message('reading')
} else {
  cdc_light_trap_livestock_enclosures_clusters_all_hh <- 
    pd_moz$minicensus_main %>%
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
                  hh_animals_dry_season_pigs) %>%
    left_join(hh_level %>%
                dplyr::select(instance_id, cluster)) %>%
    filter(!is.na(cluster)) %>%
    filter(cluster %in% cdc_light_trap_livestock_enclosures_clusters$cluster)
  locs <- extract_ll(cdc_light_trap_livestock_enclosures_clusters_all_hh$hh_geo_location)
  cdc_light_trap_livestock_enclosures_clusters_all_hh <- bind_cols(cdc_light_trap_livestock_enclosures_clusters_all_hh, locs)
  cdc_light_trap_livestock_enclosures_clusters_all_hh <- cdc_light_trap_livestock_enclosures_clusters_all_hh %>%
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
  
  write_csv(cdc_light_trap_livestock_enclosures_clusters_all_hh, 'cdc_light_trap_livestock_enclosures_clusters_all_hh.csv')
  message('writing')
}


# CDC Light Trap Livestock Enclosures Deliverable 1: A “guideline dataset” which is a table of all households from the minicensus which indicated (in the minicensus) a nearby livestock enclosure, and are in the 6 clusters, with columns showing household ID, cluster.  This table should be randomly ordered, since its ordering will serve to ensure that enrollment is random. 
if('cdc_light_trap_livestock_enclosures_clusters_deliverable_1.csv' %in% dir()){
  cdc_light_trap_livestock_enclosures_clusters_deliverable_1 <- read_csv('cdc_light_trap_livestock_enclosures_clusters_deliverable_1.csv')
} else {
  cdc_light_trap_livestock_enclosures_clusters_deliverable_1 <-
    left_join(
      cdc_light_trap_livestock_enclosures_clusters_all_hh,
      bohemia::locations %>% dplyr::select(
        hamlet_code = code,
        Ward,
        Village,
        Hamlet
      )
    ) %>%
    dplyr::select(hh_id, cluster, Ward, Village, Hamlet) %>%
    mutate(sample_type = 'CDC light trap livestock enclosures guideline') %>%
    mutate(observation = ' ')
  write_csv(cdc_light_trap_livestock_enclosures_clusters_deliverable_1, 'cdc_light_trap_livestock_enclosures_clusters_deliverable_1.csv')
}

# CDC Light Trap Livestock Enclosures Deliverable 2: 6 separate operational lists (1 per cluster) which consist of the rows of the “guideline dataset” but contain only the households for the cluster in question. Each of these 6 tables will be identically formatted to deliverable 1, but with (a) few rowers and (b) a clear “title” indicating which cluster number the list is for (ie, 1 through 6). 
if(!'cdc_light_trap_livestock_enclosures_clusters_deliverable_2.zip' %in% dir()){
  if(!dir.exists('cdc_light_trap_livestock_enclosures_clusters_deliverable_2')){
    dir.create('cdc_light_trap_livestock_enclosures_clusters_deliverable_2')
    cluster_numbers <- sort(unique(cdc_light_trap_livestock_enclosures_clusters_deliverable_1$cluster))
    for(i in 1:length(cluster_numbers)){
      this_cluster_number <- cluster_numbers[i]
      these_data <- cdc_light_trap_livestock_enclosures_clusters_deliverable_1 %>% filter(cluster == this_cluster_number)
      write_csv(these_data,
                file = paste0('cdc_light_trap_livestock_enclosures_clusters_deliverable_2/',
                              'cluster_', this_cluster_number, '.csv'))
    }
  }
  zip(zipfile = 'cdc_light_trap_livestock_enclosures_clusters_deliverable_2.zip',
      files = paste0('cdc_light_trap_livestock_enclosures_clusters_deliverable_2'))
  unlink('cdc_light_trap_livestock_enclosures_clusters_deliverable_2', recursive = TRUE)
}

# # CDC Light Trap Livestock Enclosures Deliverable 3: For every cluster above (deliverable 2) a list of ONLY 2 Livestock Enclosure IDs to assign to the first 2 LE that the FW finds that qualify for enrollment.
# Using ID notation from: https://docs.google.com/document/d/118kY_VRB_OxpUg7Iau7_7Lyl12__tC-GJjRo9W36Qw8/edit
if('cdc_light_trap_livestock_enclosures_clusters_deliverable_3.csv' %in% dir()){
  cdc_light_trap_livestock_enclosures_clusters_deliverable_3 <- read_csv('cdc_light_trap_livestock_enclosures_clusters_deliverable_3.csv')
} else {
  cluster_numbers <- sort(unique(cdc_light_trap_livestock_enclosures_clusters$cluster))
  out_list <- list()
  for(i in 1:length(cluster_numbers)){
    this_cluster_number <- cluster_numbers[i]
    these_ids <- paste0('L', this_cluster_number, '-', 11:12)
    out <- tibble(id = these_ids,
                  cluster = this_cluster_number,
                  sample_type = 'CDC light trap livestock enclosure IDs for assignment')
    out_list[[i]] <- out
  }
  cdc_light_trap_livestock_enclosures_clusters_deliverable_3 <- bind_rows(out_list)
  write_csv(cdc_light_trap_livestock_enclosures_clusters_deliverable_3, 'cdc_light_trap_livestock_enclosures_clusters_deliverable_3.csv')  
}

# Human baited double net
# From the 15 sentinel clusters, randomly sample 2 clusters for each arm (ie, 6 total clusters). Saved as “human_baited_double_net_clusters.csv”
if('human_baited_double_net_clusters.csv' %in% dir()){
  human_baited_double_net_clusters <- read_csv('human_baited_double_net_clusters.csv')
  message('reading')
} else {
  set.seed(123)
  human_baited_double_net_clusters <- ento_sentinel_clusters %>%
    dplyr::sample_n(nrow(ento_sentinel_clusters)) %>%
    mutate(dummy = 1) %>%
    group_by(arm) %>%
    mutate(cs = cumsum(dummy)) %>%
    ungroup %>%
    filter(cs <= 2) %>%
    arrange(arm, cluster) %>%
    dplyr::select(-dummy, -cs)
  write_csv(human_baited_double_net_clusters, 'human_baited_double_net_clusters.csv')
  message('writing')
}
# Human baited double net - Deliverable: A table in which each row is one household (include all households in the cluster), with columns indicating No, cluster, hh_id, is_the_hh_id_correct, hh_head_name, is_the_hh_head_name_correct, hh_head_substitute, Ward, Village, Hamlet, contact, sample_type, randomization_number, observation .
if('human_baited_double_net_deliverable_1.csv' %in% dir()){
  human_baited_double_net_deliverable_1 <- read_csv('human_baited_double_net_deliverable_1.csv')
  message('reading')
} else {
  out_list <- list()
  set.seed(123)
  for(i in 1:nrow(human_baited_double_net_clusters)){
    this_cluster <- human_baited_double_net_clusters[i,]
    this_cluster_number <- this_cluster$cluster
    these_hh <- master_poly[master_poly@data$cluster == this_cluster_number,]
    # randomize order
    sample_index <- sample(1:nrow(these_hh), nrow(these_hh)) 
    sample_hh <- these_hh[sample_index,]
    sample_hh$sample_number <- 1:nrow(these_hh)
    sample_hh$sample_type <- 'Human baited double net household'
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
  human_baited_double_net_deliverable_1 <- sample_hh
  human_baited_double_net_deliverable_1 <- left_join(
    human_baited_double_net_deliverable_1,
    arm_assignments
  )
  
  # Reconfigure columns as per August 6th email request
  subs <- pd_moz$minicensus_repeat_hh_sub
  subs <- left_join(subs, pd_moz$minicensus_main %>% dplyr::select(instance_id, hh_id))
  subs <- left_join(subs, pd_moz$minicensus_people %>%
                      dplyr::rename(hh_sub_id = num) %>%
                      dplyr::select(instance_id, hh_sub_id, 
                                    sub_first_name = first_name,
                                    sub_last_name = last_name))
  subs <- subs %>%
    mutate(hh_head_substitute = paste0(sub_first_name, ' ', sub_last_name)) %>%
    group_by(hh_id) %>%
    summarise(hh_head_substitute = paste0(hh_head_substitute, collapse = '; '))
  human_baited_double_net_deliverable_1 <- human_baited_double_net_deliverable_1 %>%
    mutate(is_the_hh_id_correct = ' ',
           is_the_hh_head_name_correct = ' ',
           code = substr(hh_id, 1, 3)) %>%
    left_join(locations %>% dplyr::select(code, Ward, Village, Hamlet)) %>%
    left_join(subs) %>%
    left_join(pd_moz$minicensus_main %>% dplyr::select(contact = hh_contact_info_number, hh_id))
  
  # Re arrange a bit as per request
  human_baited_double_net_deliverable_1 <- human_baited_double_net_deliverable_1 %>%
    mutate(No = 1:nrow(human_baited_double_net_deliverable_1)) %>%
    mutate(hh_head_name = paste0(first_name, ' ', last_name)) %>%
    dplyr::select(No, cluster, hh_id,
                  is_the_hh_id_correct,
                  hh_head_name,
                  is_the_hh_head_name_correct,
                  hh_head_substitute,
                  Ward, Village, Hamlet, contact,
                  sample_type, randomization_number) %>%
    mutate(observation = ' ') 
  write_csv(human_baited_double_net_deliverable_1, 'human_baited_double_net_deliverable_1.csv')
  message('writing')
}

# Human baited double net - Deliverable 2: An operational list populated with columns ie a copy of the above table but without is_the_hh_id_correct, is_the_hh_head_name_correct.
if('human_baited_double_net_deliverable_2.csv' %in% dir()){
  human_baited_double_net_deliverable_2 <- read_csv('human_baited_double_net_deliverable_2.csv')
} else {
  human_baited_double_net_deliverable_2 <- human_baited_double_net_deliverable_1 %>%
    dplyr::select(-is_the_hh_id_correct,
                  -is_the_hh_head_name_correct)
  write_csv(human_baited_double_net_deliverable_2, 'human_baited_double_net_deliverable_2.csv')
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

# LARVAL HABITATS BODIES OF WATER
# 1 point for every household reporting any body of water, as per minicensus. 21 and 21a
if('larval_habitats_deliverable_1.csv' %in% dir()){
  larval_habitats_deliverable_1 <- read_csv('larval_habitats_deliverable_1.csv')
  message('reading')
} else {
  water <- pd_moz$minicensus_repeat_water %>%
    left_join(pd_moz$minicensus_main %>% 
                dplyr::select(instance_id, water_bodies_how_many, wid, hh_id,
                              hh_geo_location)) %>%
    mutate(hamlet_code = substr(hh_id, 1, 3))
  locs <- extract_ll(water$hh_geo_location)
  water <- bind_cols(water, locs)
  water$description <- water$water_bodies_type
  water <- water %>%
    mutate(code = substr(hh_id, 1,3)) %>%
    left_join(hh_level %>% 
                dplyr::select(cluster, status, code, hh_id))
  larval_habitats_deliverable_1 <- water %>%
    dplyr::select(hh_id,
                  cluster,
                  `kind of water` = description,
                  status)
  write_csv(larval_habitats_deliverable_1, 'larval_habitats_deliverable_1.csv')
  message('writing')
}


# LIVESTOCK ENCLSORUES
# 1 point for every household reporting a livestock enclosure, as per minicensus. questions 19a and 19b in minicensus
# livestock <- pd_moz$minicensus_main %>%
#   mutate(hamlet_code = substr(hh_id, 1, 3)) %>%
#   dplyr::select(instance_id,
#                 hamlet_code,
#                 wid,
#                 hh_id,
#                 hh_geo_location,
#                 # Cattle
#                 hh_animals_distance_cattle_dry_season,
#                 hh_animals_distance_cattle_rainy_season,
#                 hh_animals_where_cattle_dry_season,
#                 hh_animals_where_cattle_rainy_season,
#                 # Pigs
#                 hh_animals_dry_season_distance_pigs, 
#                 hh_animals_rainy_season_distance_pigs,
#                 hh_animals_rainy_season_pigs,
#                 hh_animals_dry_season_pigs)
# locs <- extract_ll(livestock$hh_geo_location)
# livestock <- bind_cols(livestock, locs)
# livestock <- livestock %>%
#   filter(!is.na(hh_animals_rainy_season_pigs) |
#            !is.na(hh_animals_dry_season_pigs) |
#            !is.na(hh_animals_where_cattle_dry_season) |
#            !is.na(hh_animals_where_cattle_rainy_season)) %>%
#   mutate(description = 
#            paste0(
#              ifelse(!is.na(hh_animals_dry_season_pigs),
#                     paste0('Pigs in dry season: ', hh_animals_dry_season_pigs, '. '),
#                     ''),
#              ifelse(!is.na(hh_animals_rainy_season_pigs),
#                            paste0('Pigs in rainy season: ', hh_animals_dry_season_pigs, '. '),
#                            ''),
#              ifelse(!is.na(hh_animals_where_cattle_rainy_season),
#                     paste0('Cattle in rainy season: ', hh_animals_where_cattle_rainy_season, '. '),
#                     ''),
#              ifelse(!is.na(hh_animals_where_cattle_dry_season),
#                     paste0('Cattle in dry season: ', hh_animals_where_cattle_dry_season, '. '),
#                     '')
#            ))

livestock <- cdc_light_trap_livestock_enclosures_clusters_all_hh
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
