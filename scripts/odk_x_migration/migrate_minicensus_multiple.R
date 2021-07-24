server_root <- 'databrew.fr'
country <- 'Mozambique'
suitcase_dir <- '/home/joebrew/Documents/suitcase'
jar_file <- 'ODK-X_Suitcase_v2.1.8.jar'
odkx_path <- '/home/joebrew/Documents/bohemia/odkx/app/config' # must be full path!
kf <- '../../credentials/bohemia_priv.pem' #path to private key for name decryption
creds <- yaml::yaml.load_file('../../credentials/credentials.yaml')
use_real_names <- FALSE # whether to decrypt names (TRUE) or use fakes ones (false)
is_linux <- Sys.info()['sysname'] == 'Linux'
library(dplyr)

# Read in the teams-geographies mapping
library(readr)
mapping <- read_csv('migration_groups_hamletlevel.csv') # emailed from eldo
# Assign 1 of 3 teams to each hamlet
gps <- bohemia::gps
gpx <- left_join(mapping,
                 gps %>%
                   dplyr::select(code, lng, lat)) %>%
  filter(!is.na(lng))
gpx$team_number <- as.numeric(factor(gpx$team_name))

# library(ggplot2)
# ggplot(data = gpx,
#        aes(x = lng,
#            y = lat)) +
#   geom_point(aes(color = factor(team_number)))

# Based on the above plot, let's make explicit teams
team_df <- 
  tibble(team_number = 1:8,
         team_agg = c(2, 2, 1, 3,
                      1, 1, 2, 3))
gpx <- left_join(gpx, team_df)

# # Write csvs
# right <- gpx %>%
#   dplyr::distinct(team_name, .keep_all = TRUE) %>%
#   dplyr::select(team_name, team_number)
# team_write <- team_df %>%
#   left_join(right) %>%
#   dplyr::select(team_agg, team_name)
# write_csv(team_df, '~/Desktop/eldo_team.csv')
# 
# 
# ggplot(data = gpx %>%
#          mutate(team_name = paste0(team_name, ': sync-',
#                                    team_agg, '.databrew.fr')),
#        aes(x = lng,
#            y = lat)) +
#   geom_point(aes(color = factor(team_name)),
#              size = 4,
#              alpha = 0.7) +
#   geom_text(aes(label = team_agg),
#             size = 2) +
#   theme(legend.position = 'right') +
#   scale_color_manual(name = 'Team',
#                      values = rainbow(8))
# 
# library(leaflet)
# leaflet() %>%
#   addProviderTiles(providers$Esri.WorldImagery) %>%
#   addMarkers(data = gpx %>% filter(team_agg == 3)) %>%
#   addCircleMarkers(data = gpx %>% filter(team_agg %in% 1:2))


# ll <- bohemia::locations
# ll <- ll %>% filter(Country == 'Tanzania')
# ll_sub <- ll %>% filter(Ward %in% c('Kibiti', 'Dimani', 'Mchukwi', 'Mtawanya'))
# only_hamlets <- ll_sub$code # c('BAA', 'DOW', 'KIS', 'LIK') # set to NULL if you want all hamlets
# 
# print(creds$odkx_server)


# Check the directory
this_dir <- getwd()
split_dir <- unlist(strsplit(this_dir, split = '/'))
check <- split_dir[length(split_dir)] == 'odk_x_migration' & split_dir[length(split_dir) - 1] == 'scripts' & split_dir[length(split_dir) - 2] == 'bohemia'
if(!check){
  message('YOU ARE IN THE WRONG DIRECTORY. MAKE SURE YOU ARE IN bohemia/scripts/odk_x_migration')
}

#!/usr/bin/env Rscript

# Ensure you are in scripts/odk_x_migration/

library(dplyr)
library(tidyr)
library(stringr)
library(sp)

SAVEPOINT_TYPE_COMPLETE <- 'COMPLETE'
SUITCASE_UPDATE_OP <- 'FORCE_UPDATE'

CONTACT_NUM_LENGTH <- 9
HH_ID_LENGTH <- 4 * 2 - 1
EXT_ID_LENGTH <- 4 * 3 - 1

add_metadata_col <- function(df, form_id) {
  if (!('_id') %in% colnames(df)) {
    # Add an empty _id column if there isn't one.
    # When the _id column is empty,
    # it will be populated with a generated UUID by ODK-X Suitcase on upload
    df <- df %>% tibble::add_column(`_id` = NA, .before = 1)
  }

  df %>%
    tibble::add_column(
      `_locale` = NA,
      `_savepoint_type` = SAVEPOINT_TYPE_COMPLETE,
      `_savepoint_timestamp` = NA,
      `_savepoint_creator` = NA,
      `_default_access` = NA,
      `_group_modify` = NA,
      `_group_privileged` = NA,
      `_group_read_only` = NA,
      `_row_etag` = NA,
      `_row_owner` = NA,
      .before = 1
    ) %>%
    tibble::add_column(`_form_id` = form_id, .before = 1) %>%
    tibble::add_column(operation = SUITCASE_UPDATE_OP, .before = 1)
}

# select_multiple fields are encoded as JSON array
format_select_multiple <- function(...) {
  str_c('["', str_c(..., collapse = '","'), '"]')
}

# Define function for making fake names
library(babynames)
fake_names <- function(n = 1, words = 2){
  vec <- babynames::babynames$name
  first_name <- sample(vec, size = n, replace = T)
  if(words == 1){
    out <- first_name
  } else {
    last_name <- sample(vec, size = n, replace = T)
    out <- paste0(first_name, ' ', last_name)
  }
  return(out)
}

format_to_odk_x <- function(
  out_list,
  full_migration = FALSE,
  sample_hh = FALSE,
  truncate_name = FALSE,
  hh_csv_path = 'odk_x_hh.csv',
  member_csv_path = 'odk_x_member.csv',
  geo_location_csv_path = 'odk_x_geolocation.csv',
  death_csv_path = 'odk_x_death.csv') {
  message('Processing clean_minicensus_main')
  hh_col <- c(
    'instance_id',
    'hh_member_num',
    'hh_head_id',
    'hh_contact_info_number',
    'hh_contact_info_number_alternate',
    'hh_id',
    'hh_geo_location'
  )

  if (full_migration) {
    hh_col <- c(
      hh_col,
      'hh_animals_distance_cattle_dry_season',
      'hh_animals_distance_cattle_rainy_season',
      'hh_animals_dry_season_distance_pigs',
      'hh_animals_rainy_season_distance_pigs',
      'hh_animals_where_cattle_rainy_season',
      'hh_animals_where_cattle_dry_season',
      'hh_animals_rainy_season_pigs',
      'hh_animals_dry_season_pigs',
      'hh_main_building_type',
      'hh_main_energy_source_for_lighting',
      'hh_main_wall_material',
      'hh_n_constructions',
      'hh_n_constructions_sleep'
    )
  }

  odk_x_hh <- out_list$clean_minicensus_main %>%
    dplyr::distinct(hh_id, .keep_all = TRUE) %>%
    select(all_of(hh_col)) %>%
    filter(nchar(hh_id) == HH_ID_LENGTH) %>%
    mutate(across(
      c(
        hh_contact_info_number,
        hh_contact_info_number_alternate
      ),
      ~ case_when(nchar(.) == CONTACT_NUM_LENGTH ~ .)
    )) %>%
    mutate(hh_contact_no = case_when(is.na(hh_contact_info_number) ~ format_select_multiple('no_contact_num'))) %>%
    mutate(hh_contact_alt_no = case_when(is.na(hh_contact_info_number_alternate) ~ format_select_multiple('no_alt_contact_num'))) %>%
    mutate(hh_head_id = paste(hh_id, str_pad(hh_head_id, 3, pad = '0'), sep = '-')) %>%
    tibble::add_column(hh_minicenced = 'yes')

  if (sample_hh) {
    set.seed(sample_hh)
    odk_x_hh <- sample_n(odk_x_hh, sample_hh, replace = FALSE)
  }

  if (full_migration) {
    odk_x_hh <- odk_x_hh %>%
      mutate(across(
        c(
          hh_animals_where_cattle_rainy_season,
          hh_animals_where_cattle_dry_season,
          hh_animals_rainy_season_pigs,
          hh_animals_dry_season_pigs
        ),
        ~ recode(
          .,
          `Inside the compound but not inside a structure, but more than 15 meters from the house` = 'Inside the compound but not inside a structure, but >15 meters from the house',
          `Inside the compound but not inside a structure, within 15 meters from the house` = 'Inside the compound but not inside a structure within 15 meters from the household',
          `There is no fixed space to house them` = 'Unspecified location for keeping them'
        )
      )) %>%
      separate(
        col = 'hh_main_wall_material',
        sep = 'other',
        fill = 'right',
        into = c('house_wall_material', 'house_wall_material_other')
      ) %>%
      mutate(house_wall_material = ifelse(
        is.na(house_wall_material_other),
        house_wall_material,
        paste0(house_wall_material, 'other')
      )) %>%
      mutate(across(
        c('house_wall_material', 'house_wall_material_other'),
        ~ str_trim(.)
      )) %>%
      mutate(house_wall_material = sapply(str_split(house_wall_material, pattern = ' '), format_select_multiple)) %>%
      left_join(
        select(rename(out_list$clean_minicensus_repeat_water, water_bodies_num = num), instance_id, water_bodies_num),
        by = 'instance_id'
      ) %>%
      left_join(
        select(rename(out_list$clean_minicensus_repeat_mosquito_net, hh_num_nets = num), instance_id, hh_num_nets),
        by = 'instance_id'
      )
  }

  message('Processing geolocation')
  odk_x_hh_geo_location <- odk_x_hh %>%
    select(hh_id, hh_geo_location) %>%
    separate(
      col = 'hh_geo_location',
      sep = ' ',
      into = c(
        'hh_geo_location_latitude',
        'hh_geo_location_longitude',
        'hh_geo_location_altitude',
        'hh_geo_location_accuracy'
      )
    )

  message('Processing clean_minicensus_repeat_hh_sub')
  odk_x_hh_sub <- out_list$clean_minicensus_repeat_hh_sub %>%
    filter(!is.na(hh_sub_id)) %>%
    distinct(instance_id, .keep_all = TRUE) %>%
    inner_join(
      select(odk_x_hh, instance_id, hh_id),
      by = 'instance_id'
    ) %>%
    mutate(hh_sub_id = paste(hh_id, str_pad(hh_sub_id, 3, pad = '0'), sep = '-'))

  if (full_migration) {
    odk_x_hh_sub <- odk_x_hh_sub %>%
      mutate(hh_sub_relationship = recode(
        hh_sub_relationship,
        husband_or_wife = 'husband_wife',
        son_or_daughter = 'son_daughter',
        brother_or_sister = 'brother_sister',
        father = 'father_mother',
        mother = 'father_mother',
        son_or_daughter_in_law = 'son_daughterinlaw',
        brother_or_sister_in_law = 'brother_sisterinlaw',
        uncle_or_aunt = 'uncle_aunt',
        grandson_or_granddaughter = 'grandson_granddaughter',
        adopted_son_or_daughter = 'adopted_son_daughter',
        newphew_or_niece = 'nephew_niece'
      ))
  }

  message('Processing clean_minicensus_people')
  odk_x_member <- out_list$clean_minicensus_people %>%
    distinct(instance_id, pid, .keep_all = TRUE) %>%
    select(
      instance_id,
      first_name,
      last_name,
      pid,
      dob,
      gender,
      member_resident
    ) %>%
    inner_join(
      select(odk_x_hh, instance_id, hh_id),
      by = 'instance_id'
    ) %>%
    filter(nchar(pid) == EXT_ID_LENGTH) %>%
    mutate(member_resident = recode(
      member_resident,
      `Resident Member` = 'resident',
      `Non-resident Member` = 'non_resident'
    )) %>%
    mutate(
      dob_day = as.numeric(format(dob, '%d')),
      dob_month = as.numeric(format(dob, '%m')),
      dob_year = as.numeric(format(dob, '%Y'))
    ) %>%
    select(-instance_id, -dob)

  if (truncate_name) {
    odk_x_member <- odk_x_member %>%
      mutate(across(c(first_name, last_name), ~ str_sub(., end = 11)))
  }

  if (full_migration) {
    message('Processing clean_minicensus_repeat_death_info')
    odk_x_death <- out_list$clean_minicensus_repeat_death_info %>%
      filter(!is.na(death_id)) %>%
      select(
        instance_id,
        death_age,
        death_age_unit,
        death_dob_unknown,
        death_dod,
        death_gender,
        death_id,
        death_name,
        death_surname
      ) %>%
      mutate(death_age = ifelse(death_age_unit == 'Years' | is.na(death_age_unit), death_age, 0)) %>%
      mutate(death_age = coalesce(death_age, death_dob_unknown)) %>%
      mutate(death_dod = format(death_dod, '%Y-%m-%d')) %>%
      mutate(hh_death_date_dk = ifelse(is.na(death_dod), format_select_multiple('dk'), NA)) %>%
      inner_join(
        select(odk_x_hh, instance_id, hh_id),
        by = 'instance_id'
      ) %>%
      select(-death_age_unit, -death_dob_unknown, -instance_id)
  }

  # Add UUID for individuals
  # The UUID needs to be generated here to populate the HH head column in the HH table
  message('Generating UUIDs')
  odk_x_member <- tibble::add_column(
    odk_x_member,
    `_id` = paste0('uuid:', uuid::UUIDgenerate(n = nrow(odk_x_member))),
    .before = 1
  )

  # Note that some hh_head_id cannot be found in the member table
  message('Processing HH head and HH head sub')
  odk_x_hh <- odk_x_hh %>%
    left_join(
      select(odk_x_hh_sub, instance_id, hh_sub_id, hh_sub_relationship),
      by = 'instance_id'
    ) %>%
    left_join(
      select(rename(odk_x_member, hh_head_uuid = `_id`), pid, hh_head_uuid),
      by = c('hh_head_id' = 'pid')
    ) %>%
    left_join(
      select(rename(odk_x_member, hh_sub_uuid = `_id`), pid, hh_sub_uuid),
      by = c('hh_sub_id' = 'pid')
    ) %>%
    select(-hh_head_id, -hh_sub_id) %>%
    rename(
      hh_head_id = hh_head_uuid,
      hh_sub_id = hh_sub_uuid
    ) %>%
    mutate(no_hh_head_sub = ifelse(is.na(hh_sub_id), format_select_multiple('no_sub'), NA))

  if (!full_migration) {
    odk_x_hh <- odk_x_hh %>% select(-hh_sub_relationship)
  }

  message('Deleting CSVs')
  all_csv <- c(hh_csv_path, member_csv_path, geo_location_csv_path, death_csv_path)
  removedCsvs <- sum(file.remove(all_csv[sapply(all_csv, file.exists)]))
  message('Deleted ', removedCsvs, ' CSVs')

  # Rename columns and write to CSV
  message('Writing CSVs')

  odk_x_hh <- odk_x_hh %>% rename(
    `_id` = instance_id,
    minicensed_hh_n_residents = hh_member_num,
    hh_head_new_select = hh_head_id,
    hh_head_sub_new_select = hh_sub_id,
    hh_contact = hh_contact_info_number,
    hh_contact_alt = hh_contact_info_number_alternate
  )

  if (full_migration) {
    odk_x_hh <- odk_x_hh %>% rename(
      distance_cattle_rainy_season = hh_animals_distance_cattle_rainy_season,
      distance_cattle_dry_season = hh_animals_distance_cattle_dry_season,
      distance_pigs_rainy_season = hh_animals_rainy_season_distance_pigs,
      distance_pigs_dry_season = hh_animals_dry_season_distance_pigs,
      where_cattle_rainy_season = hh_animals_where_cattle_rainy_season,
      where_cattle_dry_season = hh_animals_where_cattle_dry_season,
      where_pigs_rainy_season = hh_animals_rainy_season_pigs,
      where_pigs_dry_season = hh_animals_dry_season_pigs,
      house_building_type = hh_main_building_type,
      main_energy_lighting = hh_main_energy_source_for_lighting,
      num_houses = hh_n_constructions,
      num_houses_sleep = hh_n_constructions_sleep
    )
  }

  odk_x_hh %>%
    mutate(`_id` = str_c('uuid:', `_id`)) %>%
    select(-hh_geo_location) %>%
    add_metadata_col('census') %>%
    write.csv(hh_csv_path, na = '', row.names = FALSE, fileEncoding = 'UTF-8')

  odk_x_member %>%
    rename(
      name = first_name,
      surname = last_name,
      id = pid,
      resident_status = member_resident
    ) %>%
    add_metadata_col('hh_member') %>%
    write.csv(member_csv_path, na = '', row.names = FALSE, fileEncoding = 'UTF-8')

  odk_x_hh_geo_location %>%
    add_metadata_col('hh_geo_location') %>%
    write.csv(geo_location_csv_path, na = '', row.names = FALSE, fileEncoding = 'UTF-8')

  if (full_migration) {
    odk_x_death %>%
      rename(
        hh_death_id = death_id,
        hh_death_name = death_name,
        hh_death_surname = death_surname,
        hh_death_gender = death_gender,
        hh_death_date = death_dod,
        hh_death_age = death_age
      ) %>%
      add_metadata_col('hh_death') %>%
      write.csv(death_csv_path, na = '', row.names = FALSE, fileEncoding = 'UTF-8')
  }
}

purge_odkx_server <- function(suitcase_dir, jar_file, server_url, user, pass, is_linux = FALSE){
  owd <- getwd()
  setwd(suitcase_dir)
  
  update_string <- 
    push_text <- paste0(
      "java -jar ", jar_file, " -cloudEndpointUrl '", server_url, "' -appId 'default' -username '", user, "' -password '", pass, "' -reset ",
      '  -dataVersion 2')
  message(update_string)
  if(!is_linux){
    update_string <- gsub("'", "", update_string)
  }
  system(update_string)
  setwd(owd)
}

upload_forms_odkx_server <- function(suitcase_dir, jar_file, server_url, table_id, user, pass, odkx_path, is_linux = FALSE){
  owd <- getwd()
  setwd(suitcase_dir)
  
  update_string <- 
    push_text <- paste0(
      "java -jar ", jar_file, " -cloudEndpointUrl '", server_url, "' -appId 'default' -username '", user, "' -password '", pass, "' -upload uploadOp RESET_APP -path '", odkx_path, "'  -dataVersion 2")
  message(update_string)
  if(!is_linux){
    update_string <- gsub("'", "", update_string)
  }
  system(update_string)
  setwd(owd)
}

update_odkx_data <- function(suitcase_dir, jar_file = jar_file, server_url, table_id, user, pass, update_path, is_linux = FALSE){
  owd <- getwd()
  setwd(suitcase_dir)
  
  update_string <- 
    push_text <- paste0(
      "java -jar ", jar_file, " -update -cloudEndpointUrl '", server_url, "' -appId 'default' -tableId '", table_id, "' -username '", user, "' -password '", pass, "' -path '", update_path, "' -updateLogPath '~/Desktop/log.txt' -dataVersion 2"
    )
  message(update_string)
  if(!is_linux){
    update_string <- gsub("'", "", update_string)
  }
  system(update_string)
  setwd(owd)
}

message('Loading minicensus data')
# load('minicensus_data.RData')

library(bohemia)


# Read in minicensus data
file_name <- paste0(country, '_mincensus_data.RData')
if(file_name %in% dir()){
  load(file_name)
} else {
  minicensus_data <- load_odk_data(the_country = country,
                          credentials_path = '../../credentials/credentials.yaml', # request from Databrew
                          users_path = '../../credentials/users.yaml', # request from Databrew
                          efficient = FALSE)
  save(minicensus_data,
       file = file_name)
}


# Loop through each instance / area
team_aggs <- c(2, 1, 3) # unique(team_df$team_agg)
n_teams <- length(team_aggs)
n_teams <- 3 
for(nn in team_aggs){
  out_list <- minicensus_data
  the_server <- paste0('https://sync-', nn, '.',
                       server_root)
  this_team_agg <- team_aggs[nn]
  only_hamlets <- gpx %>%
    filter(team_agg == this_team_agg) %>%
    .$code
  
  message(length(only_hamlets), ' HAMLETS --------------')
  guide_csv <- tibble(server = the_server,
                      code = sort(unique(only_hamlets)))
  write_csv(guide_csv, paste0('~/Desktop/hamlets_endpoint_',nn, '.csv' ))
  
  # Subset to specific hamlets
  if(!is.null(only_hamlets)){
    out_list$minicensus_main <- out_list$minicensus_main %>%
      filter(hh_hamlet_code %in% only_hamlets)
    iids <- out_list$minicensus_main$instance_id
    out_list$minicensus_people <- out_list$minicensus_people %>%
      filter(instance_id %in% iids)
    out_list$minicensus_repeat_death_info <- out_list$minicensus_repeat_death_info %>%
      filter(instance_id %in% iids)
    out_list$minicensus_repeat_hh_sub <- out_list$minicensus_repeat_hh_sub %>%
      filter(instance_id %in% iids)
    out_list$minicensus_repeat_mosquito_net <- out_list$minicensus_repeat_mosquito_net %>%
      filter(instance_id %in% iids)
    out_list$minicensus_repeat_water <- out_list$minicensus_repeat_water %>%
      filter(instance_id %in% iids)
  }
  
  # Decrypt names
  if(use_real_names){
    # out_list$enumerations$sub_name <- decrypt_private_data(out_list$enumerations$sub_name, keyfile = kf)
    # out_list$enumerations$chefe_name <- decrypt_private_data(out_list$enumerations$chefe_name, keyfile = kf)
    out_list$minicensus_repeat_death_info$death_name <- decrypt_private_data(out_list$minicensus_repeat_death_info$death_name, keyfile = kf)
    out_list$minicensus_repeat_death_info$death_surname <- decrypt_private_data(out_list$minicensus_repeat_death_info$death_surname, keyfile = kf)
    out_list$minicensus_people$first_name <- decrypt_private_data(out_list$minicensus_people$first_name, keyfile = kf)
    out_list$minicensus_people$last_name <- decrypt_private_data(out_list$minicensus_people$last_name, keyfile = kf)
    # out_list$va$id10007 <- decrypt_private_data(out_list$va$id10007, keyfile = kf)
    # out_list$va$id10017 <- decrypt_private_data(out_list$va$id10017, keyfile = kf)
    # out_list$va$id10018 <- decrypt_private_data(out_list$va$id10018, keyfile = kf)
    # out_list$va$id10061 <- decrypt_private_data(out_list$va$id10061, keyfile = kf)
    # out_list$va$id10062 <- decrypt_private_data(out_list$va$id10062, keyfile = kf)
  } else {
    # out_list$enumerations$sub_name <- fake_names(length(out_list$enumerations$sub_name))
    # out_list$enumerations$chefe_name <- fake_names(length(out_list$enumerations$chefe_name))
    out_list$minicensus_repeat_death_info$death_name <- fake_names(length(out_list$minicensus_repeat_death_info$death_name), words = 1)
    out_list$minicensus_repeat_death_info$death_surname <- fake_names(length(out_list$minicensus_repeat_death_info$death_surname), words = 1)
    out_list$minicensus_people$first_name <- fake_names(length(out_list$minicensus_people$first_name), words = 1)
    out_list$minicensus_people$last_name <- fake_names(length(out_list$minicensus_people$last_name), words = 1)
    # out_list$va$id10007 <- fake_names(length(out_list$va$id10007))
    # out_list$va$id10017 <- fake_names(length(out_list$va$id10017))
    # out_list$va$id10018 <- fake_names(length(out_list$va$id10018))
    # out_list$va$id10061 <- fake_names(length(out_list$va$id10061))
    # out_list$va$id10062 <- fake_names(length(out_list$va$id10062))
  }
  
  
  # Update names
  for(i in 1:length(names(out_list))){
    this_name <- names(out_list)[i]
    if(grepl('minicensus_', this_name)){
      new_name <- gsub('minicensus_', 'clean_minicensus_', this_name)
      names(out_list)[i] <- new_name
    }
  }
  
  
  # Write local csvs ready for upload to server
  format_to_odk_x(out_list = out_list, full_migration = FALSE, sample_hh = FALSE, truncate_name = FALSE)
  
  message('nn: ', nn, '. Sleeping 60 seconds --------------. About to purge')
  Sys.sleep(60)
  # Purge the database
  purge_odkx_server(suitcase_dir = suitcase_dir,
                    jar_file = jar_file,
                    server_url = the_server, 
                    user = creds$odkx_user, 
                    pass = creds$odkx_pass,
                    is_linux = is_linux)
  
  message('nn: ', nn, '. Sleeping 60 seconds --------------. About to upload')
  Sys.sleep(60)
  upload_forms_odkx_server(suitcase_dir = suitcase_dir,
                           jar_file = jar_file,
                           server_url = the_server, 
                           user = creds$odkx_user, 
                           pass = creds$odkx_pass,
                           odkx_path = odkx_path,
                           is_linux = is_linux)
  
  # Loop through each form and update
  the_tables <- c('hh_geo_location',
                  'hh_member',
                  'census')
  paths <- c('odk_x_geolocation.csv',
             'odk_x_member.csv',
             'odk_x_hh.csv')
  paths <- paste0(getwd(), '/', paths)
  
  message('nn: ', nn, '. Sleeping 60 seconds --------------. About to update')
  Sys.sleep(60)
  
  for(i in 1:length(the_tables)){
    this_table <- the_tables[i]
    this_path <- paths[i]
    x <- readr::read_csv(this_path)
    message('Updating ', this_table, '. NROWS: ', nrow(x), '. Going to sleep 60 seconds')
    Sys.sleep(60)
    update_odkx_data(suitcase_dir = suitcase_dir,
                     jar_file = jar_file,
                     server_url = the_server, 
                     table_id = this_table, 
                     user = creds$odkx_user, 
                     pass = creds$odkx_pass, 
                     update_path = this_path,
                     is_linux = is_linux)
  }
  
}


