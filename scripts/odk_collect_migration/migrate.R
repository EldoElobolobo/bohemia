library(dplyr)
library(bohemia)
library(gsheet)
library(readr)
library(babynames)
country <- 'Mozambique'
use_real_names <- TRUE # whether to decrypt names (TRUE) or use fakes ones (false)
kf <- '../../credentials/bohemia_priv.pem' #path to private key for name decryption

# 4 tables
# 'locations_data.csv',
###########   hamlet    village ward  district  region country  hamlet_id

# 'households_data.csv',
###########   hamlet_id household_id n_members country   region   district ward       village hamlet    minicensus_roster                                   
# 'people_data.csv',
###########   household_id person_id   sex    first_name   dob last_name full_name        full_name_with_id             

# 'full_roster.csv'
###########     list_name   name_key    label                         

# Load minicensus data
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
out_list <- minicensus_data

# Make location hierarchy just for data from minicensus
locations <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1hQWeHHmDMfojs5gjnCnPqhBhiOeqKWG32xzLQgj5iBY/edit#gid=640399777')
ll <- minicensus_data$minicensus_main %>%
  group_by(hamlet_id = hh_hamlet_code) %>%
  tally %>%
  dplyr::select(-n) %>%
  left_join(locations, by = c('hamlet_id' = 'name_key'))
locations_data <- ll

# Households data
hh <-  minicensus_data$minicensus_main %>%
  dplyr::select(hamlet_id = hh_hamlet_code,
                household_id = hh_id,
                instance_id) %>%
  left_join(locations, by = c('hamlet_id' = 'name_key'))
# Get n_members and minicensus_roster
people <- minicensus_data$minicensus_people %>%
  mutate(sex = Hmisc::capitalize(gender)) %>%
  mutate(dob = substr(minicensus_data$minicensus_people$dob, 1, 4)) %>%
  mutate(first_name = decrypt_private_data(first_name, keyfile = kf)) %>%
  mutate(last_name = decrypt_private_data(last_name, keyfile = kf)) %>%
  # mutate(full_name = paste0(first_name, ' ', last_name)) %>%
  # mutate(full_name_with_id = paste0(full_name, ' (',
  #                                   pid,
  #                                   ')')) %>%
  mutate(full_name = paste0(first_name, ';', last_name)) %>%
  mutate(full_name_with_id = paste0(full_name, '|ID:', pid, '|',
                                    sex, '|', dob)) %>%
  mutate(name_key = gsub(' ', '.', full_name_with_id, fixed = TRUE))

right <- people %>%
  group_by(instance_id) %>%
  summarise(n_members = n(),
            minicensus_roster = paste0(full_name_with_id, collapse = '\n'))
hh <- left_join(hh, right)
hh <- hh %>% dplyr::select(-instance_id)
households_data <- hh
households_data <- households_data %>%
  arrange(hamlet_id, household_id) %>%
  filter(!duplicated(household_id))

# people data
#   household_id person_id   sex    first_name   dob last_name full_name        full_name_with_id    
people_data <- people %>%
  mutate(household_id = substr(pid, 1, 7)) %>%
  dplyr::select(person_id = pid,
                household_id,
                sex = gender,
                first_name,
                dob,
                last_name,
                full_name,
                full_name_with_id,
                name_key)
people_data$dob <- as.numeric(people_data$dob)
people_data <- people_data %>%
  arrange(person_id) %>%
  filter(!duplicated(person_id))

# Get searcher of full roster
full_roster <- people_data %>%
  mutate(list_name = 'full_roster') %>%
  mutate(name_key = person_id,
         label = full_name_with_id) %>%
  dplyr::select(list_name, name_key, label)

# Write csvs and save
the_dir <- paste0('odk_collect_migrations_files_', country)
if(!dir.exists(the_dir)){
  dir.create(the_dir)
}
setwd(the_dir)
write_csv(households_data, 'households_data.csv')
write_csv(locations_data, 'locations_data.csv')
write_csv(people_data, 'people_data.csv' )
write_csv(full_roster, 'full_roster.csv')


# households_data <- read_csv('households_data.csv')
# locations_data <- read_csv('locations_data.csv')
# people_data <- read_csv('people_data.csv')

zip(zipfile = 'metadata.zip',
    files = c('locations_data.csv',
              'households_data.csv',
              'people_data.csv',
              'full_roster.csv'
              ))
setwd('..')

