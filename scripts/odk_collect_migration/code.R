library(dplyr)
library(bohemia)
library(gsheet)
library(readr)
library(babynames)


# 3 tables
# 1. Location hierarchy
# 2. Roster of all households
# 3. Roster of all individuals

# Get location hierarchy in the pulldata format
locations_data <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1hQWeHHmDMfojs5gjnCnPqhBhiOeqKWG32xzLQgj5iBY/edit#gid=640399777')
locations_data$hamlet_id <- locations_data$name_key
locations_data$name_key <- NULL

# Create fake households data
hh_list <- list()
for(i in 1:nrow(locations_data)){
  this_location <- locations_data$hamlet_id[i]
  these_ids <- paste0(this_location, '-', add_zero(1:100, n = 3))
  out <- tibble(hamlet_id = this_location,
                hh_id = these_ids,
                n_members = 5)
  hh_list[[i]] <- out
}
households_data <- bind_rows(hh_list)
households_data <- households_data %>% dplyr::rename(household_id = hh_id)
# Join with locations
households_data <-
  left_join(households_data,
            locations_data %>%
              dplyr::select(hamlet_id,
                            country, region, district,
                            ward, village, hamlet)) %>%
  arrange(hamlet_id)

# Get the person-level data
people_list <- list()
for(i in 1:nrow(households_data)){
  # message(i)
  this_household_code <- households_data$household_id[i]
  persons <- tibble(household_id = this_household_code,
                    person_id = paste0(this_household_code, '-', add_zero(1:5, n = 3)))
  persons <- bind_cols(persons,
                       babynames::babynames[i:(i+4),] %>%
                         dplyr::select(sex, first_name = name, dob = year)) %>%
    mutate(last_name = babynames::babynames$name[(i+5):(i+9)])
  people_list[[i]] <- persons
}
people_data <- bind_rows(people_list)
people_data$sex <- ifelse(people_data$sex, 'Male', 'Female')

# Add a field to households data which is minicensus roster
minicensus_roster <- people_data %>%
  mutate(full_name = paste0(first_name, ' ', last_name)) %>%
  group_by(household_id) %>%
  summarise(minicensus_roster = paste0(full_name, collapse = '; '))

# Join the roster to the households data
households_data <- left_join(households_data,
                             minicensus_roster)

# Modify names
people_data <- people_data %>%
  mutate(full_name = paste0(first_name, ' ', last_name)) %>%
  mutate(full_name_with_id = paste0(full_name, ' (', person_id, ')'))

# Write csvs and save
if(!dir.exists('odk_collect_migrations_files')){
  dir.create('odk_collect_migrations_files')
}
setwd('odk_collect_migrations_files')
write_csv(households_data, 'households_data.csv')
write_csv(locations_data, 'locations_data.csv')
write_csv(people_data, 'people_data.csv' )

zip(zipfile = '../metadata.zip',
    files = c('locations_data.csv',
              'households_data.csv',
              'people_data.csv'))
setwd('..')

