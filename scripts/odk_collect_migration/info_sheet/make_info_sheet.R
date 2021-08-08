library(dplyr)
library(bohemia)
library(gsheet)
library(readr)
library(babynames)
country <- 'Mozambique'
use_real_names <- TRUE # whether to decrypt names (TRUE) or use fakes ones (false)
kf <- '../../../credentials/bohemia_priv.pem' #path to private key for name decryption

# Define function for extracting geolocation
extract_ll <- function(x){
  splat <- strsplit(x, ' ')
  lat <- as.numeric(unlist(lapply(splat, function(z){z[1]})))
  lng <- as.numeric(unlist(lapply(splat, function(z){z[2]})))
  tibble(lng, lat)
}


# Load minicensus data
file_name <- paste0(country, '_mincensus_data.RData')
if(file_name %in% dir()){
  load(file_name)
} else {
  minicensus_data <- load_odk_data(the_country = country,
                                   credentials_path = '../../../credentials/credentials.yaml', # request from Databrew
                                   users_path = '../../../credentials/users.yaml', # request from Databrew
                                   efficient = FALSE)
  save(minicensus_data,
       file = file_name)
}
out_list <- minicensus_data

# Define the households or hamlets for which you would like info sheets
# (leave both null if you want all)
hh_ids <- NULL
hamlet_ids <- NULL

if(is.null(hamlet_ids)){
  if(is.null(hh_ids)){
    hh_ids <- sort(unique(minicensus_data$minicensus_main$hh_id))
  }
} else {
  hh_ids <- sort(unique(minicensus_data$minicensus_main$hh_id[substr(minicensus_data$minicensus_main$hh_id, 1, 3) %in% c(hamlet_ids)]))
}

# Prepare the data
# Make location hierarchy just for data from minicensus
locations <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1hQWeHHmDMfojs5gjnCnPqhBhiOeqKWG32xzLQgj5iBY/edit#gid=640399777')
ll <- minicensus_data$minicensus_main %>%
  group_by(hamlet_id = hh_hamlet_code) %>%
  tally %>%
  dplyr::select(-n) %>%
  left_join(locations, by = c('hamlet_id' = 'name_key'))
locations_data <- ll


# Households data
hhx <-  minicensus_data$minicensus_main %>%
  dplyr::select(hamlet_id = hh_hamlet_code,
                household_id = hh_id,
                hh_geo_location,
                instance_id,
                hh_contact_info_number) %>%
  filter(household_id %in% hh_ids) %>%
  left_join(locations, by = c('hamlet_id' = 'name_key'))


# Write csvs and save
the_dir <- paste0('info_sheets_', country)
if(!dir.exists(the_dir)){
  dir.create(the_dir)
}
for(j in 1:nrow(hhx)){
  this_hh <- hhx$household_id[j]
  rmarkdown::render(input = 'info_sheet_template.Rmd',
                    # output_file = paste0(this_hh, '.pdf'),
                    # output_dir = the_dir,
                    params = list(hh_id = this_hh))  
}

