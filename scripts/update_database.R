#!/usr/bin/Rscript
start_time <- Sys.time()
message('System time is: ', as.character(start_time))
message('---Timezone: ', as.character(Sys.timezone()))
keyfile_path <- '../credentials/bohemia_pub.pem'
creds_fpath <- '../credentials/credentials.yaml'
creds <- yaml::yaml.load_file(creds_fpath)
suppressMessages({
  library(RPostgres)
  library(bohemia)
  library(yaml)
  library(dplyr)
}
)

is_local <- FALSE
drv <- RPostgres::Postgres()

if(is_local){
  con <- dbConnect(drv, dbname='bohemia')
} else {
  psql_end_point = creds$endpoint
  psql_user = creds$psql_master_username
  psql_pass = creds$psql_master_password
  con <- dbConnect(drv, dbname='bohemia', host=psql_end_point, 
                   port=5432,
                   user=psql_user, password=psql_pass)
}

id2 = NULL
skip_deprecated <- FALSE

moz <- tza <- TRUE
sys_hour <- lubridate::hour(Sys.time())
if(sys_hour %in% 7:10){
  moz <- FALSE
  message('Skipping Mozambique due to time of day (server down for maintenance)')
} 

if(tza){
  
  message('PULLING VA REFUSALS TANZANIA')
  for(id in c('varefusals')){
    message('Working on ', id)
    url <- creds$tza_odk_server
    user = creds$tza_odk_user
    password = creds$tza_odk_pass
    suppressWarnings({
      existing_uuids <- dbGetQuery(con, 'SELECT instance_id FROM va_refusals')
    })
    if (nrow(existing_uuids)< 0){
      existing_uuids <- c()
    } else {
      existing_uuids <- existing_uuids$instance_id
    }
    # Get data
    data <- odk_get_data(
      url = url,
      id = id,
      id2 = id2,
      unknown_id2 = FALSE,
      uuids = NULL,
      exclude_uuids = existing_uuids,
      user = user,
      password = password,
      pre_auth = TRUE,
      use_data_id = FALSE
    )
    new_data <- FALSE
    if(!is.null(data)){
      new_data <- TRUE
    }
    if(new_data){
      # Format data
      formatted_data <- format_va_refusals(data = data, keyfile = keyfile_path)
      # Update data
      update_va_refusals(formatted_data = formatted_data,
                        con = con)
    }
  }
  
  message('PULLING MINICENSUS TANZANIA')
  ############# SMALLCENSUSA TANZANIA
  for(id in c('smallcensusa', 'smallcensusb')){
    message('Working on ', id)
    url <- creds$tza_odk_server
    user = creds$tza_odk_user
    password = creds$tza_odk_pass
    suppressWarnings({
      existing_uuids <- dbGetQuery(con, 'SELECT instance_id FROM minicensus_main')
    })
    if (nrow(existing_uuids)< 0){
      existing_uuids <- c()
    } else {
      existing_uuids <- existing_uuids$instance_id
    }
    # Get data
    data <- odk_get_data(
      url = url,
      id = id,
      id2 = id2,
      unknown_id2 = FALSE,
      uuids = NULL,
      exclude_uuids = existing_uuids,
      user = user,
      password = password,
      pre_auth = TRUE,
      use_data_id = FALSE
    )
    new_data <- FALSE
    if(!is.null(data)){
      new_data <- TRUE
    }
    if(new_data){
      # Format data
      formatted_data <- format_minicensus(data = data, keyfile = keyfile_path)
      # Update data
      update_minicensus(formatted_data = formatted_data,
                        con = con)
    }
  }
  
  message('PULLING TANZANIA VA')
  ############### TANZANIA VA
  for(id in c('va153', 'va153b')){
    message('Working on ', id)
    url <- creds$tza_odk_server
    user = creds$tza_odk_user
    password = creds$tza_odk_pass
    suppressWarnings({
      existing_uuids <- dbGetQuery(con, 'SELECT instance_id FROM va')
    })
    if (nrow(existing_uuids)< 0){
      existing_uuids <- c()
    } else {
      existing_uuids <- existing_uuids$instance_id
    }
    # Get data
    data <- odk_get_data(
      url = url,
      id = id,
      id2 = id2,
      unknown_id2 = FALSE,
      uuids = NULL,
      exclude_uuids = existing_uuids,
      user = user,
      password = password,
      pre_auth = TRUE,
      use_data_id = FALSE
    )
    new_data <- FALSE
    if(!is.null(data)){
      new_data <- TRUE
    }
    if(new_data){
      # Format data
      formatted_data <- format_va(data = data, keyfile = keyfile_path)
      # Update data
      update_va(formatted_data = formatted_data,
                con = con)
    }
  }
  
  message('PULLING REFUSALS (TANZANIA)')
  # REFUSALS TANZANIA ######################################################################
  for(id in c('refusals', 'refusalsb')){
    message('working on ', id)
    url <- creds$tza_odk_server
    user = creds$tza_odk_user
    password = creds$tza_odk_pass
    suppressWarnings({
      existing_uuids <- dbGetQuery(con, 'SELECT instance_id FROM refusals')
    })
    if (nrow(existing_uuids)< 0){
      existing_uuids <- c()
    } else {
      existing_uuids <- existing_uuids$instance_id
    } 
    # Get data
    data <- odk_get_data(
      url = url,
      id = id,
      id2 = id2,
      unknown_id2 = FALSE,
      uuids = NULL,
      exclude_uuids = existing_uuids,
      user = user,
      password = password
    )
    new_data <- FALSE
    if(!is.null(data)){
      new_data <- TRUE
      message('---', nrow(data$non_repeats), ' new data points.')
    }
    if(new_data){
      # Format data
      formatted_data <- format_refusals(data = data)
      # Update data
      update_refusals(formatted_data = formatted_data,
                      con = con)
    }
  }
}

if(moz){
  message('PULLING MINICENSUS (MOZAMBIQUE')
  # MINICENSUS MOZAMBIQUE #######################################################################
  for(id in c('minicensus', 'minicensusb', 'smallcensus', 'smallcensusa', 'smallcensusb')){
    message('Working on ', id)
    url <- creds$moz_odk_server
    user = creds$moz_odk_user
    password = creds$moz_odk_pass
    suppressWarnings({
      existing_uuids <- dbGetQuery(con, 'SELECT instance_id FROM minicensus_main')
    })
    if (nrow(existing_uuids)< 0){
      existing_uuids <- c()
    } else {
      existing_uuids <- existing_uuids$instance_id
    }
    if(id == 'minicensusb'){
      id2 = 'minicensus'
      use_data_id = FALSE
    } else {
      id2 = NULL
      use_data_id = TRUE
    }
    if(id %in% c('smallcensus', 'smallcensusa', 'smallcensusb')){
      id2 = NULL
      use_data_id = FALSE
    }
    try({
      # Get data
      data <- odk_get_data(
        url = url,
        id = id,
        id2 = id2,
        unknown_id2 = FALSE,
        uuids = NULL,
        exclude_uuids = existing_uuids,
        user = user,
        password = password,
        pre_auth = TRUE,
        use_data_id = use_data_id,
        chunk_size = 50000
      )
      new_data <- FALSE
      if(!is.null(data)){
        new_data <- TRUE
      }
      if(new_data){
        # Format data
        formatted_data <- format_minicensus(data = data, keyfile = keyfile_path)
        # Update data
        update_minicensus(formatted_data = formatted_data,
                          con = con)
      }
    })
  }
  
  message('PULLING MOZAMBIQUE VA')
  ############### MOZAMBIQUE VA
  for(id in c('va153', 'va153b')){
    message('Working on ', id)
    url <- creds$moz_odk_server
    user = creds$moz_odk_user
    password = creds$moz_odk_pass
    suppressWarnings({
      existing_uuids <- dbGetQuery(con, 'SELECT instance_id FROM va')
    })
    if (nrow(existing_uuids)< 0){
      existing_uuids <- c()
    } else {
      existing_uuids <- existing_uuids$instance_id
    }
    # Get data
    new_data <- FALSE
    
    try({
      data <- odk_get_data(
        url = url,
        id = id,
        id2 = id2,
        unknown_id2 = FALSE,
        uuids = NULL,
        exclude_uuids = existing_uuids,
        user = user,
        password = password,
        pre_auth = TRUE,
        use_data_id = FALSE,
        chunk_size = 50000
      )
      if(!is.null(data)){
        new_data <- TRUE
      }
      if(new_data){
        # Format data
        formatted_data <- format_va(data = data, keyfile = keyfile_path)
        # Update data
        update_va(formatted_data = formatted_data,
                  con = con)
      }
    })
    
  }
  
  message('PULLING MOZAMBIQUE VA REFUSALS')
  ############### MOZAMBIQUE VA
  for(id in c('varefusals')){
    message('Working on ', id)
    url <- creds$moz_odk_server
    user = creds$moz_odk_user
    password = creds$moz_odk_pass
    suppressWarnings({
      existing_uuids <- dbGetQuery(con, 'SELECT instance_id FROM va_refusals')
    })
    if (nrow(existing_uuids)< 0){
      existing_uuids <- c()
    } else {
      existing_uuids <- existing_uuids$instance_id
    }
    # Get data
    new_data <- FALSE
    try({
      data <- odk_get_data(
        url = url,
        id = id,
        id2 = id2,
        unknown_id2 = FALSE,
        uuids = NULL,
        exclude_uuids = existing_uuids,
        user = user,
        password = password,
        pre_auth = TRUE,
        use_data_id = FALSE,
        chunk_size = 50000
      )
      if(!is.null(data)){
        new_data <- TRUE
      }
      if(new_data){
        # Format data
        formatted_data <- format_va_refusals(data = data, keyfile = keyfile_path)
        # Update data
        update_va_refusals(formatted_data = formatted_data,
                           con = con)
      }
    })
    
  }
  
  message('PULLING ENUMERATIONS (MOZAMBIQUE')
  # ENUMERATIONS MOZAMBIQUE######################################################################
  for(id in c('enumerations', 'enumerationsb')){
    message('Working on ', id)
    url <- creds$moz_odk_server
    user = creds$moz_odk_user
    password = creds$moz_odk_pass
    suppressWarnings({
      existing_uuids <- dbGetQuery(con, 'SELECT instance_id FROM enumerations')
    })
    if (nrow(existing_uuids)< 0){
      existing_uuids <- c()
    } else {
      existing_uuids <- existing_uuids$instance_id
    } 
    # Get data
    new_data <- FALSE
    try({
      data <- odk_get_data(
        url = url,
        id = id,
        id2 = id2,
        unknown_id2 = FALSE,
        uuids = NULL,
        exclude_uuids = existing_uuids,
        user = user,
        password = password,
        chunk_size = 50000
      )
      if(!is.null(data)){
        new_data <- TRUE
        # message('---', nrow(data$non_repeats), ' new data points.')
      }
      if(new_data){
        # Format data
        formatted_data <- format_enumerations(data = data, keyfile = keyfile_path)
        # Update data
        update_enumerations(formatted_data = formatted_data,
                            con = con)
      }
    })
    
  }
  
  message('PULLING REFUSALS (MOZAMBIQUE)')
  # REFUSALS MOZAMBIQUE######################################################################
  for(id in c('refusals', 'refusalsb')){
    message('Working on ', id)
    url <- creds$moz_odk_server
    user = creds$moz_odk_user
    password = creds$moz_odk_pass
    suppressWarnings({
      existing_uuids <- dbGetQuery(con, 'SELECT instance_id FROM refusals')
    })
    if (nrow(existing_uuids)< 0){
      existing_uuids <- c()
    } else {
      existing_uuids <- existing_uuids$instance_id
    } 
    # Get data
    new_data <- FALSE
    try({
      data <- odk_get_data(
        url = url,
        id = id,
        id2 = id2,
        unknown_id2 = FALSE,
        uuids = NULL,
        exclude_uuids = existing_uuids,
        user = user,
        password = password, 
        pre_auth = TRUE,
        chunk_size = 50000
      )
      if(!is.null(data)){
        new_data <- TRUE
        message('---', nrow(data$non_repeats), ' new data points.')
      }
      if(new_data){
        # Format data
        formatted_data <- format_refusals(data = data)
        # Update data
        update_refusals(formatted_data = formatted_data,
                        con = con)
      }
    })
    
  }
}

## TRACCAR LOCATIONS ##############################################

# get traccar data - one row per ID 
try({
  message('Syncing traccar workers')
  sync_workers_traccar(credentials = creds)
  
  # 
  message('Retrieving information on workers from traccar')
  dat <- get_traccar_data(url = creds$traccar_server,
                          user = creds$traccar_user,
                          pass = creds$traccar_pass)
  # 
  
  # Get existing max date
  max_date <- dbGetQuery(con, 'SELECT max(devicetime) FROM traccar')
  # fetch data only from one week prior to max date
  fetch_date <- max_date$max - lubridate::days(3)
  fetch_date <- as.character(as.Date(fetch_date))
  if(is.na(fetch_date)){
    fetch_date <- '2020-01-01'
  }
  fetch_path <- paste0("api/positions?from=", fetch_date, "T22%3A00%3A00Z&to=2022-12-31T22%3A00%3A00Z")
  
  message('Retrieving information on positions from traccar (takes a while)')
  library(dplyr)
  position_list <- list()
  for(i in 1:nrow(dat)){
    # message(i, ' of ', nrow(dat))
    this_id <- dat$id[i]
    unique_id <- dat$uniqueId[i]
    # message(i, '. ', this_id)
    suppressWarnings({
      suppressMessages({
        this_position <- bohemia::get_positions_from_device_id(url = creds$traccar_server,
                                                               user = creds$traccar_user,
                                                               pass = creds$traccar_pass,
                                                               device_id = this_id,
                                                               path = fetch_path) %>%
          mutate(unique_id = unique_id) %>%
          mutate(accuracy = as.numeric(accuracy),
                 altitude = as.numeric(altitude),
                 course = as.numeric(course),
                 deviceId = as.numeric(deviceId),
                 deviceTime = lubridate::as_datetime(deviceTime),
                 fixTime = lubridate::as_datetime(fixTime),
                 latitude = as.numeric(latitude),
                 longitude = as.numeric(longitude),
                 id = as.numeric(id))
      })
    })
    
    
    if(!is.null(this_position)){
      if(nrow(this_position) > 0){
        position_list[[i]] <- this_position
      }
    }
  }
  message('Finished retrieving positions. Combining...')
  positions <- bind_rows(position_list)
  message('Finished combining. Adding to database...')
  names(positions) <- tolower(names(positions))
  positions <- positions %>%
    dplyr::select(
      accuracy,
      altitude,
      course,
      deviceid,
      devicetime,
      id,
      latitude,
      longitude,
      valid ,
      unique_id)
  message('...', nrow(positions), ' positions retrieved from traccar server.')
  
  # Get existing ids
  existing_ids <- dbGetQuery(con, 'SELECT id FROM traccar')
  
  # Subset to remove those which are in existing ids
  if(nrow(existing_ids) > 0){
    existing_ids <- existing_ids$id
    message('...', length(existing_ids), ' positions already in database.')
    positions <- positions %>%
      filter(!id %in% existing_ids)
    message('...filtered. going to add ', nrow(positions), ' new positions to database.')
    
  }
  message('...going to add ', nrow(positions), ' positions to traccar table')
  # Update the database
  dbAppendTable(conn = con,
                name = 'traccar',
                value = positions)
  message('...done adding positions to traccar table.')
})



# #Execute cleaning code
# create_clean_db(credentials_file = '../credentials/credentials.yaml')
message('--- NOW EXECUTING CLEANING CODE ---')
source('clean_database.R')


####### ANOMALIES CREATION ##################################################
library(dplyr)
data_moz <- load_odk_data(the_country = 'Mozambique', 
                          credentials_path = '../credentials/credentials.yaml',
                          users_path = '../credentials/users.yaml',
                          local = is_local,
                          efficient = FALSE)
data_tza <- load_odk_data(the_country = 'Tanzania', 
                          credentials_path = '../credentials/credentials.yaml',
                          users_path = '../credentials/users.yaml',
                          local = is_local,
                          efficient = FALSE)
# Run anomaly detection
url <- 'https://docs.google.com/spreadsheets/d/1MH4rLmmmQSkNBDpSB9bOXmde_-n-U9MbRuVCfg_VHNI/edit#gid=0'
anomaly_and_error_registry <- gsheet::gsheet2tbl(url)
anomalies_moz <- identify_anomalies_and_errors(data = data_moz,
                                               anomalies_registry = anomaly_and_error_registry,
                                               locs = bohemia::locations)
anomalies_tza <- identify_anomalies_and_errors(data = data_tza,
                                               anomalies_registry =anomaly_and_error_registry,
                                               locs = bohemia::locations)
anomalies <- bind_rows(
  anomalies_moz %>% mutate(country = 'Mozambique'),
  anomalies_tza %>% mutate(country = 'Tanzania')
)
anomalies$date[nchar(anomalies$date) != 10] <- NA
anomalies$date <- as.Date(anomalies$date)

# Drop old anomalies and add these ones to the database
# however, we don't want to drop any old anomalies that have a correction already
# associated (since we want to give the site "credit" for that)
corrections <- dbGetQuery(conn = con, "SELECT * FROM corrections;")

new_anomalies <- anomalies %>% mutate(keep = TRUE)
already_anomalies <- dbGetQuery(conn = con, "SELECT * FROM anomalies;") %>%
  mutate(keep = id %in% corrections$id)

anomalies <- bind_rows(
  already_anomalies,
  new_anomalies
) %>%
  dplyr::distinct(id, .keep_all = TRUE) %>%
  filter(keep) %>%
  dplyr::select(-keep)

# anomalies <- bind_rows(
#   already_anomalies,
#   new_anomalies
# ) %>%
#   dplyr::distinct(id, .keep_all = TRUE)
# keep_these <- paste0('(', paste0("'", corrections$id, "'", collapse = ', '), ')', collapse = '')
# dbExecute(conn = con,
#           statement = paste0('DELETE FROM anomalies WHERE id NOT IN ', keep_these, ';'))
# 
# already_in <- dbGetQuery(conn = con, 'select id from anomalies;')
# 
# anomalies <- anomalies %>% filter(!duplicated(id),
#                                   !id %in% already_in$id) # need to check on this!


dbWriteTable(conn = con,
             name = 'anomalies',
             value = anomalies,
             overwrite = TRUE)

# See how many corrections are pending fixes

# get corrections and fixes.
anomalies <- dbGetQuery(con, 'select * from anomalies')
corrections <- dbGetQuery(con, 'select * from corrections')
fixes <- dbGetQuery(con, 'select * from fixes')
fixes_ad_hoc <- dbGetQuery(con, 'select * from fixes_ad_hoc')

#get ids from fixes 
fixes_ids <- fixes$id

# keep only response_details, id, and instance_id
pending_for_databrew <- corrections %>% filter(!id %in% fixes_ids) %>% select(id, instance_id, response_details)
done_by_databrew <- corrections %>% filter(id %in% fixes_ids) %>% select(id, instance_id, response_details)

corrected <- left_join(anomalies, corrections %>% dplyr::filter(!duplicated(id))) %>% filter(!is.na(resolved_by))
pending_correction <- left_join(anomalies, corrections %>% dplyr::filter(!duplicated(id))) %>% filter(is.na(resolved_by))

message(paste0(nrow(anomalies), ' total anomalies'),
        paste0('\n---MOZ: ', nrow(anomalies[anomalies$country == 'Mozambique',])),
        paste0('\n---TZA: ', nrow(anomalies[anomalies$country == 'Tanzania',])),
        paste0('\n------'),
        
        paste0('\n',nrow(corrected), ' total corrections submitted by sites'),
        paste0('\n---MOZ: ', nrow(corrected[corrected$country == 'Mozambique',])),
        paste0('\n---TZA: ', nrow(corrected[corrected$country == 'Tanzania',])),
        paste0('\n---', nrow(done_by_databrew), ' total corrections already implemented by Databrew'),
        paste0('\n---', nrow(pending_for_databrew), ' total corrections waiting for Databrew to implement'),
        paste0('\n------'),
        
        paste0('\n', nrow(pending_correction), ' total corrections requiring site input'),
        paste0('\n---MOZ: ', nrow(pending_correction[pending_correction$country == 'Mozambique',])),
        paste0('\n---TZA: ', nrow(pending_correction[pending_correction$country == 'Tanzania',])),
        paste0('\n------'),
        paste0('\n', nrow(fixes_ad_hoc), ' ad-hoc changes implemented (non-anomalies).')
)


replace_local_files <- FALSE
if(replace_local_files){
  remove_file <- '/tmp/Mozambique_efficient_local.RData'
  if(file.exists(remove_file)){
    file.remove(remove_file)
  }
  remove_file <- '/tmp/Tanzania_efficient_local.RData'
  if(file.exists(remove_file)){
    file.remove(remove_file)
  }
  
  use_cached <- TRUE
  for(the_country in c('Mozambique', 'Tanzania')){
    out <- load_odk_data(local = is_local, the_country = the_country, efficient = TRUE, use_cached = use_cached, con = con)
  }
}


x = dbDisconnect(con)



end_time <- Sys.time()
message('Done at : ', as.character(Sys.time()))
time_diff <- end_time - start_time
message('That took ', as.character(round(as.numeric(time_diff), 2)), ' ', attr(time_diff, 'units'))

# Look at anomalies
x = anomalies %>%
  filter(description == "A minicensus form cannot be entered prior to June 2020 or after today's date")

