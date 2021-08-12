
# Get data from odk server
getEntoDataB4 = function(){
  require(readr)
  
  briefcase_dir <- "."
  jar_file_briefcase <- 'ODK-Briefcase-v1.18.0'
  
  # Make a temporary directory
  odk_server = Sys.getenv("databrew_odk_server")
  odk_user = Sys.getenv("databrew_odk_user")
  odk_password = Sys.getenv("databrew_odk_pass")
  
  temp_dir <- tempdir()
  
  old_file = paste0(temp_dir, "/entob3.csv")
  if(file.exists(old_file)){
    file.remove(old_file)
    cat("[INFO]...Removing old entob3 file...")
  }else{
    cat("[INFO]..Previous file not available...")
  }
  
  
  # Retrieve the data
  odkr::pull_remote(target = paste0(briefcase_dir),
              briefcase = jar_file_briefcase,
              id = "entob3",
              to = temp_dir,
              from = odk_server,
              username = odk_user,
              password = odk_password)
  
  # Export the data as csv
  odkr::export_data(target = briefcase_dir,
              briefcase = jar_file_briefcase,
              id = "entob3",
              from = temp_dir,
              to = temp_dir,
              filename = "entob3.csv")
  
  entob3_files <- dir(temp_dir)
  entob3_files <- entob3_files[grepl('entob3', entob3_files)]
  
  
  entob3_list = lapply(entob3_files, function(x) {
    readr::read_csv(file.path(temp_dir, x))
  })
  
  
  names(entob3_list) <- gsub('.csv', '', entob3_files, fixed = TRUE)
  entob3 <- entob3_list
  
  # # Read in the csv
  entob3_df <- suppressWarnings(readr::read_csv(file.path(temp_dir, 'entob3.csv'))) 
  
  
  #entob3 <- do.call("rbind",entob3_list)
  
  b4 <- entob3_df %>% dplyr::filter_at(dplyr::vars(contains("survival")),
                                dplyr::any_vars(. %in% "Survival"))%>%
    dplyr::select(SubmissionDate,hh_id_manual,note_indoors,start_time,end_time)
  
  if(nrow(b4)>= 1){
    
    df = data.frame("Date" = rep(lubridate::mdy_hms(b4$SubmissionDate),each = 3),
                    "HH ID" = rep(b4$hh_id_manual, each = 3),
                    "Indoors or Outdoors" = rep(c("indoors"), each = 3),
                    "Time period" = rep(as.integer(lubridate::mdy_hms(b4$end_time) - lubridate::mdy_hms(b4$start_time)),each=3),
                    "Species" = rep(c("An gambiae", "An funestus","An.Other (Specify)"), nrow(b4)))
    edit_cols = data.frame(matrix(ncol = 16, nrow = nrow(df)))
    entob3_df = cbind(df,edit_cols)
  }else{
    df <- data.frame(
      "Date" = as.Date(character()),
      "HH ID" = character(),
      "Indoors or Outdoors" = character(),
      "Time Period" = numeric(),
      "Species" = character(),
      check.names = FALSE
    )
    
    edit_cols = data.frame(matrix(ncol = 16, nrow = nrow(df)))
    entob3_df = cbind(df,edit_cols)
  }
  
  
  
  
  
  names(entob3_df)[-c(1:5)] = c("D1_dead_mosquitoes_N",
                                "D1_dead_mosquitoes_QR",
                                "D2_dead_mosquitoes_N",
                                "D2_dead_mosquitoes_QR",
                                "D3_dead_mosquitoes_N",
                                "D3_dead_mosquitoes_QR",
                                "D4_dead_mosquitoes_N",
                                "D4_dead_mosquitoes_QR",
                                "D5_dead_mosquitoes_N",
                                "D5_dead_mosquitoes_QR",
                                "D6_dead_mosquitoes_N",
                                "D6_dead_mosquitoes_QR",
                                "D7_dead_mosquitoes_N",
                                "D7_dead_mosquitoes_QR",
                                "alive_at_D7_mosquitoes_N",
                                "alive_at_D7_mosquitoes_QR")
  
  return(entob3_df)
}
