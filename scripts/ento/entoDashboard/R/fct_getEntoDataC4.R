

# Get C4 data from C3 form
getEntoDataC4 <- function(){
  require(readr)
  briefcase_dir <- "."
  jar_file_briefcase <- 'ODK-Briefcase-v1.18.0'
  
  
  # Make a temporary directory
  odk_server = Sys.getenv("databrew_odk_server")
  odk_user = Sys.getenv("databrew_odk_user")
  odk_password = Sys.getenv("databrew_odk_pass")
  
  temp_dir <- tempdir()
  
  # Retrieve the data
  odkr::pull_remote(target = paste0(briefcase_dir),
              briefcase = jar_file_briefcase,
              id = "entoc3",
              to = temp_dir,
              from = odk_server,
              username = odk_user,
              password = odk_password)
  
  # Export the data as csv
  odkr::export_data(target = briefcase_dir,
              briefcase = jar_file_briefcase,
              id = "entoc3",
              from = temp_dir,
              to = temp_dir,
              filename = "entoc3.csv",
              overwrite = TRUE)
  
  entoc3_files <- dir(temp_dir)
  entoc3_files <- entoc3_files[grepl('entoc3', entoc3_files)]
  
  
  entoc3_list <- lapply(entoc3_files, function(x) {
    read_csv(file.path(temp_dir, x))
  })
  
  
  names(entoc3_list) <- gsub('.csv', '', entoc3_files, fixed = TRUE)
  entoc3 <- entoc3_list
  
  # # Read in the csv
  entoc4_df <- suppressWarnings(readr::read_csv(file.path(temp_dir, 'entoc3.csv')))
  
  #function to preprocess entoc3 data
  c4_ind_mosq <- function(df){
    
    n_var <- df %>%
      dplyr::select_at(dplyr::vars(contains("_kept_survival")))
    c4 <- df %>%
      dplyr::mutate(num_survive = rowSums(.[grepl("_kept_survival", names(.))], na.rm = TRUE))%>%
      dplyr::select(todays_date,household_id,num_survive)%>%
      dplyr::mutate("Collection Location" = rep("Indoors",nrow(df)))
    
    if (nrow(c4) >= 1){
      c4_rep <- c4[rep(seq(nrow(c4)), c4$num_survive),]
      repl <- df %>%dplyr::select_at(dplyr::vars(contains("_kept_survival")))%>%
        replace(is.na(.),0)%>%
        setNames(NULL)%>%
        t()%>%
        as.vector()
      
      Species <- rep(c("gambiae","funestus"),nrow(c4))
      cmb = data.frame(Species, repl)
      cmb_rep <- cmb[rep(seq(nrow(cmb)), cmb$repl),]
      
      c4 <- cbind(c4_rep,cmb_rep)%>%
        dplyr::select(todays_date,household_id,`Collection Location`)
    }
    
    
    c4$num_survive = NULL
    edit_cols <- data.frame(matrix(ncol = 16, nrow = nrow(c4)))
    entoc4_df <- cbind(c4,edit_cols)
    names(entoc4_df) <- c("Date",
                          "HH ID",
                          "Collection Location",
                          "Life_History_O_D1",
                          "Life_History_O_D2",
                          "Life_History_O_D3",
                          "Life_History_O_D4",
                          "Life_History_O_D5",
                          "Life_History_O_D6",
                          "Life_History_O_D7",
                          "Life_History_D_D1",
                          "Life_History_D_D2",
                          "Life_History_D_D3",
                          "Life_History_D_D4",
                          "Life_History_D_D5",
                          "Life_History_D_D6",
                          "Life_History_D_D7",
                          "Species Complex",
                          "Sample tude ID and link to QR Code")
    
    return(entoc4_df)
  }
  
  # Geting data into the right shape for C4 dashboard.
  output <- c4_ind_mosq(entoc4_df)
  
  return(output)
}
