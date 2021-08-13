


getEntoData <- function(){
  require(readr)
  require(magrittr)
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
                    id = "entoa3",
                    to = temp_dir,
                    from = odk_server,
                    username = odk_user,
                    password = odk_password)
  
  # Export the data as csv
  odkr::export_data(target = briefcase_dir,
                    briefcase = jar_file_briefcase,
                    id = "entoa3",
                    from = temp_dir,
                    to = temp_dir,
                    filename = "entoa3.csv",
                    overwrite = TRUE)
  
  entoc3_files <- dir(temp_dir)
  entoc3_files <- entoc3_files[grepl('entoa3', entoc3_files)]
  
  
  entoc3_list <- lapply(entoc3_files, function(x) {
    read_csv(file.path(temp_dir, x))
  })
  
  
  names(entoc3_list) <- gsub('.csv', '', entoc3_files, fixed = TRUE)
  entoc3 <- entoc3_list
  
  # # Read in the csv
  entoa3_df <- suppressWarnings(readr::read_csv(file.path(temp_dir, 'entoa3.csv')))
  
  #function to preprocess entoc3 data
  a4_ind_mosq <- function(df) {
    
    n_diss <- df %>% dplyr::select_at(dplyr::vars(contains("how_many_dis_")))%>%colnames()
    
    a4 <-df %>%
      dplyr::mutate(n_dissected = rowSums(.[grepl("how_many_dis_", names(.))], na.rm = TRUE),
             Cluster = ifelse(site == "Household",hh_id,ifelse(site == "Livestock_Enclosure",le_id,site)))%>%
      dplyr::select(todays_date,site,paste0(n_diss), Cluster, n_dissected)
    
    if(nrow(a4) >= 1){
      a4_df <- a4[rep(seq(nrow(a4)), a4$n_dissected),]
      
      repl <- a4 %>%dplyr::select_at(dplyr::vars(contains("how_many_dis_")))%>%
        replace(is.na(.),0)%>%
        setNames(NULL)%>%
        t()%>%
        as.vector()
      
      Species = rep(c("gambiae","funestus"),length(repl)/2)
      
      cmb <- data.frame(Species,repl)
      new_df <- cmb[rep(seq(nrow(cmb)), cmb$repl),]
      
      a4 <- cbind(a4_df, new_df)%>%
        dplyr::select(todays_date,Cluster,site,Species)
    }else{
      a4 <- a4 %>%dplyr::select(todays_date,Cluster,site)
      a4<- data.frame(a4, "Species" = character())
    }
    
    a4$n_dissected = NULL
    temp_df <- data.frame(matrix(ncol = 3, nrow = nrow(a4)))
    A4df <- cbind(a4,temp_df)
    names(A4df) <- c("Date",
                     "Household/Livestock ID",
                     "Collection Location (OR)",
                     "Species",
                     "Parity_Status",
                     "Wing-length (micrometers)",
                     "Sample tube ID and link to QR code")
    
    A4df$`Wing-length (micrometers)` =  as.numeric(A4df$`Wing-length (micrometers)`)
    A4df$`Sample tube ID and link to QR code` = as.character(A4df$`Sample tube ID and link to QR code`)
    
    return(A4df)
  }
  
  output <- a4_ind_mosq(entoa3_df)
  
  return(output)
}
