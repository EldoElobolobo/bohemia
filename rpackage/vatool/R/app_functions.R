library(shiny)
library(dplyr)
library(RPostgres)
library(DBI)

# # load VA data (for now, this is fake)
load_va_data <- function(is_local = FALSE,
                         use_cached = TRUE,
                         credentials_path = 'credentials/credentials.yaml'){
  
  if(use_cached){
    if(file.exists('/tmp/va.RData')){
      load('/tmp/va.RData')
      get_new <- FALSE
    } else {
      get_new <- TRUE
    }
  }
  # if(file.exists('../data-raw/va.csv')){
  #   out <- read.csv('../data-raw/va.csv')
  # } else {
  #   stop('YOU NEED TO DOWNLOAD va.csv INTO data-raw. Get from https://trello.com/c/75qsyxWu/2368-bohemia-va-tool-create-functioning-tool')
  # }
  drv <- RPostgres::Postgres()
  if(get_new){
    if(is_local){

      con <- dbConnect(drv = drv,
                       dbname = 'bohemia')
    } else {
      creds <- yaml::yaml.load_file(credentials_path)
      psql_end_point = creds$endpoint
      psql_user = creds$psql_master_username
      psql_pass = creds$psql_master_password
      con <- dbConnect(drv, dbname='bohemia', host=psql_end_point,
                       port=5432,
                       user=psql_user, password=psql_pass)
    }
    out <- dbReadTable(conn = con, name = 'va')
    dbDisconnect(con)
    save(out, file = '/tmp/va.RData')
  }
  return(out)
}

# function for getting readable names

# select_names <- va_survey$name[grepl('select', va_survey$type)]
# select_names <- select_names[!is.na(select_names)]

get_va_names <- function(va_data, country = 'Tanzania'){
  col_names <- names(va_data)
  for(i in 1:length(col_names)){
    this_col <- col_names[i]
    if(any(this_col==tolower(va_names$name))){
      name_index <- which(this_col ==tolower(va_names$name))
      if(country == 'Tanzania'){
        names(va_data)[i] <- as.character(va_names$label_english[name_index])
      } else {
        if(!is.na(as.character(va_data[i])) & names(va_data)[i] %in% tolower(select_names)){
          temp <- unique(va_choices$`label::Portuguese`[as.character(va_data[i]) ==va_choices$name])
          if(length(temp)>1){
            ct <- nchar(temp) - nchar(va_data[i])
            this_index <- which(ct == min(ct, na.rm = T))[1]
            temp <- temp[this_index]
          }
          va_data[i] <- temp
        }
        names(va_data)[i] <- as.character(va_names$label_portuguese[name_index])

      }
    }
  }
  return(va_data)
}


# Random date function
random_date <- function(n = 10000){
  sample(Sys.Date() - n:
           Sys.Date(),
         1)
}

# Function for cause of death choices
cod_choices <- function(country){
  if(country == 'Mozambique'){
    icd_data <- vatool::icd_data_moz
  } else {
    icd_data <- vatool::icd_data_tza
  }
  icd_codes <- icd_data$icd_codes
  icd_names <- icd_data$icd_names
  choices <- icd_codes
  names(choices) <- paste0( icd_codes, ' ',icd_names)
  return(choices)
}


# create data to match codes and names
cod_data <- function(country){
  if(country == 'Mozambique'){
    icd_data <- vatool::icd_data_moz
  } else {
    icd_data <- vatool::icd_data_tza
  }
  icd_codes <- icd_data$icd_codes
  icd_names <- icd_data$icd_names
  dat <- tibble(cod_code = icd_codes, cod_names = icd_names)
  return(dat)
}


# Function for generating the about page
make_about <- function(){
  fluidPage(
    fluidRow(
      div(img(src='www/logo.png', align = "center"), style="text-align: center;"),
      h4('Built in partnership with ',
         a(href = 'http://databrew.cc',
           target='_blank', 'Databrew'),
         align = 'center'),
      p('Empowering research and analysis through collaborative data science.', align = 'center'),
      div(a(actionButton(inputId = "email", label = "info@databrew.cc",
                         icon = icon("envelope", lib = "font-awesome")),
            href="mailto:info@databrew.cc",
            align = 'center')),
      style = 'text-align:center;'
    )
  )
}

# Plot theme
theme_va <- ggplot2::theme_bw

# Get database connection
get_db_connection <- function(local = FALSE){
  creds <- yaml::yaml.load_file('credentials/va_tool_credentials.yaml')
  # users <- yaml::yaml.load_file('credentials/users.yaml')
  drv <- RPostgres::Postgres()
  
  if(local){
    con <- dbConnect(drv, dbname = 'vadb')
  } else {
    psql_end_point = creds$e
    psql_user = creds$u
    psql_pass = creds$p
    con <- dbConnect(drv, 
                     dbname='vadb', 
                     host=psql_end_point,
                     port=5432,
                     user=psql_user, 
                     password=psql_pass)
  }
  
  return(con)
}
