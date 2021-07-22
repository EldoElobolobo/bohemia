# Define the country
country <- 'Mozambique'
if(country == 'Mozambique'){
  iso <- 'MOZ'
} else {
  iso <- 'TZA'
}

# Define some parameters for ODK-X retrieval
briefcase_dir <- '/home/joebrew/Documents/briefcase'
download_dir <- '/home/joebrew/Documents/odkx_storage'

briefcase_storage_dir <- download_dir <-
  paste0(download_dir, '/', iso)

jar_file_briefcase <- 'ODK-Briefcase-v1.18.0.jar'
is_linux <- Sys.info()['sysname'] == 'Linux'
keyfile = '../../credentials/bohemia_priv.pem'
keyfile_public = '../../credentials/bohemia_pub.pem'


library(readr)
library(aws.s3)
library(dplyr)
library(tidyr)
library(stringr)
library(sp)
library(babynames)
library(bohemia)

# Configure AWS bucket info

# Read in credentials for S3 bucket
s3creds <- read_csv('../../credentials/bohemiacensuss3credentials.csv')

# Read in credentials for ODK Aggregate server
odk_collect_creds <- yaml::yaml.load_file('../../credentials/credentials.yaml')

# Set environment variables for AWS s3
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = s3creds$`Access key ID`,
  "AWS_SECRET_ACCESS_KEY" = s3creds$`Secret access key`,
  "AWS_DEFAULT_REGION" = "eu-west-3"
)

# # Get bucket details
# get_bucket(bucket = 'bohemiacensus')


# Define some parameters for ODK-X retrieval
jar_file_briefcase <- 'ODK-Briefcase-v1.18.0.jar'

# Check the directory
this_dir <- getwd()
split_dir <- unlist(strsplit(this_dir, split = '/'))
check <- split_dir[length(split_dir)] == 'incidents_dashboard' & split_dir[length(split_dir) - 1] == 'analyses' 
if(!check){
  message('YOU ARE IN THE WRONG DIRECTORY. MAKE SURE YOU ARE IN bohemia/scripts')
}

# Retrieve the ODK Aggregate forms
isol <- tolower(iso)
url <- odk_collect_creds[paste0(isol, '_odk_server')]
user <- odk_collect_creds[paste0(isol, '_odk_user')]
password <- odk_collect_creds[paste0(isol, '_odk_pass')]

# Function for pulling briefcase stuff
get_data_briefcase <- function(url,
                               id,
                               user,
                               password,
                               briefcase_dir,
                               jar_file_briefcase,
                               briefcase_storage_dir,
                               dry_run = FALSE){
  owd <- getwd()
  setwd(briefcase_dir)
  cli_text <- paste0(
    'java -Xms512m -Xmx16g -jar ',
    jar_file_briefcase, ' --pull_aggregate',
    ' -U ', url,
    ' -u ', user,
    ' -p ', password,
    ' -id ', id,
    ' -sd ', briefcase_storage_dir,
    ' -e ',
    ' -ed ', briefcase_storage_dir,
    ' -ef ', paste0(id, '.csv')
  )
  message(cli_text)
  if(!dry_run){
    system(cli_text)
  }
  setwd(owd)
}

# Get refusals, etc.
ids <- c('incidents')
for(i in 1:length(ids)){
  id <- ids[i]
  get_data_briefcase(
    url = url,
    id = id,
    user = user,
    password = password,
    briefcase_dir = briefcase_dir,
    briefcase_storage_dir = briefcase_storage_dir,
    jar_file_briefcase = jar_file_briefcase,
    dry_run = FALSE
  )
}

# Read in all tables for ODK Aggregate
this_id <- 'incidents'
this_path <- paste0(file.path(briefcase_storage_dir, this_id), '.csv')
incidents <- read_csv(this_path)

# Put the objects into S3
temp_dir <- tempdir()

file_name <- paste0('incidents_', country, '_',
                    as.character(as.character(Sys.time())),
                    '.RData')
full_path <- file.path(temp_dir, file_name)
save(incidents, file = full_path)
s3_path <- paste0('incidents/', file_name)
put_object(
  file = full_path,
  object = s3_path,
  bucket = "bohemiacensus"
)

buck <- get_bucket(bucket = 'bohemiacensus',
                   prefix = 'incidents/')
message(length(buck), ' objects in the bucket')


