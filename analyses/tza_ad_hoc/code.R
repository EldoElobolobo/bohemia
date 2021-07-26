library(dplyr)
library(DBI)
library(RPostgres)
library(yaml)
library(readr)

creds <- yaml::yaml.load_file('../../credentials/credentials.yaml') 

# Connect to database
drv <- RPostgres::Postgres()
con <- dbConnect(drv = drv,
                 dbname = 'bohemia')

# Read in data
anomalies <- dbReadTable(conn = con, 'anomalies')
corrections <- dbReadTable(conn = con, 'corrections')
write_csv(anomalies, '~/Desktop/anomalies.csv')
write_csv(corrections, '~/Desktop/corrections.csv')
clean_minicensus_main <- dbReadTable(conn = con, 'clean_minicensus_main')
people <- dbReadTable(conn = con, 'clean_minicensus_people')
minicensus_main <- dbReadTable(conn = con, 'minicensus_main')


# KAI-041
pd <- clean_minicensus_main %>%
  filter(hh_id == 'KAI-041')

# KAI-042
pd <- clean_minicensus_main %>%
  filter(hh_id == 'KAI-042')
# KKP-009
# SAL-093
# KWL-260
# KWL-261
# KWL-262
# KWL-263
# KWL-264
# KWL-265
# KWL-266
# KWL-267
# KWL-268
# KWL-269
# KWL-270

dbDisconnect(con)
