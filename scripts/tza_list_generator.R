library(bohemia)
library(dplyr)
library(rmarkdown)

# Define parameters for connecting to ODK server
# creds <- yaml::yaml.load_file('../credentials/credentials.yaml')
# creds$user <- creds$databrew_odk_user
# creds$password <- creds$databrew_odk_pass
# creds$url <- creds$databrew_odk_server
# creds$user <- creds$tza_odk_user
# creds$password <- creds$tza_odk_pass
# creds$url <- creds$tza_odk_server
creds <- list(
  user = '<USERNAME GOES HERE>',
  password = '<PASSWORD GOES HERE>',
  url = '<ODK AGGREGATE SERVER URL GOES HERE>'
)

# Define other parameters
refresh <- TRUE # do you want to fetch new data (TRUE) or just use previously fetched data (FALSE)
lc <- 'KAI' # 3 letter location code
n_teams <- 3 # Number of enumeration teams
enum <- TRUE # whether to generate a list for enumerators (true) or not (false). For TZA, always TRUE
use_previous <- FALSE # whether to use previous data on households collected through enumeration (true) or guess households based on recon (false). For tza, always false
date_range <- as.Date(c('2020-01-01',
                        '2021-12-31')) # the min and max dates to filter for consent verification list

############# MINICENSUS
# Read in the "minicensus" data
id <- 'smallcensusa'
file_name <- paste0('tza_list_data/', id, '.RData')
exists_minicensus <- file.exists(file_name)

if(refresh | !exists_minicensus){
  minicensus <-
    odk_get_data(url = creds$url,
                 id = id,
                 user = creds$user,
                 password = creds$password,
                 unknown_id2 = FALSE,
                 uuids = NULL,
                 exclude_uuids = NULL)
  save(minicensus, file = file_name)
} else {
  load(file_name)
}

############# VISIT CONTROL SHEET
# Get number of households for the place in question (per recon)
gps <- bohemia::gps
hamlet <- gps %>% filter(code == lc)
n_hh <- hamlet$n_households

# Get other details
data <- data.frame(n_hh,
                   n_teams,
                   id_limit_lwr = 1,
                   id_limit_upr = n_hh)
out_file <- paste0(getwd(), '/pdfs/visit_control_sheet.pdf')
rmarkdown::render(input = #paste0('../rpackage/bohemia/inst/rmd/visit_control_sheet.Rmd'),
                  paste0(system.file('rmd', package = 'bohemia'), '/visit_control_sheet.Rmd'),
                  output_file = out_file,
                  params = list(xdata = data,
                                loc_id = lc,
                                enumeration = enum,
                                use_previous = use_previous,
                                enumerations_data = NULL, 
                                include_name = TRUE,
                                li = TRUE))
message('PDF produced at ', out_file)

######## CONSENT VERIFICATION LIST
# Get the data
# Get the odk data
pd <- minicensus$non_repeats %>%
  filter(todays_date >= date_range[1],
         todays_date <= date_range[2])
people <- minicensus$repeats$repeat_household_members_enumeration
# Get the country
co <- 'Tanzania'
pd <- pd %>% dplyr::filter(hh_country == co)
people <- people %>% filter(instanceID %in% pd$instanceID)

# Get hh head
out <- pd %>%
  dplyr::select(num = hh_head_id,
                instanceID,
                todays_date,
                hh_head_dob,
                wid,
                hh_hamlet_code) %>%
  mutate(num = as.character(num)) %>%
  left_join(people %>% mutate(num = as.character(hh_member_number)),
            by = c('instanceID', 'num'))
pd <- out %>%
  mutate(name = paste0(first_name, ' ', last_name),
         age = floor(as.numeric(as.Date(todays_date) - as.Date(hh_head_dob))/ 365.25)) %>%
  mutate(consent = 'HoH (minicensus)') %>%
  mutate(x = ' ',y = ' ', z = ' ') %>%
  mutate(hh_id = substr(permid, 1, 7)) %>%
  mutate(firma = '  ') %>%
  dplyr::select(wid,
                hh_hamlet_code,
                hh_head_permid = permid,
                hh_id,
                name,
                firma,
                age,
                todays_date,
                consent,
                x,y,z) %>%
  mutate(todays_date = as.Date(todays_date)) %>%
  arrange(wid, todays_date)

# get date closest to today
qc <- pd
qc <- qc %>%  
  dplyr::select(`Hamlet code` = hh_hamlet_code,
                `Worker code` = wid,
                `Household ID` = hh_id, 
                `HH Head ID` = hh_head_permid,
                Age = age,
                Date = todays_date)
# get inputs for slider to control sample size
min_value <- 1
max_value <- nrow(qc)
selected_value <- sample(min_value:max_value, 1)
if(co == 'Mozambique'){
  names(pd) <- c('Código TC',
                 'Código Bairro',
                 'ExtID (número de identificão do participante)',
                 'ID Agregado',
                 'Pessoa que assino o consentimiento',
                 'Idade do membro do agregado',
                 'Data de recrutamento',
                 'Consentimento/ Assentimento informado (marque se estiver correto e completo)',
                 'Se o documento não estiver preenchido correitamente, indicar o error',
                 'O error foi resolvido (sim/não)',
                 'Verificado por (iniciais do arquivista) e data')
  
} else {
  names(pd) <- c('FW code',
                 'Hamlet code',
                 'ExtID HH member',
                 'Household ID',
                 'Name',
                 "Person who signed consent",
                 'Age of household member',
                 'Recruitment date',
                 'Informed consent/assent type (check off if correct and complete)',
                 'If not correct, please enter type of error',
                 'Was the error resolved (Yes/No)?',
                 'Verified by (archivist initials) and date')
}

eldo_format <- FALSE
if(!is.null(eldo_format)){
  if(eldo_format){
    pd <- pd %>%
      left_join(fids %>%
                  dplyr::mutate(fid_name = paste0(first_name, ' ', last_name)) %>%
                  dplyr::select(`Código TC` = bohemia_id,
                                `Nome do inquiridor` = fid_name))
    pd <- pd %>%
      left_join(locations %>%
                  dplyr::select(`Código Bairro` = code,
                                Hamlet))
    pd <- pd %>%
      mutate(xxx = ' ', yyy = ' ', zzz = ' ')
    
    pd <- pd %>% dplyr::select(
      `Data`= `Data de recrutamento`,
      `ID`= `Código TC`,
      `Nome do inquiridor`,
      `Bairro` = Hamlet,
      `ID Bairro` = `Código Bairro`,
      `ID Agregado`,
      `Idade do chefe(a)` = `Idade do membro do agregado`,
      `O consentimento existe?` = xxx,
      `O ICF esta preenchido correctamente?` = yyy,
      `Se o documento não estiver preenchido correitamente, indicar o error`,
      `O error foi resolvido (sim/não)`,
      `Verificado por (iniciais do arquivista) e data`
    )
  }  
}

qc
pdx <- pd
if(all(is.na(pdx$`FW code`))){
  pdx$`FW code` <- 0
}
out_file <- paste0(getwd(), '/pdfs/consent_verification_list.pdf')
rmarkdown::render(input = paste0(system.file('rmd', package = 'bohemia'), '/consent_verification_list.Rmd'),
                  output_file = out_file,
                  params = list(data = pdx))
message('PDF produced at ', out_file)

