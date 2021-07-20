library(dplyr)
dat <- read.csv('contact_info.csv')
dat$X <- NULL
usethis::use_data(dat, overwrite = TRUE)

# new icd 10 data
icd_por <- read.csv('icd10_portuguese.csv', stringsAsFactors = FALSE)
icd_por$X <- NULL

# remove ' ' and -
icd_por <- icd_por %>% filter(name != '')
icd_por <- icd_por %>% filter(code != '')

icd_por <- icd_por %>% filter(!grepl('-', code, fixed = T))
icd_por$name <- unlist(lapply(strsplit(icd_por$name, '.', fixed = T), function(x) x[length(x)]))
icd_por$name <- trimws(icd_por$name, which = 'both')

icd_codes_por <- icd_por$code
icd_names_por <- icd_por$name

# new icd 10 data
icd_eng <- read.csv('icd10_english.csv', stringsAsFactors = FALSE)
icd_eng$X <- NULL

# remove ' ' and -
icd_eng <- icd_eng %>% filter(name != '')
icd_eng <- icd_eng %>% filter(code != '')

icd_eng <- icd_eng %>% filter(!grepl('-', code, fixed = T))
icd_eng$name <- unlist(lapply(strsplit(icd_eng$name, '.', fixed = T), function(x) x[length(x)]))
icd_eng$name <- trimws(icd_eng$name, which = 'both')

icd_codes_eng <- icd_eng$code
icd_names_eng <- icd_eng$name

# # make icd10 data
# icd_codes <- c('A00', 'A01', 'A01', 'A06', 'A09', 'A15', 'A16', 'A20', 'A33', 'A41', 'A75', 'B05', 'B24', 'B45', 'B53', 'B54', 'C80', 'G03', 'G04', 'G83', 'I50', 'J06', 'J18', 'J45', 'J81', 'J98', 'K75', 'L08', 'R09', 'R50', 'S09', 'S36', 'T14', 'T14.9', 'T30', 'C22', 'C46', 'C50', 'C55', 'C61', 'C80', 'D48', 'E14', 'I10', 'I42', 'I64', 'A80', 'K25', 'K29', 'K37', 'K46', 'K56', 'K65', 'K74', 'K75', 'K92', 'M86', 'M89', 'N04', 'N05', 'N15', 'N39', 'N94', 'B24', 'P05', 'P15', 'P21', 'P22', 'P23', 'P36', 'P37', 'P54', 'P74', 'P78', 'P95', 'P95', 'P95', 'Q05', 'Q24', 'Q89', 'R95', 'X49', 'Y09', 'O06', 'O16/O15', 'O46', 'O66', 'O71', 'O72', 'O75', 'O75', 'O85', 'O98', 'O99', 'O99', 'O99', 'O99', 'O99.4', 'Z21', 'T29', 'T30', 'T31', 'T32', 'T50', 'T51', 'T54', 'T56', 'T58', 'T59', 'T60', 'T65', 'T67', 'T70', 'T71', 'T80', 'T81', 'T83', 'Y08', 'Y09', 'V98', 'V99', 'V99')
# icd_names <- c("Cholera", "Typhoid fever (salmonellosis)", "Typhoid", "Dysentery Acute/Chronic", "Diarrhoea", "TB Confirmed", "TB Not confirmed", "Plague", "Tetanus, Neonatal", "Septicaemia", "Relapsing Fever (Louse borne Typhus)", "Measles", "HIV and AIDS", "Meningitis Cryptococal", "Malaria confirmed", "Malaria presumptive", "Neoplasm", "Meningitis", "Encephalitis", "Acute Flaccid Paralysis", "Heart failure", "Respiratory Infection Acute (ARI)", "Pneumonia", "Asthma", "Pulmonary oedema", "Pneumopathies", "Hepatitis", "Skin infections", "Pleurisy (non-Tuberculosis)", "Fever Chronic (> 1 month)", "Head injury", "Ruptured spleen", "Fractures", "Trauma Other", "Burns", "Cancer Liver", "Kaposi's sarcoma", "Cancer Breast", "Cancer Uterine", "Cancer Prostate", "Tumours Other malignant", "Tumours Other non-malignant", "Diabetes", "Hypertension", "Cardiomyopathy", "Cerebrovascular accident", "Acute Flacid Paralysis (polio)", "Ulcer, gastro-duodenal", "Gastritis", "Appendicitis", "Hernia", "Intestinal occlusion", "Peritonitis (non-Tuberculosis)", "Cirrhosis of the liver", "Hepatitis", "Digestive tract Haemorrhages", "Bone infections (including osteomyelitis)", "Bone and joint disease other", "Nephrotic syndrome", "Glomerulonephritis", "Kidney infections", "Urinary tract infections", "Gynecological problems", "Paediatric AIDS", "Low birth weight or Prematurity Complication", "Birth trauma", "Neonatal Asphyxia", "Respiratory distress", "Pneumonia", "Neonatal Septicaemia", "Malaria", "Haemorrhage", "Dehydration", "Diarrhoea", "Stillbirth (fresh)", "Stillbirth (macerated)", "Stillbirth", "Congenital hydrocephalus and spinal bifida", "Congenital malformation of the heart", "Other congenital malformation", "Sudden infant death syndrome", "Accidental poisoning by and exposure to noxious substances", "Assault", "Abortion", "Severe Hypertension in pregnancy/ eclampsia", "Antepartum Haemorrhage", "Obstructed Labour", "Rupture uterus", "Post-partum haemorrhage", "Unknown fever", "Local herbs", "Puerperal Sepsis /Septicaemia", "Malaria in pregnancy", "Pneumonia", "Anaemia in Pregnancy", "Pulmonary oedema", "Meningitis", "Cardiomyopathy", "Asymptomatic HIV", "Burns and corrosions of multiple body regions", "Burn and corrosion, body region unspecified", "Burns classified according to extent of body surface involved", "Corrosions classified according to extent of body surface involved", "Poisoning by diuretics and other unspecified drugs, medicaments and biological substances", "Toxic effect of alcohol", "Toxic effect of corrosive substances", "Toxic effect of metals", "Toxic effect of carbon monoxide", "Toxic effect of other gases, fumes and vapours", "Toxic effect of pesticides", "Toxic effect of other and unspecified substances", "Effects of heat and light", "Effects of air pressure and water pressure", "Asphyxiation", "Complications following infusion, transfusion and therapeutic injection", "Complications of procedures, not elsewhere classified", "Complications of genitourinary devices, implants and grafts", "Assault by other specified means", "Assault by other unspecified means", "Other Specified transport accidents", "Unspecified transport accidents", "Test ICD 10 Value")

icd_data_moz <- tibble(icd_codes = icd_codes_por, icd_names = icd_names_por)
icd_data_tza <- tibble(icd_codes = icd_codes_eng, icd_names = icd_names_eng)

usethis::use_data(icd_data_moz, overwrite = T)
usethis::use_data(icd_data_tza, overwrite = T)


# read in va survey and choices 
va_survey <- readr::read_csv('va_survey.csv')
va_choices <- readr::read_csv('va_choices.csv')
va_choices_ext <- readr::read_csv('va_choices_external.csv')
va_choices_ext <- va_choices_ext %>% select(list_name, name, `label::English`, `label::Portuguese`, `label::Swahili`, country)
va_choices <- rbind(va_choices, va_choices_ext)

# get names of select_one and select_multiples
select_names <- va_survey$name[grepl('select', va_survey$type)]
select_names <- select_names[!is.na(select_names)]

usethis::use_data(va_survey, overwrite = T)
usethis::use_data(va_choices, overwrite = T)
usethis::use_data(select_names, overwrite = T)

# get readable names from iD codes
# get va form (For readable variable names)
va_form <- read.csv('va_survey.csv', na.strings = c('NA'=''), stringsAsFactors = F)
va_form <- va_form %>% select(name, label_english = label..English, label_portuguese = label..Portuguese, label_swahili = label..Swahili)
# if swahili missing,fill with english
va_form$label_swahili <- ifelse(is.na(va_form$label_swahili), va_form$label_english, va_form$label_swahili)
va_form$label_portuguese <- ifelse(is.na(va_form$label_portuguese),
                                   va_form$label_english, va_form$label_portuguese)
va_names <- va_form[complete.cases(va_form),]
usethis::use_data(va_names, overwrite = TRUE)

