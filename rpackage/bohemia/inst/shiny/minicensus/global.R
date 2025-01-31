
# get data from server
library(bohemia)
library(yaml)
library(reshape2)
library(tidyverse)
library(readxl)

if('data.RData' %in% dir()){
  load('data.RData')
} else {
  
  # Read in xls form
  xf <- read_excel('minicensus.xlsx') %>% dplyr::select(name,
                                                        en = `label::English`,
                                                        sw = `label::Swahili`)
  
  # # Read in list of ids to be evaluated
  # imani <- read_excel('Final_list_Test_2.xlsx')
  # keep_uuids <- imani$instanceid
  
  # read in creds
  creds <- read_yaml('credentials.yaml')
  
  # Read in answer key
  data <- odk_get_data(
    url = creds$tza_odk_server,
    id = 'minicensus',
    id2 = NULL,
    unknown_id2 = FALSE,
    # uuids = NULL,
    exclude_uuids = NULL,
    # uuids = c("uuid:06e9b414-4c82-4097-bd4a-e31065f1265e",
    #           "uuid:641eaadd-1e47-4e48-99c2-02c13de93b0a",
    #           "uuid:bab37889-d8a8-4b6c-984c-a99e2b9926ce"),
    user = creds$tza_odk_user,
    password = creds$tza_odk_pass
  )
  # Keep only those from the date period specified by Imani
  answer_uuid <- answer_uuids <- "uuid:0e05fd1e-4c0d-46d6-a02e-6786a57e0102"
  keep_uuids <- sort(unique(data$non_repeats$instanceID))
  # keep_uuids <- c(keep_uuids, 
  #                 data$non_repeats$instanceID[data$non_repeats$instanceID == answer_uuid])
  # keep_uuids <- data$non_repeats$instanceID[data$non_repeats$todays_date >= '2020-09-05' |
  #                                            data$non_repeats$instanceID == answer_uuid]
  data$non_repeats <- data$non_repeats[data$non_repeats$instanceID %in% keep_uuids,]
  
  # # Manually make some changes per Imani
  # data$non_repeats$wid[data$non_repeats$instanceID == 'uuid:f3249504-e3f3-41d9-86ad-a40d7e58f78a'] <- 76
  # drop_ids <- 'uuid:5f6083e2-b507-48dc-96e9-b601e6a008dd'
  # data$non_repeats <- data$non_repeats[!data$non_repeats$instanceID %in% drop_ids,]
  drop_ids <- c()
  
  # Divide answers vs non-answers
  repeat_names <- names(data$repeats)
  non_answer_repeats <- answer_repeats <- list()
  for(i in 1:length(repeat_names)){
    this_repeat_name <- repeat_names[i]
    this_repeat_data <- data$repeats[[this_repeat_name]]
    this_answer_repeats <- this_repeat_data %>% filter(instanceID %in% answer_uuids & !instanceID %in% drop_ids)
    this_non_answer_repeats <- this_repeat_data %>% filter(!instanceID %in% answer_uuids & !instanceID %in% drop_ids)
    answer_repeats[[i]] <- this_answer_repeats
    # Remove from the non-answer repeats those which are not in the correct dates
    this_non_answer_repeats <- this_non_answer_repeats %>% filter(instanceID %in% keep_uuids)
    non_answer_repeats[[i]] <- this_non_answer_repeats
  }
  fw_repeats <- non_answer_repeats
  names(answer_repeats) <-  names(fw_repeats) <- repeat_names
  
  # Compare non-answers with answers
  fw_non_repeats <- data$non_repeats %>% filter(!instanceID %in% answer_uuids)
  answer_non_repeats <- data$non_repeats %>% filter(instanceID %in% answer_uuids)
  
  # # We now have 4 objects
  # fw_repeats
  # fw_non_repeats
  # answer_repeats
  # answer_non_repeats
  
  # Define those vars we never want to grade
  dont_evaluate <- c('instanceID',
                     'device_id',
                     'end_time',
                     'hh_photograph',
                     'instanceName',
                     'start_time',
                     'todays_date',
                     'wid',
                     'wid_manual',
                     'repeat_name',
                     'repeated_id')
  # Grade
  out_list <- list()
  wrong_hamlet_list <- c()
  for(i in 1:nrow(fw_non_repeats)){
    message('i is ', i)
    no_answer <- FALSE
    # Get the non repeats
    this_row <- fw_non_repeats[i,]
    wid <- this_row$wid
    the_uuid <- this_row$instanceID
    this_hamlet_id <- this_row$hh_hamlet_code
    this_answer <- answer_non_repeats# %>% filter(hh_hamlet_code == this_hamlet_id)
    if(nrow(this_answer) < 1){
      wrong_hamlet_list <- c(wrong_hamlet_list, wid)
    } else {
      answer_uuid <- this_answer$instanceID
      answer_nas <- as.logical(is.na(this_answer)) | as.logical(this_answer == '()')
      this_answer <- this_answer[,!answer_nas]
      this_row <- this_row[,!answer_nas]
      this_answer <- this_answer[,!names(this_answer) %in% dont_evaluate]
      this_row <- this_row[,!names(this_row) %in% dont_evaluate]
      response_vector <- as.logical(this_row == this_answer)
      label_df <- tibble(name = names(this_row)) %>% left_join(xf)
      out <- tibble(`Worker ID` = wid,
                    `Variable` = names(this_row),
                    `Label (en)` = label_df$en,
                    `Label (sw)` = label_df$sw,
                    `Supplied answer` = as.character(this_row),
                    `Correct answer` = as.character(this_answer),
                    `Status` = ifelse(response_vector, 'Correct', 'Incorrect'))
      
      # Go through each repeat and get those correct answers too
      rep_list <- list()
      counter <- 0
      for(r in 1:length(repeat_names)){
        print(r)
        this_repeat_name <- repeat_names[r]
        this_repeat_data <- fw_repeats[[this_repeat_name]]
        this_row <- this_repeat_data %>% filter(instanceID == the_uuid)
        this_answer <- answer_repeats[[this_repeat_name]] %>% filter(instanceID == answer_uuid)
        # answer_nas <- as.logical(is.na(this_answer)) | as.logical(this_answer == '()')
        # this_answer <- this_answer[,!answer_nas]
        # this_row <- this_row[,!answer_nas]
        this_answer <- this_answer[,!names(this_answer) %in% dont_evaluate]
        this_row <- this_row[,!names(this_row) %in% dont_evaluate]
        for(x in 1:nrow(this_answer)){
          x_answer <- this_answer[x,]
          x_row <- this_row[x,]
          response_vector <- as.logical(x_row == x_answer)
          
          label_df <- tibble(name = names(x_answer)) %>% left_join(xf)
          outx <- tibble(`Worker ID` = wid,
                         `Variable` = names(x_answer),
                         `Label (en)` = label_df$en,
                         `Label (sw)` = label_df$sw,
                         `Supplied answer` = as.character(this_row),
                         `Correct answer` = as.character(this_answer),
                         `Status` = ifelse(response_vector, 'Correct', 'Incorrect'))
          counter <- counter + 1
          rep_list[[counter]] <- outx
        }
      }
      reps <- bind_rows(rep_list)
      final <- bind_rows(
        out,
        reps
      )
      out_list[[i]] <- final
    }
  }
  
  done <- bind_rows(out_list)
  # wrong_hamlet_list
  
  # Clean up a bit
  done <- done %>% filter(!grepl('note_', Variable)) %>%
    filter(!grepl('_count', Variable)) %>%
    filter(!is.na(`Correct answer`)) %>%
    filter(`Correct answer` != 'NA')
  
  # Get aggregated
  agg <- done %>%
    group_by(`Worker ID`) %>%
    summarise(Correct = length(which(Status == 'Correct')),
              Incorrect = length(which(Status == 'Incorrect')),
              Percent = round(Correct / (Correct+Incorrect) * 100, digits = 2)) %>%
    arrange(desc(Percent))
  save(agg, done, wrong_hamlet_list, xf, final,
       fw_repeats,
       fw_non_repeats,
       answer_repeats,
       answer_non_repeats,
       repeat_names,
       dont_evaluate,
       file = 'data.RData')
}

# # Write csvs for Elena
# dir.create('~/Desktop/elena')
# write_csv(answer_non_repeats, '~/Desktop/elena/scenarios_non_repats.csv')
# write_csv(answer_repeats$repeat_hh_sub, '~/Desktop/elena/scenarios_repats_hh_sub.csv')
# write_csv(answer_repeats$repeat_household_members_enumeration, '~/Desktop/elena/scenarios_repeats_household_members_enumeration.csv')
# write_csv(answer_repeats$repeat_mosquito_net, '~/Desktop/elena/scenarios_repeat_mosquito_net.csv')
# write_csv(answer_repeats$repeat_water, '~/Desktop/elena/scenarios_repeat_water.csv')
