
##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2
#' @import gsheet
#' @import DT
#' @import shinyMobile
#' @import dplyr
#' @import shinyjs

suppressPackageStartupMessages(
  library(shinyjs)
)

library(dplyr)

temp_dir <- tempdir()# getwd()
if(!dir.exists(temp_dir)){
  dir.create(temp_dir)
}
load_base_data <- function(){
  if('ifc.RData' %in% dir(temp_dir)){
    load(paste0(temp_dir, '/ifc.RData'))
  } else {
    # Get fieldworker data
    # Define a default fieldworkers data
    # if(!'fids.csv' %in% dir(temp_dir)){
      fids_url <- 'https://docs.google.com/spreadsheets/d/1o1DGtCUrlBZcu-iLW-reWuB3PC8poEFGYxHfIZXNk1Q/edit#gid=0'
      fids1 <- gsheet::gsheet2tbl(fids_url) %>% dplyr::select(bohemia_id, first_name, last_name, supervisor, Role = details) %>% dplyr::mutate(country = 'Tanzania')
      fids_url <- 'https://docs.google.com/spreadsheets/d/1o1DGtCUrlBZcu-iLW-reWuB3PC8poEFGYxHfIZXNk1Q/edit#gid=409816186'
      fids2 <- gsheet::gsheet2tbl(fids_url) %>% dplyr::select(bohemia_id, first_name, last_name, supervisor, Role = details) %>% dplyr::mutate(country = 'Mozambique')
      fids_url <- 'https://docs.google.com/spreadsheets/d/1o1DGtCUrlBZcu-iLW-reWuB3PC8poEFGYxHfIZXNk1Q/edit#gid=179257508'
      fids3 <- gsheet::gsheet2tbl(fids_url) %>% dplyr::select(bohemia_id, first_name, last_name, supervisor, Role = details) %>% dplyr::mutate(country = 'Catalonia')
      fids <- bind_rows(fids1, fids2, fids3)
      print('WORKING DIRECTORY IS')
      print(getwd())
      # readr::write_csv(fids, paste0(temp_dir, '/fids.csv'))
    # } else {
      # fids <- readr::read_csv(paste0(temp_dir, '/fids.csv'))
    # }
    
    # Read in locations
    locations <- bohemia::locations
    
    
    
    # Prepare some dummy data
    date_choices <- as.Date((Sys.Date()-30):Sys.Date(), origin = '1970-01-01')
    dates <- sample(date_choices, 1500, replace = TRUE)
    hh_ids <- paste0(sample(locations$code, 1500, replace = T), '-',
                     seq(100, 599, by = 1))
    hh <- 
      dplyr::tibble(date = dates,
                    hh_id = hh_ids)
    out_list <- list()
    for(i in 1:nrow(hh)){
      this_hh <- hh$hh_id[i]
      nn <- sample(1:15, 1)
      nums <- bohemia::add_zero(1:nn, 3)
      ids <- paste0(this_hh, '-', nums)
      out <- dplyr::tibble(hh_id = this_hh,
                           date = hh$date[i],
                           id = ids)
      out_list[[i]] <- out
    }
    df <- bind_rows(out_list)
    df$fid <- sample(fids$bohemia_id[!is.na(fids$first_name)], nrow(df), replace = TRUE)
    df <- left_join(df, fids %>% mutate(fw_name = paste0(first_name, ' ', last_name)) %>%
                      dplyr::select(bohemia_id, fw_name), by = c('fid' = 'bohemia_id'))
    df$hamlet_code <- substr(df$id, 1, 3)
    df <- left_join(df, locations %>% dplyr::select(code, hamlet = Hamlet), by = c('hamlet_code' = 'code'))
    df$name <- paste0(sample(babynames::babynames$name, nrow(df), replace = TRUE))
    df$age <- round(rnorm(n = nrow(df), mean = 50, sd = 20)); df$age <- ifelse(df$age <0, df$age *-1, df$age)
    df$type_icf <- sample(c('HoH',
                            'Adult',
                            'Child 12-18',
                            'Parent/guardian'),
                          nrow(df),
                          replace = TRUE)
    df$pid <- df$id
    df$attempts <- 0
    save(df, file = paste0(temp_dir, '/ifc.RData'))
  }
  return(df)
}

# Function for storing / loading ALL data
load_all_data <- function(){
  if('all_data.RData' %in% dir()){
    message('Loading previously saved "all_data.RData"')
    load('all_data.RData')
  } else {
    message('Creating an "all_data.RData" from scratch')
    main <- load_base_data()
    dl <- list(main = main,
               list1 = data.frame(),
               list2 = data.frame(),
               list3 = data.frame())
    save(dl, file = 'all_data.RData')
  }
  return(dl)
}


# Define functions for migrating rows between lists
remove_from <- function(list_number = 'main', this_id, data_list){
  # Get full original list
  if(list_number == 'main'){
    original_df <- data_list$main
  } else {
    dl <- reactiveValuesToList(data_list)
    original_df <- dl[[paste0('list', list_number)]]
  }
  # Remove the ID
  new_df <- original_df %>% filter(!id %in% this_id)
  # Communicate the result
  message('Just reduced the ', list_number, ' table from ',
          nrow(original_df), 
          ' to ',
          nrow(new_df))
  # Retun the new table
  return(new_df)
}
add_to_list <- function(list_number, new_row, data_list){
  # Add the archivist data to the new row
  new_row$archivist <- 'ARCHIVIST NAME'
  new_row$verification_date <- Sys.Date()
  # Get the old one
  dl <- reactiveValuesToList(data_list)

  old_list <- dl[[paste0('list', list_number)]]
  # Add the new row
  print('old list is:')
  print(old_list)
  ok <- FALSE
  if(!is.null(old_list)){
    if(nrow(old_list) > 0){
      ok <- TRUE
    }
  }
  if(ok){
    new_list <- bind_rows(old_list, new_row)
  } else {
    new_list <- new_row
  }
  # Communicate the result
  message('Just increased list ', list_number, ' from ',
          nrow(old_list), 
          ' to ',
          nrow(new_list))
  # Return the new list
  return(new_list)
}


error_list <- c('Participant did not sign',
                'Participant did not date',
                'Witness did not sign',
                'Witness did not date',
                'Fieldworker did not sign',
                'Fieldworker did not date',
                'Dates do not match',
                'Fieldworker wrote participant name',
                'Fieldworker wrote participant date',
                'Fieldworker wrote witness name',
                'Fieldworker wrote witness date',
                'Unclear thumbprint',
                'Questions and/or answers were not written in correct place',
                'Questions were not recorded/not documented that there were no questions',
                'Answers not recorded',
                'Answers incorrect/misleading',
                'Participant ID not on each page',
                'Participant ID differs across pages',
                'Participant name is different to participant signature',
                'Witness name is different to witness signature',
                'Handwriting ilegibile',
                'Incorrect participant ID',
                'Overwriting and/or obliteration')
error_list <- paste0(1:length(error_list), '. ', error_list)

app_ui <- function(request) {
  options(scipen = '999')
  
  tagList(
    mobile_golem_add_external_resources(),
    
    dashboardPage(
      dashboardHeader (title = "ICF tool"),
      dashboardSidebar(
        sidebarMenu(
          menuItem(
            text="ICF First Verification",
            tabName="icf1"),
          menuItem(
            text="ICF Return Verification",
            tabName="icf2"),
          menuItem(
            text="Lists",
            tabName='lists'
          ),
          menuItem(
            text = 'About',
            tabName = 'about')
        )),
      dashboardBody(
        # tags$head(
        #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        # ),
        tabItems(
          tabItem(
            tabName="icf1",
            fluidPage(
              shinyjs::useShinyjs(),
              fluidRow(
                uiOutput('top_button')
              ),
              
              fluidRow(
                column(3,
                       uiOutput('ui_date_input')),
                column(3,
                       uiOutput('ui_hamlet_filter')),
                column(3, 
                       uiOutput('ui_hh_id')),
                column(3,
                       textOutput('person_id_text1'))),
              fluidRow(column(12,
                              DT::dataTableOutput('person_table'))),
              fluidRow(
                uiOutput('ui_icf_present'),
                uiOutput('ui_present'),
                uiOutput('ui_big')    
              )
            )
          ),
          tabItem(
            tabName = 'icf2',
            fluidPage(
              fluidRow(
                column(4,
                       uiOutput('ui_date_input2')),
                column(4, 
                       uiOutput('ui_hh_id2')),
                column(4,
                       textOutput('person_id_text2'))),
              fluidRow(column(12,
                              helpText('The below shows the previous errors associated with this ICF.'))),
              fluidRow(column(12,
                              DT::dataTableOutput('person_table2'))), 
              fluidRow(
                uiOutput('ui_icf_present2'), 
                uiOutput('ui_present2'),
                uiOutput('ui_big2')    
              )
            )),
          tabItem(
            tabName = 'lists',
            fluidPage(
              fluidRow(h1('List 1 (missing ICFs)')),
              uiOutput('ui_list_1'),
              uiOutput('ui_list_1b'),
              
              fluidRow(h1('List 2 (correct ICFs)')),
              uiOutput('ui_list_2'),
              
              fluidRow(h1('List 3 (ICFs to be corrected)')),
              uiOutput('ui_list_3')
            )
          ),
          tabItem(
            tabName = 'about',
            fluidPage(
              fluidRow(
                div(img(src='https://www.databrew.cc/images/logosmall.png', align = "center"), style="text-align: center;"),
                h4('Built in partnership with ',
                   a(href = 'https://databrew.cc',
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
          )
        )
      )
    )
  )
}



##################################################
# SERVER
##################################################
#' @import shiny
#' @import leaflet
app_server <- function(input, output, session) {
  
  
  observeEvent(input$show, {
    lit <- log_in_text()
    showModal(modalDialog(
      title = "Log in",
      fluidPage(
        fluidRow(
          column(6,
                 textInput('user', 'Bohemia Fieldworker ID number')),
          column(6,
                 passwordInput('password', 'Password'))
        ),
        fluidRow(
          column(12, align = 'center',
                 actionButton('log_in', 'Log in'))
        ),
        fluidRow(
          p(lit)
        )
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  log_in_text <- reactiveVal(value = '')
  logged_in <- reactiveVal(value = FALSE)
  observeEvent(input$log_in,{
    u <- input$user
    u <- as.numeric(u)
    p <- input$password
    ok <- FALSE
    if(u %in% c(313, 344, 377, 534, 403, 601)){
      if(p == 'admin'){
        logged_in(TRUE)
        removeModal()
        ok <- TRUE
        log_in_text('')
      }
    }
    if(ok){
      log_in_text('Incorrect username and password')
    }
  })
  
  output$top_button <- renderUI({
    li <- logged_in()
    if(li){
      # actionButton("log_out", "Log out")
      fluidPage()
    } else {
      h1(actionButton("show", "Log in"))
    }
  })
  
  # Create reactive objects for storing person ID
  person_ids <- reactiveValues(id1 = NA, id2 = NA)
  

  observeEvent(input$log_out, {
    logged_in(FALSE)
  })
  
  # create a reactive dataframe to store data
  dl <- load_all_data()
  data_list <- reactiveValues(main = dl$main,
                              list1 = dl$list1,
                              list2 = dl$list2,
                              list3 = dl$list3)
  
  # Date input ui
  output$ui_date_input <- renderUI({
    li <- logged_in()
    if(li){
      df <- data_list$main
      dateInput('date_of_visit', 'Date of visit',min = min(df$date),
                max = max(df$date))
    }
    
  })
  output$ui_date_input2 <- renderUI({
    li <- logged_in()
    if(li){
      df <- data_list$list3
      ok <- FALSE
      if(!is.null(df)){
        if(nrow(df) > 0){
          ok <- TRUE
        }
      }
      if(ok){
        dateInput('date_of_visit2', 'Date of visit',min = min(df$date),
                  max = max(df$date)) 
      } else {
        NULL
      }
    }
  })
  
  output$ui_hamlet_filter <- renderUI({
    li <- logged_in()
    if(li){
      date_of_visit <- input$date_of_visit
      ok <- FALSE
      if(!is.null(date_of_visit)){
        if(length(date_of_visit) > 0){
          ok <- TRUE
        }
      }
      if(ok){
        message('---Setting up the hh_id input')
        df <- data_list$main
        choices <- df %>% filter(date == date_of_visit) 
        the_choices <- sort(unique(choices$hh_id))
        the_choices <- sort(unique(substr(the_choices, 1, 3)))
        selectizeInput('hamlet_id', 'Filter by hamlet ID', choices = the_choices,
                       multiple = TRUE,
                       selected = the_choices)
      } else {
        NULL
      }
    }
  })
  
  output$ui_hh_id <- renderUI({
    li <- logged_in()
    if(li){
      date_of_visit <- input$date_of_visit
      ok <- FALSE
      if(!is.null(date_of_visit)){
        if(length(date_of_visit) > 0){
          ok <- TRUE
        }
      }
      if(ok){
        message('---Setting up the hh_id input')
        df <- data_list$main
        choices <- df %>% filter(date == date_of_visit) 
        hid <- input$hamlet_id
        if(!is.null(hid)){
          choices <- choices %>%
            mutate(hamlet_id = substr(hh_id, 1, 3)) %>%
            filter(hamlet_id %in% hid)
        }
        the_choices <- sort(unique(choices$hh_id))
        selectInput('hh_id', 'Household ID', choices = the_choices)
      } else {
        NULL
      }
    }
  })
  output$ui_hh_id2 <- renderUI({
    li <- logged_in()
    if(li){
      date_of_visit <- input$date_of_visit2
      
      ok <- FALSE
      if(!is.null(date_of_visit)){
        if(length(date_of_visit) > 0){
          ok <- TRUE
        }
      }
      if(ok){
        message('---Setting up the hh_id input')
        df <- data_list$list3
        choices <- df %>% filter(date == date_of_visit) 
        the_choices <- sort(unique(choices$hh_id))
        selectInput('hh_id2', 'Household ID', choices = the_choices)
      } else {
        NULL
      }
    }
  })
  
  # output$ui_person_id <- renderUI({
  #   out <- data_list$main
  #   out <- data.frame(out)
  #   date_of_visit <- input$date_of_visit
  #   xhid <- input$hh_id
  #   done <- NULL
  #   if(!is.null(date_of_visit)){
  #     if(length(date_of_visit) > 0){
  #       if(!is.null(xhid)){
  #         out <- out %>% 
  #           filter(date == date_of_visit) %>%
  #           filter(hh_id == xhid)
  #         id_choices <- out$pid
  #         message('---Setting up the person_id input')
  #         done <- selectInput('person_id', 'Person ID', choices = id_choices)
  #       }
  #     }
  #   }
  #   return(done)
  # })
  
  # output$ui_person_id2 <- renderUI({
  #   out <- data_list$list3
  #   out <- data.frame(out)
  #   date_of_visit <- input$date_of_visit2
  #   xhid <- input$hh_id2
  #   done <- NULL
  #   if(!is.null(date_of_visit)){
  #     if(length(date_of_visit) > 0){
  #       if(!is.null(xhid)){
  #         out <- out %>% 
  #           filter(date == date_of_visit) %>%
  #           filter(hh_id == xhid)
  #         id_choices <- out$pid
  #         message('---Setting up the person_id input')
  #         done <- selectInput('person_id2', 'Person ID', choices = id_choices)
  #       }
  #     }
  #   }
  #   return(done)
  # })
  

  dfr <- reactive({
    message('---Setting up the reactive dfr() object')
    
    out <- data_list$main
    date_of_visit <- input$date_of_visit
    xhid <- input$hh_id
    # person_id <- person_ids$id1
    
    # save(out, date_of_visit, xhid, person_id, file = '/tmp/jj.RData')
    
    if(!is.null(date_of_visit)){
      if(length(date_of_visit) > 0){
        out <- out %>% filter(date == date_of_visit)
      }
    }
    if(!is.null(xhid)){
      out <- out %>% filter(hh_id == xhid)
    }
    # if(!is.null(person_id)){
    #   out <- out %>% filter(pid == person_id)
    # }
    if(#is.null(person_id) |
       is.null(xhid) |
       is.null(date_of_visit)){
      out <- NULL
    } else 
    message('---Done setting up the reactive dfr() object')
    return(out)
  })
  
  dfr2 <- reactive({
    message('---Setting up the reactive dfr2() object')
    
    out <- data_list$list3
    date_of_visit <- input$date_of_visit2
    xhid <- input$hh_id2
    # person_id <- person_ids$id2
    
    
    if(!is.null(date_of_visit)){
      if(length(date_of_visit) > 0){
        out <- out %>% filter(date == date_of_visit)
      }
    }
    if(!is.null(xhid)){
      out <- out %>% filter(hh_id == xhid)
    }
    # if(!is.null(person_id)){
    #   out <- out %>% filter(pid == person_id)
    # }
    if(#is.null(person_id) |
       is.null(xhid) |
       is.null(date_of_visit)){
      out <- NULL
    }
    message('---Done setting up the reactive dfr2() object')
    return(out)
  })
  
  
  output$person_table <- DT::renderDataTable({
    message('---Setting up the person_table (datatable)')
    sub_df <- dfr()
    DT::datatable(sub_df,
                  selection = list(mode = 'single', selected = c(1)),
                  options = list(paging = FALSE,
                                 searching = FALSE))
  })
  output$person_table2 <- DT::renderDataTable({
    message('---Setting up the person_table2 (datatable)')
    sub_df <- dfr2()
    DT::datatable(sub_df,
                  selection = list(mode = 'single', selected = c(1)),
                  options = list(paging = FALSE,
                                 searching = FALSE))
  })
  
  # Get the selected ID text
  output$person_id_text1 <- renderText({
    li <- logged_in()
    if(li){
      paste0('Selected ID: ', person_ids$id1)
    }
  })
  output$person_id_text2 <- renderText({
    li <- logged_in()
    if(li){
      paste0('Selected ID: ', person_ids$id2)
    }
  })
  
  # Observe the clicks on the person table and update the objects accordingly
  observeEvent(input$person_table_rows_selected,{
    rs <- input$person_table_rows_selected
    message('Row selected is ', rs)
    sub_df <- dfr()
    sub_df <- sub_df[rs,]
    person_ids$id1 <- sub_df$pid
  })
  observeEvent(input$person_table2_rows_selected,{
    rs <- input$person_table2_rows_selected
    message('Row selected 2 is ', rs)
    sub_df <- dfr2()
    sub_df <- sub_df[rs,]
    person_ids$id2 <- sub_df$pid
    
  })
  
  output$ui_icf_present <- renderUI({
    li <- logged_in()
    if(li){
      # Observe changes to df to reset
      x <- dfr()
      radioButtons('icf_present', 'Is the ICF present?', choices = c('Yes', 'No'),
                   selected = character(0),
                   inline = TRUE)
    }
  })
  output$ui_icf_present2 <- renderUI({
    li <- logged_in()
    if(li){
      # Observe changes to df to reset
      x <- dfr2()
      # See if there is anything in the correction list anywa
      x <- input$date_of_visit2
      if(!is.null(x)){
        radioButtons('icf_present2', 'Is the ICF present?', choices = c('Yes', 'No'),
                     selected = character(0),
                     inline = TRUE)
      } else {
        fluidPage(
          fluidRow(
            h3('No pending items to be returned.')
          ),
          fluidRow(
            p('This page will populate after some ICFs have been marked as erroneous.')
          )
        )
      }
    }

  })
  
  output$ui_present <- renderUI({
    li <- logged_in()
    if(li){
      icf_present <- input$icf_present
      if(!is.null(icf_present)){
        if(icf_present == 'No'){
          fluidPage(
            h3('Report missing ICF (list 1)'),
            actionButton('submit_list_1', 'Submit data')
          )
        }
      }
    }
  })
  
  output$ui_present2 <- renderUI({
    li <- logged_in()
    if(li){
      icf_present <- input$icf_present2
      if(!is.null(icf_present)){
        if(icf_present == 'No'){
          fluidPage(
            # h3('Report missing ICF (list 1)'),
            # actionButton('submit_list_1', 'Submit data')
            h5('This ICF has already been marked as requiring corrections. Wait for the correction to be submitted.')
          )
        }
      }
    }
  })
  
  output$ui_big <- renderUI({
    li <- logged_in()
    if(li){
      icf_present <- input$icf_present
      message('---Setting up ui_big')
      
      # Observe changes to the main dataframe and reset
      x <- dfr()
      
      if(!is.null(icf_present)){
        if(icf_present == 'Yes'){
          fluidPage(
            fluidRow(
              h3('Transcription of errors detected in the field'),
              p('Check the "Verification list for supervisors", did the ICF present any of the following errors detected by the supervisor in the field?'),
              mod_error_check_ui("error_1", error_list[1]),
              mod_error_check_ui("error_2", error_list[2]),
              mod_error_check_ui("error_3", error_list[3]),
              mod_error_check_ui("error_4", error_list[4]),
              mod_error_check_ui("error_5", error_list[5]),
              mod_error_check_ui("error_6", error_list[6]),
              mod_error_check_ui("error_7", error_list[7]),
              mod_error_check_ui("error_8", error_list[8]),
              mod_error_check_ui("error_9", error_list[9]),
              mod_error_check_ui("error_10", error_list[10]),
              mod_error_check_ui("error_11", error_list[11]),
              mod_error_check_ui("error_12", error_list[12]),
              checkboxInput('no_errors', 'No errors', value = FALSE)),
            fluidRow(
              h3('Errors detected by the archivist'),
              p('Does the ICF present any of the following errors?'),
              mod_error_check_archivist_ui('error_arc_1', error_list[1]),
              mod_error_check_archivist_ui('error_arc_2', error_list[2]),
              mod_error_check_archivist_ui('error_arc_3', error_list[3]),
              mod_error_check_archivist_ui('error_arc_4', error_list[4]),
              mod_error_check_archivist_ui('error_arc_5', error_list[5]),
              mod_error_check_archivist_ui('error_arc_6', error_list[6]),
              mod_error_check_archivist_ui('error_arc_7', error_list[7]),
              mod_error_check_archivist_ui('error_arc_8', error_list[8]),
              mod_error_check_archivist_ui('error_arc_9', error_list[9]),
              mod_error_check_archivist_ui('error_arc_10', error_list[10]),
              mod_error_check_archivist_ui('error_arc_11', error_list[11]),
              mod_error_check_archivist_ui('error_arc_12', error_list[12]),
              mod_error_check_archivist_ui('error_arc_13', error_list[13]),
              mod_error_check_archivist_ui('error_arc_14', error_list[14]),
              mod_error_check_archivist_ui('error_arc_15', error_list[15]),
              mod_error_check_archivist_ui('error_arc_16', error_list[16]),
              mod_error_check_archivist_ui('error_arc_17', error_list[17]),
              mod_error_check_archivist_ui('error_arc_18', error_list[18]),
              mod_error_check_archivist_ui('error_arc_19', error_list[19]),
              mod_error_check_archivist_ui('error_arc_20', error_list[20]),
              mod_error_check_archivist_ui('error_arc_21', error_list[21]),
              mod_error_check_archivist_ui('error_arc_22', error_list[22]),
              mod_error_check_archivist_ui('error_arc_23', error_list[23]),
              checkboxInput('other', 'Other', value = FALSE),
              uiOutput('ui_other'),
              checkboxInput('no_errors_arc', 'No errors', value = FALSE),
              uiOutput('ui_arc_end')
            )
          )
        }
      }
    }
  })
  
  output$ui_big2 <- renderUI({
    li <- logged_in()
    if(li){
      icf_present <- input$icf_present2
      message('---Setting up ui_big2')
      
      # Observe changes to the main dataframe and reset
      x <- dfr2()
      
      if(!is.null(icf_present)){
        if(icf_present == 'Yes'){
          fluidPage(
            fluidRow(
              h3('Errors detected by the archivist'),
              p('Does the ICF present any of the following errors?'),
              helpText('Pay special attention to the previously flagged errors in the above table.'),
              mod_error_check_archivist_ui('error_arc_1b', error_list[1]),
              mod_error_check_archivist_ui('error_arc_2b', error_list[2]),
              mod_error_check_archivist_ui('error_arc_3b', error_list[3]),
              mod_error_check_archivist_ui('error_arc_4b', error_list[4]),
              mod_error_check_archivist_ui('error_arc_5b', error_list[5]),
              mod_error_check_archivist_ui('error_arc_6b', error_list[6]),
              mod_error_check_archivist_ui('error_arc_7b', error_list[7]),
              mod_error_check_archivist_ui('error_arc_8b', error_list[8]),
              mod_error_check_archivist_ui('error_arc_9b', error_list[9]),
              mod_error_check_archivist_ui('error_arc_10b', error_list[10]),
              mod_error_check_archivist_ui('error_arc_11b', error_list[11]),
              mod_error_check_archivist_ui('error_arc_12b', error_list[12]),
              mod_error_check_archivist_ui('error_arc_13b', error_list[13]),
              mod_error_check_archivist_ui('error_arc_14b', error_list[14]),
              mod_error_check_archivist_ui('error_arc_15b', error_list[15]),
              mod_error_check_archivist_ui('error_arc_16b', error_list[16]),
              mod_error_check_archivist_ui('error_arc_17b', error_list[17]),
              mod_error_check_archivist_ui('error_arc_18b', error_list[18]),
              mod_error_check_archivist_ui('error_arc_19b', error_list[19]),
              mod_error_check_archivist_ui('error_arc_20b', error_list[20]),
              mod_error_check_archivist_ui('error_arc_21b', error_list[21]),
              mod_error_check_archivist_ui('error_arc_22b', error_list[22]),
              mod_error_check_archivist_ui('error_arc_23b', error_list[23]),
              checkboxInput('otherb', 'Other', value = FALSE),
              uiOutput('ui_otherb'), 
              checkboxInput('no_errors_arcb', 'No errors', value = FALSE),
              uiOutput('ui_arc_endb') 
            )
          )
        }
      }
    }
  })
  
  output$ui_other <- renderUI({
    li <- logged_in()
    if(li){
      message('---Setting up ui_other')
      
      pd <- input$other
      if(!is.null(pd)){
        if(pd){
          textInput('other_text', 'Specify')
        }
      }
    }
  })
  
  output$ui_otherb <- renderUI({
    li <- logged_in()
    if(li){
      message('---Setting up ui_otherb')
      
      pd <- input$otherb
      if(!is.null(pd)){
        if(pd){
          textInput('other_textb', 'Specify')
        }
      }
    }
  })
  
  error_1 <- callModule(mod_error_check, 'error_1')
  error_2 <- callModule(mod_error_check, 'error_2')
  error_3 <- callModule(mod_error_check, 'error_3')
  error_4 <- callModule(mod_error_check, 'error_4')
  error_5 <- callModule(mod_error_check, 'error_5')
  error_6 <- callModule(mod_error_check, 'error_6')
  error_7 <- callModule(mod_error_check, 'error_7')
  error_8 <- callModule(mod_error_check, 'error_8')
  error_9 <- callModule(mod_error_check, 'error_9')
  error_10 <- callModule(mod_error_check, 'error_10')
  error_11 <- callModule(mod_error_check, 'error_11')
  error_12 <- callModule(mod_error_check, 'error_12')
  
  # error_1b <- callModule(mod_error_check, 'error_1b')
  # error_2b <- callModule(mod_error_check, 'error_2b')
  # error_3b <- callModule(mod_error_check, 'error_3b')
  # error_4b <- callModule(mod_error_check, 'error_4b')
  # error_5b <- callModule(mod_error_check, 'error_5b')
  # error_6b <- callModule(mod_error_check, 'error_6b')
  # error_7b <- callModule(mod_error_check, 'error_7b')
  # error_8b <- callModule(mod_error_check, 'error_8b')
  # error_9b <- callModule(mod_error_check, 'error_9b')
  # error_10b <- callModule(mod_error_check, 'error_10b')
  # error_11b <- callModule(mod_error_check, 'error_11b')
  # error_12b <- callModule(mod_error_check, 'error_12b')
  
  
  error_arc_1 <- callModule(mod_error_check_archivist, 'error_arc_1')
  error_arc_2 <- callModule(mod_error_check_archivist, 'error_arc_2')
  error_arc_3 <- callModule(mod_error_check_archivist, 'error_arc_3')
  error_arc_4 <- callModule(mod_error_check_archivist, 'error_arc_4')
  error_arc_5 <- callModule(mod_error_check_archivist, 'error_arc_5')
  error_arc_6 <- callModule(mod_error_check_archivist, 'error_arc_6')
  error_arc_7 <- callModule(mod_error_check_archivist, 'error_arc_7')
  error_arc_8 <- callModule(mod_error_check_archivist, 'error_arc_8')
  error_arc_9 <- callModule(mod_error_check_archivist, 'error_arc_9')
  error_arc_10 <- callModule(mod_error_check_archivist, 'error_arc_10')
  error_arc_11 <- callModule(mod_error_check_archivist, 'error_arc_11')
  error_arc_12 <- callModule(mod_error_check_archivist, 'error_arc_12')
  error_arc_13 <- callModule(mod_error_check_archivist, 'error_arc_13')
  error_arc_14 <- callModule(mod_error_check_archivist, 'error_arc_14')
  error_arc_15 <- callModule(mod_error_check_archivist, 'error_arc_15')
  error_arc_16 <- callModule(mod_error_check_archivist, 'error_arc_16')
  error_arc_17 <- callModule(mod_error_check_archivist, 'error_arc_17')
  error_arc_18 <- callModule(mod_error_check_archivist, 'error_arc_18')
  error_arc_19 <- callModule(mod_error_check_archivist, 'error_arc_19')
  error_arc_20 <- callModule(mod_error_check_archivist, 'error_arc_20')
  error_arc_21 <- callModule(mod_error_check_archivist, 'error_arc_21')
  error_arc_22 <- callModule(mod_error_check_archivist, 'error_arc_22')
  error_arc_23 <- callModule(mod_error_check_archivist, 'error_arc_23')
  
  error_arc_1b <- callModule(mod_error_check_archivist, 'error_arc_1b')
  error_arc_2b <- callModule(mod_error_check_archivist, 'error_arc_2b')
  error_arc_3b <- callModule(mod_error_check_archivist, 'error_arc_3b')
  error_arc_4b <- callModule(mod_error_check_archivist, 'error_arc_4b')
  error_arc_5b <- callModule(mod_error_check_archivist, 'error_arc_5b')
  error_arc_6b <- callModule(mod_error_check_archivist, 'error_arc_6b')
  error_arc_7b <- callModule(mod_error_check_archivist, 'error_arc_7b')
  error_arc_8b <- callModule(mod_error_check_archivist, 'error_arc_8b')
  error_arc_9b <- callModule(mod_error_check_archivist, 'error_arc_9b')
  error_arc_10b <- callModule(mod_error_check_archivist, 'error_arc_10b')
  error_arc_11b <- callModule(mod_error_check_archivist, 'error_arc_11b')
  error_arc_12b <- callModule(mod_error_check_archivist, 'error_arc_12b')
  error_arc_13b <- callModule(mod_error_check_archivist, 'error_arc_13b')
  error_arc_14b <- callModule(mod_error_check_archivist, 'error_arc_14b')
  error_arc_15b <- callModule(mod_error_check_archivist, 'error_arc_15b')
  error_arc_16b <- callModule(mod_error_check_archivist, 'error_arc_16b')
  error_arc_17b <- callModule(mod_error_check_archivist, 'error_arc_17b')
  error_arc_18b <- callModule(mod_error_check_archivist, 'error_arc_18b')
  error_arc_19b <- callModule(mod_error_check_archivist, 'error_arc_19b')
  error_arc_20b <- callModule(mod_error_check_archivist, 'error_arc_20b')
  error_arc_21b <- callModule(mod_error_check_archivist, 'error_arc_21b')
  error_arc_22b <- callModule(mod_error_check_archivist, 'error_arc_22b')
  error_arc_23b <- callModule(mod_error_check_archivist, 'error_arc_23b')
  
  # Create a reactive object for capturing any errors by part
  error_list_1 <- reactive({
    c(error_1(), error_2(), error_3(), error_4(), error_5(), error_6(),
      error_7(), error_8(), error_9(), error_10(), error_11(), error_12())
  })
  
  # error_list_1b <- reactive({
  #   c(error_1b(), error_2b(), error_3b(), error_4b(), error_5b(), error_6b(),
  #     error_7b(), error_8b(), error_9b(), error_10b(), error_11b(), error_12b())
  # })
  # 
  any_errors_part_1 <- reactive({
    el1 <- error_list_1()
    message('el1 is')
    print(el1)
    out <- any(
      el1,
      na.rm = T)
    message('Any errors in part 1: ', out)
    out
  })
  
  # any_errors_part_1b <- reactive({
  #   el1 <- error_list_1b()
  #   message('el1 is')
  #   print(el1)
  #   out <- any(
  #     el1,
  #     na.rm = T)
  #   message('Any errors in part 1b: ', out)
  #   out
  # })
  
  error_list_2 <- reactive({
    c(error_arc_1(), error_arc_2(), error_arc_3(), error_arc_4(), error_arc_5(), error_arc_6(),
      error_arc_7(), error_arc_8(), error_arc_9(), error_arc_10(), error_arc_11(), error_arc_12(),
      error_arc_13(), error_arc_14(), error_arc_15(), error_arc_16(), error_arc_17(), error_arc_18(),
      error_arc_19(), error_arc_20(), error_arc_21(), error_arc_22(), error_arc_23())
  })
  
  error_list_2b <- reactive({
    c(error_arc_1b(), error_arc_2b(), error_arc_3b(), error_arc_4b(), error_arc_5b(), error_arc_6b(),
      error_arc_7b(), error_arc_8b(), error_arc_9b(), error_arc_10b(), error_arc_11b(), error_arc_12b(),
      error_arc_13b(), error_arc_14b(), error_arc_15b(), error_arc_16b(), error_arc_17b(), error_arc_18b(),
      error_arc_19b(), error_arc_20b(), error_arc_21b(), error_arc_22b(), error_arc_23b())
  })
  
  any_errors_part_2 <- reactive({
    el2 <- error_list_2()
    message('Error list 2 is: ')
    print(el2)
    out <- any(
      el2,
      na.rm = T)
    message('Any errors in part 2: ', out)
    out
  })
  
  any_errors_part_2b <- reactive({
    el2 <- error_list_2b()
    message('Error list 2b is: ')
    print(el2)
    out <- any(
      el2,
      na.rm = T)
    message('Any errors in part 2b: ', out)
    out
  })
  
  list_3_input <- reactive({
    message('---Setting up the list_3_input reactive object')
    # See if "no errors" were declared
    ne1 <- input$no_errors
    ne2 <- input$no_errors_arc
    # See if any errors were ticked
    ae1 <- any_errors_part_1()
    ae2 <- any_errors_part_2()
    # If any error was marked on #7 the information about each error will feed List 3: ICFs to be corrected and be submitted to the server.
    if(ae2){
      # Get the part 2 errors
      e2_index <- error_list_2()
      # Get the person-level data
      p2 <- dfr()
      e2 <- error_list[e2_index]
      combined <- tibble(Error = e2)
      if(nrow(p2) > 0 & nrow(combined) > 0){ # avoids very temporary short errors. not clear why
        for(j in 1:ncol(p2)){
          combined[,names(p2)[j]] <- p2[,j]
        }
      }
      combined
    } else {
      NULL
    }
  })
  
  list_3_inputb <- reactive({
    message('---Setting up the list_3_inputb reactive object')
    # See if "no errors" were declared
    ne2 <- input$no_errors_arcb
    # See if any errors were ticked
    ae2 <- any_errors_part_2b()
    if(ae2){
      # Get the part 2 errors
      e2_index <- error_list_2b()
      # Get the person-level data
      p2 <- dfr2()
      e2 <- error_list[e2_index]
      combined <- tibble(Error = e2)
      # save(combined, e2, p2, e2_index, ae2, ne2, file = '/tmp/xyz2.RData')
      
      if(nrow(p2) > 0 & nrow(combined) > 0){ # avoids very temporary short errors. not clear why
        # Remove previous info on the error
        p2$Error <- p2$archivist <- p2$verification_date <- NULL
        p2 <- p2[1,] # we only want info about the individual, not each error
        for(j in 1:ncol(p2)){
          combined[,names(p2)[j]] <- p2[,j]
        }
      }
      combined
    } else {
      NULL
    }
  })
  
  output$ui_arc_end <- renderUI({
    out <- NULL
    
    # See if "no errors" were declared
    ne1 <- input$no_errors
    ne2 <- input$no_errors_arc
    
    # See if any errors were ticked
    ae1 <- any_errors_part_1()
    ae2 <- any_errors_part_2()
    
    combined <- list_3_input()
    if(!is.null(combined)){
      combined_dt <- bohemia::prettify(combined, download_options = TRUE, nrows = nrow(combined))
      out <-
        fluidPage(
          fluidRow(h1('ICFs to be corrected (the below errors will be added to list 3)')),
          fluidRow(combined_dt),
          fluidRow(actionButton('submit_list_3', 'Submit data'))
        )
    } else {
      if(ae1 & ne2){
        out <- "You have marked errors not corrected in the field by field worker in 'Transcription of errors detected in the field' but have marked no errors in 'Errors detected by the archivist'. Please review."
      } else if(ne2){
        out <- 
          fluidPage(
            h2('Congratulations. You can file the below ICF. It will go to "List 2: Correct ICFs". Click below to do so.'),
            DT::datatable(dfr()),
            actionButton('submit_list_2', 'Submit data')
          )
      } 
      if(!is.null(out)){
        out <- h3(out)
      }
    }
    return(out)
  })

  output$ui_arc_endb <- renderUI({
    out <- NULL
    
    # See if "no errors" were declared
    # ne1 <- input$no_errorsb
    ne2 <- input$no_errors_arcb
    
    # See if any errors were ticked
    # ae1 <- any_errors_part_1b()
    ae2 <- any_errors_part_2b()
    
    combined <- list_3_inputb()
    if(!is.null(combined)){
      combined_dt <- bohemia::prettify(combined, download_options = TRUE, nrows = nrow(combined))
      out <-
        fluidPage(
          fluidRow(h3('ICFs to be corrected (the below errors will be added to list 3)')),
          helpText('IMPORTANT: Any previous errors (table at top of page) which are not in the below table will be marked as corrected.'),
          fluidRow(combined_dt),
          fluidRow(actionButton('submit_list_3b', 'Submit data'))
        )
    } else {
      if(ne2){
        # Get the previous errors 
        x <- dfr2()
        nn <- nrow(x)
        word <- ifelse(nn > 1, 'errors have', 'error has')
        out <- 
          fluidPage(
            h4('You are confirming that the below ', nn, ' ', word, ' been corrected and there are no additional errors. The ICF for the below individual will leave "List 3: ICFs to be corrected" and go to "List 2: Correct ICFs". Click below to do so.'),
            DT::datatable(x),
            actionButton('submit_list_22', 'Submit data')
          )
      } 
      if(!is.null(out)){
        out <- h3(out)
      }
    }
    return(out)
  })
  
  # Observe submission to lists and move over  
  observeEvent(input$submit_list_1,{
    # Capture the row that is getting submitted
    this_row <- dfr()
    this_row$attempts <- this_row$attempts + 1
    # Remove the submitted row from the main table
    new_main <- remove_from('main', this_row$id, data_list = data_list)
    data_list$main <- new_main
    # Add the submitted row to list 1
    new_1 <- add_to_list(1, this_row, data_list = data_list)
    data_list$list1 <- new_1
    # Save the data for permanence
    dl <- reactiveValuesToList(data_list)
    save(dl, file = 'all_data.RData')
    # Flash the main table
    x <- dfr(); x<- dfr2()    
  })
  
  observeEvent(input$submit_list_2,{
    # Capture the row that is getting submitted
    this_row <- dfr()
    this_row$attempts <- this_row$attempts + 1
    # Remove the submitted row from the main table
    new_main <- remove_from('main', this_row$id, data_list = data_list)
    data_list$main <- new_main
    # Add the submitted row to list 2
    new_2 <- add_to_list(2, this_row, data_list = data_list)
    data_list$list2 <- new_2
    # Save the data for permanence
    dl <- reactiveValuesToList(data_list)
    save(dl, file = 'all_data.RData')
    # Flash the main table
    x <- dfr(); x<- dfr2()    
  })
  
  observeEvent(input$submit_list_22,{
    # Capture the row that is getting submitted
    this_row <- dfr2()
    
    # Remove the submitted row from the table 3 (incorrect)
    new_list <- remove_from(3, this_row$id, data_list = data_list)
    data_list$list3 <- new_list
    # Add the submitted row to list 2 (correct) - note, if multiple errors, just add one
    this_row <- this_row[1,]
    this_row <- this_row %>% dplyr::select(-Error)
    this_row$attempts <- this_row$attempts + 1
    new_2 <- add_to_list(2, this_row, data_list = data_list)
    data_list$list2 <- new_2
    # Save the data for permanence
    dl <- reactiveValuesToList(data_list)
    save(dl, file = 'all_data.RData')
    # Flash the main table
    x <- dfr(); x<- dfr2()    
  })
  
  observeEvent(input$submit_list_3,{
    # Capture the row that is getting submitted
    this_row <- list_3_input()
    this_row$attempts <- this_row$attempts + 1
    # Remove the submitted row from the main table
    new_main <- remove_from('main', this_row$id, data_list = data_list)
    data_list$main <- new_main
    # Add the submitted row to list 2
    new_3 <- add_to_list(3, this_row, data_list = data_list)
    data_list$list3 <- new_3
    # Save the data for permanence
    dl <- reactiveValuesToList(data_list)
    save(dl, file = 'all_data.RData')
    # Flash the main table
    x <- dfr(); x<- dfr2()    
  })
  
  observeEvent(input$submit_list_3b,{
    # Remove any previous errors from the list of errors
    this_row <- dfr2()
    this_row$attempts <- this_row$attempts + 1
    new_list <- remove_from(3, this_row$id, data_list = data_list)
    data_list$list3 <- new_list
    # Capture the new errors that are being submitted
    this_row <- list_3_inputb()
    # Remove the submitted row from the main table (if applicable)
    new_main <- remove_from('main', this_row$id, data_list = data_list)
    data_list$main <- new_main
    # Add the submitted row to list 3 (errors)
    new_3 <- add_to_list(3, this_row, data_list = data_list)
    data_list$list3 <- new_3
    # Save the data for permanence
    dl <- reactiveValuesToList(data_list)
    save(dl, file = 'all_data.RData')
    # Flash the main tables
    x <- dfr(); x<- dfr2()    
  })
  
  # List UIs
  output$ui_list_1_table <- DT::renderDataTable({
    out <- data_list$list1
    ok <- FALSE
    if(!is.null(out)){
      if(nrow(out) > 0){
        ok <- TRUE
      }
    }
    if(ok){
      bohemia::prettify(out, nrows = nrow(out), download_options = TRUE)
    } else {
      NULL
    }
    
  })
  output$ui_list_1 <- renderUI({
    fluidPage(
      fluidRow(
        DT::dataTableOutput('ui_list_1_table')
      )
    )
  })
  
  unmissing <- reactiveValues(value = data.frame())
  output$ui_list_1b <- renderUI({
    ok <- FALSE
    out <- data_list$list1
    ok <- FALSE
    rs <- input$ui_list_1_table_rows_selected
    if(!is.null(out)){
      if(nrow(out) > 0){
        if(!is.null(rs)){
          if(length(rs) > 0){
            ok <- TRUE
          }
        }
      }
    }
    if(ok){
      fluidRow(
        column(12, align = 'center',
               actionButton('unmiss', 'Mark as no longer missing'))
      )
    } else {
      NULL
    }
  })
  observeEvent(input$unmiss,{
    # Get the unmissing index
    rs <- input$ui_list_1_table_rows_selected
    # Get the unmissing data
    out <- data_list$list1
    # Subset just the row
    final <- out[rs,]
    # Update the object
    message('The no longer missing row is: ')
    unmissing$value <- final
    print(final)
    # Remove from list 1
    new_list_1 <- data_list$list1[!((1:nrow(data_list$list1)) %in% rs),]
    data_list$list1 <- new_list_1
    # Add to main list
    old_main <- data_list$main
    print('old_main looks like')
    print(old_main)
    print('final looks like')
    print(final)
    new_main <- bind_rows(old_main, final)
    print('new_main looks like')
    print(new_main)
    data_list$main <- new_main
  })
  
  output$ui_list_2 <- renderUI({
    out <- data_list$list2
    ok <- FALSE
    if(!is.null(out)){
      if(nrow(out) > 0){
        ok <- TRUE
      }
    }
    if(ok){
      return(
        fluidRow(
          bohemia::prettify(out, nrows = nrow(out), download_options = TRUE)
        )
      )
    } else {
      return(h5('No entries yet.'))
    }
  })
  output$ui_list_3 <- renderUI({
    out <- data_list$list3
    ok <- FALSE
    if(!is.null(out)){
      if(nrow(out) > 0){
        ok <- TRUE
      }
    }
    if(ok){
      return(
        fluidRow(
          bohemia::prettify(out, nrows = nrow(out), download_options = TRUE)
        )
      )
    } else {
      return(h5('No entries yet.'))
    }
  })
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
mobile_golem_add_external_resources <- function(){
  # addResourcePath(
  #   'www', system.file('app/www', package = 'icf')
  # )
  
  
  share <- list(
    title = "Bohemia ICF tool",
    url = "https://bohemia.team/icf/",
    image = "https://www.databrew.cc/images/logo_clear.png",
    description = "Bohemia app",
    twitter_user = "data_brew"
  )
  
  tags$head(
    
    
    # If you have a custom.css in the inst/app/www
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}