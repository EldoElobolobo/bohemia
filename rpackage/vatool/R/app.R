
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
#' @import DBI
#' @import RPostgres
#' @import dplyr
app_ui <- function(request) {
  options(scipen = '999')
  
  tagList(
    mobile_golem_add_external_resources(),
    
    dashboardPage(
      dashboardHeader (title = "Bohemia VA tool", uiOutput('top_button')),
      dashboardSidebar(
        sidebarMenu(
          menuItem(
            text="Main",
            tabName="main"),
          menuItem(
            text="History",
            tabName="history"),
          menuItem(
            text="Adjudicate",
            tabName="adjudicate"),
          menuItem(
            text="Unresolved VAs",
            tabName="adjudicate_two"),
          menuItem(
            text="Summary",
            tabName="summary"),
          menuItem(
            text = 'About',
            tabName = 'about')
        )),
      dashboardBody(
        tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #ffe9e9 !important;}')),
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
          tabItem(
            tabName="main",
            fluidPage(
              fluidRow(
                selectInput(inputId = 'country',
                            label = 'Choose country',
                            choices = c('Mozambique', 'Tanzania'),
                            selected = 'Mozambique')
              ),
              fluidRow(
                column(12, 
                       # uiOutput('top_button'),
                       # br(),
                       div(class = 'tableCard',
                           uiOutput('ui_main', inline = TRUE)
                       )
                )
              ),
              br(),
              fluidRow(
                column(8,
                       div(class = 'tableCard',
                           h1('VA form'),
                           DT::dataTableOutput('va_table')
                       )
                ),
                column(4,
                         div(class = 'tableCard',
                             h3('Physician inputs'),
                             uiOutput('ui_select_va'),
                             uiOutput('ui_assign_cod'),
                             uiOutput('ui_submission')
                         )
                       
                     
                )
              )
            )
          ),
          tabItem(
            tabName = 'history',
            fluidRow(
              column(12,
                     h2('Adjudicator info'),
                     div(class = 'tableCard',
                         DT::dataTableOutput('user_table')
                     ),
                     
                     br(),
                     
                     h2('Submission history'),
                     div(class = 'tableCard',
                         DT::dataTableOutput('history_table')
                     )
              )
            )
          ),
          tabItem(
            tabName = 'adjudicate',
            fluidRow(
              uiOutput('ui_adjudicate')
            )
          ),
          tabItem(
            tabName = 'adjudicate_two',
            fluidRow(
              uiOutput('ui_adjudicate_two')
            )
          ),
          tabItem(
            tabName = 'summary',
            fluidRow(
              div(class = "tableCard",
                  h2('Individual submissions'),
                  DT::dataTableOutput('summary_1')),
              br(), br(),
              div(class = "tableCard",
                  h2('Country submissions'),
                  DT::dataTableOutput('summary_2'))
            )
          ),
          tabItem(
            tabName = 'about',
            make_about()
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
#' @import dplyr
app_server <- function(input, output, session) {
  
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  is_local <- ifelse(is_aws, FALSE, TRUE)
  # is_local <- FALSE
  if(is_local){
    message('Running with the local datbase---------------------')
  } else {
    message('Running with the remote AWS database---------------')
  }
  
  logged_in <- reactiveVal(value = FALSE)
  submission_success <- reactiveVal(value = NULL)
  adj_submission_success <- reactiveVal(value = NULL)
  adj_two_submission_success <- reactiveVal(value = NULL)
  
  log_in_fail <- reactiveVal(value=FALSE)
  # Connect to database
  message('Connecting to database : ', ifelse(is_local, ' local', 'remote'))
  con <- get_db_connection(local = is_local)
  # Get list of authorized users, session, and cod tables
  users <- dbReadTable(conn = con, 'vatool_users')
  cods <- dbReadTable(conn=con, 'vatool_cods')
  data <- reactiveValues(va = data.frame(), session = data.frame(), cod = data.frame())
  # save(users, cods, file = 'va_data.rda')
  # print(users)
  
  # Upon log in, read in data
  observeEvent(input$log_in,{
    message('in observeEvent(input$log_in')
    liu <- input$log_in_user
    lip <- input$log_in_password
    # See if the user exists and the password is correct
    ok <- FALSE
    if(!is.null(users)){
      message('at point 6')
      if(tolower(liu) %in% users$username & tolower(lip) %in% users$password){
        ok <-TRUE
      }
    }
    
    if(ok){
      logged_in(TRUE)
      removeModal()
      
      # load data
      data$va <- load_va_data(is_local = is_local)
      # data$va <- readRDS('~/Desktop/va_data.rda')

      # print(head(data$va))
      # create table with same columns as session table in database (to append upon logout)
      print(users)
      message('at point 2')
      # save(users, file = '/tmp/users.RData')
      user_id <- users %>% dplyr::filter(username == tolower(liu)) %>% .$user_id
      start_time <- Sys.time()
      end_time <- NA
      data$session <- dplyr::tibble(user_id = user_id, start_time=start_time, end_time=end_time)
      
      # create cod table
      data$cod <- dplyr::tibble(user_id = user_id, death_id = NA, cod_code_1 = NA,cod_1 =NA,cod_code_2 = NA,cod_2 =NA,cod_code_3 = NA,cod_3 =NA, time_stamp = NA, doctor_note = NA)
      
    } else {
      logged_in(FALSE)
      log_in_fail(TRUE)
      removeModal()
    }
    
  })
  table_cods <- reactiveValues(data=cods)

  # # Summary table 1
  output$summary_1 <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    if(li){
      temp_cods <- table_cods$data
      
      # join users and cods
      out <- left_join(users, temp_cods) %>%
        group_by(`Last name` = last_name) %>%
        summarise(`Number of VAs coded` = length(which(!is.na(time_stamp))))
    }
    if(!is.null(out)){
      if(is.data.frame(out)){
        DT::datatable(out, rownames = FALSE, options = list(
          columnDefs = list(list(className= 'dt-left',width = '0.5px', targets = 1))
        ))
        
      }
    }
  })
  
  # # Summary table 2
  output$summary_2 <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    if(li){
      pd <- data$va
      pd <- pd %>% group_by(country = server) %>%
        summarise(`Total VAs` = n()) %>%
        mutate(country = ifelse(grepl('ihi', country), 'Tanzania', 'Mozambique'))
      
      temp_cods <- table_cods$data
      
      out <- left_join(users, temp_cods) %>%
        group_by(country) %>%
        summarise(`Number of VAs coded` = length(which(!is.na(time_stamp)))) %>% 
        inner_join(pd)
      # save(out, file = 'temp_out.rda')
      out$`Weekly Target`<- ifelse(out$country == 'Mozambique', 59, 100)
      # add in target
      out <- out %>% select(Country = country,`Number of VAs coded`,`Weekly Target`,  `Total VAs`)
      
    }
    if(!is.null(out)){
      if(is.data.frame(out)){
        DT::datatable(out, rownames = FALSE, options = list(
          columnDefs = list(list(className= 'dt-left',width = '30px', targets = c(0,1,2,3)))
        ))
      }
    }
  })
  
  # UNRESOLVED VAS
  # Selection input for VA ID
  output$ui_adjudicate_two <- renderUI({
    li <- logged_in()
    uu <- !is.null(users)
    li <- li & uu
    if(li){
      cn <- input$country
      liu <- input$log_in_user
      
      death_id_choices <- va_choices$adjudicator_two_choices
      
      cods_choices <- cod_choices(country = cn)
      # save(users, file = 'temp_users.rda')
      
      liu <- input$log_in_user
      if(liu %in% c('ben', 'charfudin', 'carlos','hansel')){
       
          fluidPage(
            fluidRow(
              column(12,
                     h2('Causes of deaths from other physicians')
              ),
              column(9,
                     
                     div(class = "tableCard",
                         DT::dataTableOutput('adj_two_table_2')
                     ),
                     h2('VA form'),
                     div(class = "tableCard",
                         DT::dataTableOutput('adj_two_table_1')
                     )
              ),
              column(3,
                     # h2(' '),
                     
                     div(class = "tableCard",
                         selectInput('adj_two_death_id', 'Select the VA ID', choices = death_id_choices),
                         selectInput('adj_two_cods', 'Select underlying cause of death',  choices = c('', sort(unique(names(cods_choices))))),
                         textAreaInput(inputId = 'adj_two_phy_notes', label = 'Enter additional notes on cause of death', value = NULL),
                         actionButton('adj_two_submit_cod', 'Submit cause of death', width = '170px'),
                         uiOutput('ui_two_submission_adj')
                     )
              )
            )
          )
        
      } else {
        h4('This page can only be viewed by the final adjudicator')
      }
      
    } else {
      NULL
    }
  })
  
  # table showing 
  output$adj_two_table_1 <- DT::renderDataTable({
    li <- logged_in()
    cn <- input$country
    out <- data_frame(' ' = 'No VAs to adjudicate')
    if(li){
      idi <- input$adj_two_death_id
      if(!is.null(idi) & idi != ''){
        pd <- data$va 
        if(cn == 'Mozambique'){
          pd <- pd %>% filter(grepl('manhica', server))
        } else {
          pd <- pd %>% filter(grepl('ihi', server))
        }
        person <- pd %>% filter(death_id == idi)
        
        # get pdf title 
        pdf_file <- paste0('va_', idi)
        pdf_title <- paste0('VA for ID ', idi)   
        # remove other columns
        remove_these <- "write your 3 digit|Id10007|server|first or given|the surname|name of VA|1	Manually write your 3 digit worker ID here|tz001|this_usernameTake a picture of the painted Household ID|isadult1|isadult2|isneonatal|isneonatal2|ischild1|ischild2|instancename|instance_id|device_id|end_time|start_time|todays_date|wid|Do you have a QR code with your worker ID?|wid|ageindays|ageindaysneonate|ageinmonths|ageinmonthsbyyear|ageinmonthsremain|ageinyears2|ageinyearsremain|The GPS coordinates represents|Collect the GPS coordinates of this location|Does the house you're at have a painted ID number on it?|hh_id|Write the 6 digit household ID here|id10007|id10008|id10009|id10010|id10010a|id10010b|id10011|id10013|id10017|id10018|id10018d|id10020|id10022|id10023|id10052|id10053|id10057|id10061|id10062|id10069|id10007|id10008|id10009|id10010|id10010a|id10010b|id10011|id10013|id10017|id10018|id10018d|id10020|id10022|id10023|id10052|id10053|id10057|id10061|id10062|id10069|id10458|id10459|id10462"
        
        # remove columns with NA
        person <- person[, !grepl(remove_these, names(person))]
        person <- get_va_names(person, country = cn)
        person <- person[, apply(person, 2, function(x) !any(is.na(x)))] 
        # person <- person[, apply(person, 2, function(x) x != 'no')]
        out <- as.data.frame(t(person))
        out$Question <- rownames(out)

        if(ncol(out)==2){
          names(out) <- c('Answer', 'Question')
          rownames(out) <- NULL
          out <- out[, c('Question', 'Answer')]
          DT::datatable(out, extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                    pageLength = nrow(out),
                                                                    buttons = list(
                                                                      list(extend = 'copy'),
                                                                      list(extend = 'pdf',
                                                                           filename = pdf_file,
                                                                           title = pdf_title,
                                                                           header = FALSE)
                                                                    )
          ))
        } else {
          out <- NULL
        }
      }
      out
    } 
   
  })
  
  # table showing 
  output$adj_two_table_2 <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    if(li){
      idi <- input$adj_two_death_id
      
      if(!is.null(idi)){
        out <- table_cods$data %>% filter(death_id == idi)
      }
    }
    if(!is.null(out)){
      names(out) <- c('User ID', 'Death ID', 'Immediate COD code', 'Immediate COD', 'Intermediary COD code', 'Intermediary COD', 'Underlying COD code', 'Underlying COD', 'Time stamp', 'Physician Notes')
      DT::datatable(out, options = list(scrollX = TRUE))
      
    }
    
  })
  
  
  
  
  
  
  
  # Selection input for VA ID
  output$ui_adjudicate <- renderUI({
    li <- logged_in()
    uu <- !is.null(users)
    li <- li & uu
    if(li){
      cn <- input$country
      liu <- input$log_in_user
     
      death_id_choices <- va_choices$adjudicator_choices
      
      cods_choices <- cod_choices(country = cn)
      # save(users, file = 'temp_users.rda')
      
      liu <- input$log_in_user
      if(liu %in% c('ben', 'charfudin', 'carlos', 'robert', 'joyce', 'celso', 'elisio', 'hansel')){
        if(cn == 'Mozambique'){
          fluidPage(
            fluidRow(
             
              column(9,
                     
                     h2('VA form'),
                     div(class = "tableCard",
                         DT::dataTableOutput('adj_table_1')
                     )
              ),
              column(3,
                     # h2(' '),
                     
                     div(class = "tableCard",
                         selectInput('adj_death_id', 'Select the VA ID', choices = death_id_choices),
                         selectInput('adj_cods', 'Select underlying cause of death',  choices = c('', sort(unique(names(cods_choices))))),
                         textAreaInput(inputId = 'adj_phy_notes', label = 'Enter additional notes on cause of death', value = NULL),
                         actionButton('adj_submit_cod', 'Submit cause of death', width = '170px'),
                         uiOutput('ui_submission_adj')
                     )
              )
            )
          )
        } else {
          fluidPage(
            fluidRow(
              
              column(9,
                     h2('Cause of death from other physicians'),
                     
                     div(class = "tableCard",
                         DT::dataTableOutput('adj_table_2')
                     ),
                     h2('VA form'),
                     div(class = "tableCard",
                         DT::dataTableOutput('adj_table_1')
                     )
              ),
              column(3,
                     # h2(' '),
                     
                     div(class = "tableCard",
                         selectInput('adj_death_id', 'Select the VA ID', choices = death_id_choices),
                         selectInput('adj_cods', 'Select underlying cause of death',  choices = c('', sort(unique(names(cods_choices))))),
                         textAreaInput(inputId = 'adj_phy_notes', label = 'Enter additional notes on cause of death', value = NULL),
                         actionButton('adj_submit_cod', 'Submit cause of death', width = '170px'),
                         uiOutput('ui_submission_adj')
                     )
              )
            )
          )
        }
      } else {
        h4('This page can only be viewed by Adjudicators')
      }

    } else {
      NULL
    }
  })
  
  # table showing 
  output$adj_table_1 <- DT::renderDataTable({
    li <- logged_in()
    cn <- input$country
    out <- data_frame(' ' = 'No VAs to adjudicate')
    if(li){
      idi <- input$adj_death_id
      if(!is.null(idi) & idi != ''){
        pd <- data$va 
        if(cn == 'Mozambique'){
          pd <- pd %>% filter(grepl('manhica', server))
        } else {
          pd <- pd %>% filter(grepl('ihi', server))
        }
        person <- pd %>% filter(death_id == idi)
        # get pdf title
        pdf_file <- paste0('va_', idi)
        pdf_title <- paste0('VA for ID ', idi)
        # remove other columns
        remove_these <- "write your 3 digit|Id10007|server|first or given|the surname|name of VA|1	Manually write your 3 digit worker ID here|tz001|this_usernameTake a picture of the painted Household ID|isadult1|isadult2|isneonatal|isneonatal2|ischild1|ischild2|instancename|instance_id|device_id|end_time|start_time|todays_date|wid|Do you have a QR code with your worker ID?|wid|ageindays|ageindaysneonate|ageinmonths|ageinmonthsbyyear|ageinmonthsremain|ageinyears2|ageinyearsremain|The GPS coordinates represents|Collect the GPS coordinates of this location|Does the house you're at have a painted ID number on it?|hh_id|Write the 6 digit household ID here|id10007|id10008|id10009|id10010|id10010a|id10010b|id10011|id10013|id10017|id10018|id10018d|id10020|id10022|id10023|id10052|id10053|id10057|id10061|id10062|id10069|id10007|id10008|id10009|id10010|id10010a|id10010b|id10011|id10013|id10017|id10018|id10018d|id10020|id10022|id10023|id10052|id10053|id10057|id10061|id10062|id10069|id10458|id10459|id10462"
        
        # remove columns with NA
        person <- person[, !grepl(remove_these, names(person))]
        person <- get_va_names(person, country = cn)
        person <- person[, apply(person, 2, function(x) !any(is.na(x)))] 
        # person <- person[, apply(person, 2, function(x) x != 'no')]
        out <- as.data.frame(t(person))
        out$Question <- rownames(out)
        if(ncol(out)==2){
          names(out) <- c('Answer', 'Question')
          rownames(out) <- NULL
          out <- out[, c('Question', 'Answer')]
          DT::datatable(out, extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                    pageLength = nrow(out),
                                                                    buttons = list(
                                                                      list(extend = 'copy'),
                                                                      list(extend = 'pdf',
                                                                           filename = pdf_file,
                                                                           title = pdf_title,
                                                                           header = FALSE)
                                                                    )
          ))
        } else {
          out <- NULL
        }
      }
      out
    } 
  
  })
  
  # table showing 
  output$adj_table_2 <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    if(li){
      idi <- input$adj_death_id
      
      if(!is.null(idi)){
        out <- table_cods$data %>% filter(death_id == idi)
      }
    } 
    names(out) <- c('User ID', 'Death ID', 'Immediate COD code', 'Immediate COD', 'Intermediary COD code', 'Intermediary COD', 'Underlying COD code', 'Underlying COD', 'Time stamp', 'Physician Notes')
    DT::datatable(out, options = list(scrollX = TRUE))
    
  })
  
  # Define the button
  output$top_button <- renderUI({
    li <- logged_in()
    if(li){
      actionButton("log_out", "Log out")
    } else {
      actionButton("show", "Log in")
    }
  })
  
  # Define the placeholder main ui
  output$ui_main <- renderUI({
    li <- logged_in()
    lif <- log_in_fail()
    if(li){
      liu <- input$log_in_user
      li_text <- paste0('Welcome ', liu)
      p(li_text)
      
      
    } else if (lif){
      p('Username or password incorrect')
    } else {
      p('Not logged in')
    }
    
    
  })
  
  # Create a reactive object to store the choices of death ids
  va_choices <- reactiveValues(choices = c(),
                               adjudicator_choices = c(),
                               adjudicator_two_choices = c())
  
  # Observe the log in, and upon log in, populate the roster of choices
  observeEvent(c(input$log_in,input$country, input$submit_cod, input$adj_submit_cod), {
    message('JUST LOGGED IN')
    li <- logged_in()
    # if they just logged in, populate choices of deaht ids
    cn <- input$country
    ok <- FALSE
    message('making va choices: cn is ', cn)
    message('making va choices: li is ', li)
    if(li){
      if(!is.null(cn)){
        if(nchar(cn) > 0){
          ok <- TRUE
        }
      }
    }
    if(ok){
      pd <- data$va

      if(cn == 'Mozambique'){
        pd <- pd %>% filter(grepl('manhica', server))
      } else {
        pd <- pd %>% filter(grepl('ihi', server))
      }
      # save(pd, file = 'temp_pd.rda')
      country_ids <- unique(pd$death_id)
      liu <- input$log_in_user
      user <- users %>% dplyr::filter(username == tolower(liu))
      userid <- user %>% dplyr::filter(username == tolower(liu)) %>% .$user_id
      # save(pd, liu, user, userid,cods,  file = 'temp_choice.rda')
      out <-table_cods$data %>% dplyr::filter(user_id == userid)
      choices <- pd$death_id
      # Removing already used VA ID from the list of the user
      choices <- setdiff(choices, out$death_id)
      # Removing VA IDs with more than one frequency  
      frequency<-as.data.frame(table(table_cods$data$death_id))
      emit<-frequency %>% dplyr::filter(Freq > 1)
      choices <- setdiff(choices, emit$Var1)
      choices <- choices[!is.na(choices)]
      va_choices$choices <- choices
      
      # Also populate the adjudicator choices
      # get ids that have more than one diagnosis
      user <- users %>% dplyr::filter(username == tolower(liu))
      userid <- user %>% dplyr::filter(username == tolower(liu)) %>% .$user_id
      
      # make sure user cant adjudicate a VA he already adjudicatd 
      # cods <- cods %>% filter(user_id != userid)
      death_id_choices <- table_cods$data%>% 
        filter(death_id %in% country_ids) %>%
        group_by(death_id, cod_3) %>% 
        summarise(counts = n()) %>%
        group_by(death_id) %>% 
        summarise(agree_counts = sum(counts),
                  adj_counts = n()) %>% 
        filter(adj_counts == 2, agree_counts < 3) %>% 
        .$death_id
      # death_id_choices <- unique(death_id_choices)
      
      va_choices$adjudicator_choices <- death_id_choices
      
      # get unresolved VAs
      # cods <- cods %>% filter(user_id != userid)
      death_id_choices <- table_cods$data%>% 
        filter(death_id %in% country_ids) %>%
        group_by(death_id, cod_3) %>% 
        summarise(counts = n()) %>%
        group_by(death_id) %>% 
        summarise(agree_counts = sum(counts),
                  adj_counts = n()) %>% 
        filter(adj_counts == 3, agree_counts < 4) %>% 
        .$death_id
      
      va_choices$adjudicator_two_choices <- death_id_choices
    }
  })
  
  
  # Selection input for VA ID
  output$ui_select_va <- renderUI({
    message('in ui_select_va')
    li <- logged_in()
    cn <- input$country
    ok <- FALSE
    message('cn is ', cn)
    message('li is ', li)
    if(li){
      if(!is.null(cn)){
        if(nchar(cn) > 0){
          ok <- TRUE
        }
      }
    }
    if(ok){
      choices <- va_choices$choices
      selectInput('death_id', 'Select the VA ID', choices = sort(unique(choices)), selected = sort(unique(choices))[1]) 
    } else {
      NULL
    }
  })
  
  output$ui_assign_cod <- renderUI({
    li <- logged_in()
    cn <- input$country
    ok <- FALSE
    if(li){
      if(!is.null(cn)){
        if(nchar(cn) > 0){
          ok <- TRUE
        }
      }
    }
    if(ok){
      message('about to run cod_choices')
      vas <- va_choices$choices
      
      choices <- cod_choices(country = cn)
      
      fluidPage(
        fluidRow(
          selectInput('cod_1', 'Select immediate cause of death', choices =c('', sort(unique(names(choices))))),
          selectInput('cod_2', 'Select intermediary cause of death', choices =c('', sort(unique(names(choices))))),
          selectInput('cod_3', 'Select underlying cause of death', choices =c('', sort(unique(names(choices))))),

        textAreaInput(inputId = 'phy_notes', label = 'Enter additional notes on cause of death', value = NULL),
       
        actionButton('submit_cod', 'Submit causes of death and notes')
          
        )
      )
    } else {
      NULL
    }
  })
  
  
  output$va_table <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    cn <- input$country
    idi <- input$death_id
    ok <- FALSE
    message('va_table cn is ', cn)
    message('va_table li is ', li)
    if(li){
      if(!is.null(cn)){
        if(nchar(cn) > 0){
          if(!is.null(idi)){
            ok <- TRUE
          }
        }
      }
    }
    if(ok){
      pd <- data$va
      if(cn == 'Mozambique'){
        pd <- pd %>% filter(grepl('manhica', server))
      } else {
        pd <- pd %>% filter(grepl('ihi', server))
      }
      person <- pd %>% filter(death_id == idi)
      
      # get pdf title
      pdf_file <- paste0('va_', idi)
      pdf_title <- paste0('VA for ID ', idi)      # save(person, file = 'temp_person.rda')
      # remove other columns 
      remove_these <- "write your 3 digit|Id10007|server|first or given|the surname|name of VA|1	Manually write your 3 digit worker ID here|tz001|this_usernameTake a picture of the painted Household ID|isadult1|isadult2|isneonatal|isneonatal2|ischild1|ischild2|instancename|instance_id|device_id|end_time|start_time|todays_date|wid|Do you have a QR code with your worker ID?|wid|ageindays|ageindaysneonate|ageinmonths|ageinmonthsbyyear|ageinmonthsremain|ageinyears2|ageinyearsremain|The GPS coordinates represents|Collect the GPS coordinates of this location|Does the house you're at have a painted ID number on it?|hh_id|Write the 6 digit household ID here|id10007|id10008|id10009|id10010|id10010a|id10010b|id10011|id10013|id10017|id10018|id10018d|id10020|id10022|id10023|id10052|id10053|id10057|id10061|id10062|id10069|id10007|id10008|id10009|id10010|id10010a|id10010b|id10011|id10013|id10017|id10018|id10018d|id10020|id10022|id10023|id10052|id10053|id10057|id10061|id10062|id10069|id10458|id10459|id10462"
      
      person <- person[, !grepl(remove_these, names(person))]
      # person <- person[,apply(person, 2, function(x) x != 'no')]
      if(nrow(person)!=0){
        # save(person, file = 'person.rda')
        person <- get_va_names(person, country = cn)
        # remove columns with NA
        person <- person[ , apply(person, 2, function(x) !any(is.na(x)))]
        out <- as.data.frame(t(person))
        out$Question <- rownames(out)
        message('in va_table number of columns in va form ', ncol(out))
        if(ncol(out)==2){
          names(out) <- c('Answer', 'Question')
          rownames(out) <- NULL
          out <- out[, c('Question', 'Answer')]
        } else {
          out <- NULL
        }
      } else {
        out <- NULL
      }
      if(!is.null(out)){
        if(is.data.frame(out)){
          DT::datatable(out, extensions = 'Buttons', options = list(scrollY = '350px',
                                                                    dom = 'Bfrtip',
                                                                    pageLength = nrow(out),
                                                                    buttons = list(
                                                                      list(extend = 'copy'),
                                                                      list(extend = 'pdf',
                                                                           filename = pdf_file,
                                                                           title = pdf_title,
                                                                           header = FALSE)
                                                                    )
          ))
          # databrew::prettify(out, nrows = nrow(out), download_options = TRUE)
        }
      }
     
    }
    
  })
  
  observeEvent(input$show, {
    # logged_in(TRUE)
    showModal(modalDialog(
      title = "Log in",
      # fluidPage(
      # fluidRow(
      div(
        textInput('log_in_user', 'User')),
      div(
        passwordInput('log_in_password', 'Password')),
      # )
      # ,
      # fluidRow(
      #   actionButton('log_in', 'Log in')
      # )
      # ),
      
      footer =  tagList(
        modalButton("Dismiss"),
        actionButton('log_in', 'Log in')
      )
    ))
  })
  observeEvent(input$log_out, {
    logged_in(FALSE)
    session_data <- data$session
    session_data$end_time <- Sys.time()
    dbAppendTable(conn = con, name = 'vatool_sessions', value = session_data)
  })
  
  # Observe submission of cause of death and save
  observeEvent(input$submit_cod, {
    cn <- input$country
    cod_names <- cod_data(country = cn)
    cod_data <- data$cod
    pn <- input$phy_notes
    cod_1 = input$cod_1
    cod_2 = input$cod_2
    cod_3 = input$cod_3
    # save(cod_data, pn, cod_1, cod_2, cod_3,cod_names, file = 'temp_cods.rda')
    # condition if underlying cause of death is not fiilled out, wont submit
    if(cod_3==''){
      submission_success(FALSE)
    } else {
      death_id = input$death_id
      # save(death_id, cod_1, cod_2, cod_3, cod_data, cod_names, file = 'temp_submit.rda')
      cod_data$cod_1 = paste0(unlist(lapply(strsplit(input$cod_1,split = ' '), function(x) x[-1])), collapse = ' ')
      cod_data$cod_2 = paste0(unlist(lapply(strsplit(input$cod_2,split = ' '), function(x) x[-1])), collapse = ' ')
      cod_data$cod_3 = paste0(unlist(lapply(strsplit(input$cod_3,split = ' '), function(x) x[-1])), collapse = ' ')
      cod_data$death_id = death_id
      cod_data$time_stamp <- Sys.time()
      
      # ISSUE HERE IS THAT SOME (LIKE DIARRHOEA) ARE ASSOCIATED WITH TWO CODES AND VICE VERSA
      cod_data$cod_code_1 <- cod_names$cod_code[cod_names$cod_names==cod_data$cod_1][1]
      cod_data$cod_code_2 <- cod_names$cod_code[cod_names$cod_names==cod_data$cod_2][1]
      cod_data$cod_code_3 <- cod_names$cod_code[cod_names$cod_names==cod_data$cod_3][1]
      cod_data$doctor_note <- pn
      dbAppendTable(conn = con, name = 'vatool_cods', value = cod_data)
      old_cods <- table_cods$data
      new_cods <- rbind(old_cods, cod_data)
      table_cods$data <- new_cods
      # remove VA id from choices 
      old_choices <- va_choices$choices 
      new_choices <- old_choices[!old_choices %in% death_id]
      va_choices$choices <- new_choices
      submission_success(TRUE)
    }
    
  })
  
  # Observe changes in inputs
  observeEvent(c(input$cod,input$death_id), {
    submission_success(NULL)
  })
  
  output$ui_submission <- renderUI({
    ss <- submission_success()
    if(is.null(ss)){
      NULL
    } else if(ss){
      h3('Submission successful')
    } else {
      h3('Submission unsuccessful')
    }
  })
  
  # history table 
  output$user_table <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    if(li){
      if(!is.null(users)){
        message('at point 4')
        liu <- input$log_in_user
        out <- users %>% dplyr::filter(username == tolower(liu))
        names(out) <- c('User ID', 'Username', 'Password', 'First name', 'Last name', 'Country', 'Role')
        out
      }
    } 
    out
  })
  
  # history table 
  output$history_table <- DT::renderDataTable({
    li <- logged_in()
    out <- NULL
    if(li){
      if(!is.null(users)){
        message('at point 5')
        liu <- input$log_in_user
        user <- users %>% dplyr::filter(username == tolower(liu))
        userid <- user %>% dplyr::filter(username == tolower(liu)) %>% .$user_id
        temp_cods <- table_cods$data
        
        out <- temp_cods %>% dplyr::filter(user_id == userid)
        names(out) <-  c('User ID', 'Death ID', 'Immediate COD code', 'Immediate COD', 'Intermediary COD code', 'Intermediary COD', 'Underlying COD code', 'Underlying COD', 'Time stamp', 'Physician Notes')
      }
    } 
    DT::datatable(out, options = list(scrollX = TRUE)
    )
  })
  
  # Adjudicator submissions
  # Observe submission of cause of death and save
  observeEvent(input$adj_submit_cod, {
    cn <- input$country
    cod_names <- cod_data(country = cn)
    cod_data <- data$cod
    pn <- input$adj_phy_notes
    cod_3 = input$adj_cods
    # condition if underlying cause of death is not fiilled out, wont submit
    if(cod_3==''){
      adj_submission_success(FALSE)
    } else {
      death_id = input$adj_death_id
      # save(cod_data,death_id, pn, cod_1, file = 'adj_temp.rda' )
      
      cod_data$cod_3 = paste0(unlist(lapply(strsplit(input$adj_cods,split = ' '), function(x) x[-1])), collapse = ' ')
      cod_data$death_id = death_id
      cod_data$time_stamp <- Sys.time()
      cod_data$doctor_note <- pn
      
      cod_data$cod_code_3 <- cod_names$cod_code[cod_names$cod_names==cod_data$cod_3][1]
      dbAppendTable(conn = con, name = 'vatool_cods', value = cod_data)
      adj_submission_success(TRUE)
      
      # also remove the now adjudicated ID from the choices
      old_choices <- va_choices$adjudicator_choices
      new_choices <- old_choices[!old_choices %in% death_id]
      va_choices$adjudicator_choices <- new_choices
      
      # add to cods table 
      old_cods <- table_cods$data
      new_cods <- rbind(old_cods, cod_data)
      table_cods$data <- new_cods
    }
    
  })
  
  # Observe changes in inputs
  observeEvent(c(input$adj_cod,input$adj_death_id), {
    adj_submission_success(NULL)
  })
  
  output$ui_submission_adj <- renderUI({
    ss <- adj_submission_success()
    if(is.null(ss)){
      NULL
    } else if(ss){
      h3('Submission successful')
    } else {
      h3('Submission unsuccessful')
    }
  })
  
  
  # Unresolved VAs
  observeEvent(input$adj_two_submit_cod, {
    cn <- input$country
    cod_names <- cod_data(country = cn)
    cod_data <- data$cod
    pn <- input$adj_two_phy_notes
    cod_3 = input$adj_two_cods
    # condition if underlying cause of death is not fiilled out, wont submit
    if(cod_3==''){
      adj_two_submission_success(FALSE)
    } else {
      death_id = input$adj_two_death_id
      cod_data$cod_3 = paste0(unlist(lapply(strsplit(input$adj_two_cods,split = ' '), function(x) x[-1])), collapse = ' ')
      cod_data$death_id = death_id
      cod_data$time_stamp <- Sys.time()
      cod_data$doctor_note <- pn
      
      cod_data$cod_code_3 <- cod_names$cod_code[cod_names$cod_names==cod_data$cod_3][1]
      dbAppendTable(conn = con, name = 'vatool_cods', value = cod_data)
      adj_two_submission_success(TRUE)
      
      # also remove the now adjudicated ID from the choices
      old_choices <- va_choices$adjudicator_two_choices
      new_choices <- old_choices[!old_choices %in% death_id]
      va_choices$adjudicator_two_choices <- new_choices
      
      # add to cods table 
      old_cods <- table_cods$data
      new_cods <- rbind(old_cods, cod_data)
      table_cods$data <- new_cods
    }
    
  })
  
  # Observe changes in inputs
  observeEvent(c(input$adj_two_cod,input$adj_two_death_id), {
    adj_two_submission_success(NULL)
  })
  
  output$ui_two_submission_adj <- renderUI({
    ss <- adj_two_submission_success()
    if(is.null(ss)){
      NULL
    } else if(ss){
      h3('Submission successful')
    } else {
      h3('Submission unsuccessful')
    }
  })
  session$onSessionEnded(function(){
    cat(paste0('Session ended.'))
    if(exists('con')){
      dbDisconnect(con)
      cat(paste0('Disconnected from database.'))
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
  addResourcePath(
    'www', system.file('app/www', package = 'vatool')
  )
  
  
  share <- list(
    title = "Bohemia VA Tool",
    url = "https://bohemia.team/va/",
    image = "https://www.databrew.cc/images/logo_clear.png",
    description = "Bohemia app",
    twitter_user = "data_brew"
  )
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}
