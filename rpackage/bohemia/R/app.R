##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2
#' @import gt
#' @import sp
#' @import DT
#' @import mapview
#' @import stplanr
#' @import lubridate
#' @import RPostgres
#' @import yaml
#' @import leafgl
#' @import stplanr
#' @import sf
#' @import shinybusy
#' @import ggrepel
#' @import osmextract

app_ui <- function(request) {
  options(scipen = '999')
  tagList(
    mobile_golem_add_external_resources(),
    
    dashboardPage(
      dashboardHeader(#title = tags$a(tags$img(src='www/logo.png',height='32',width='36', alt = 'BohemiApp')),
        tags$li(class = 'dropdown',
                tags$style(type='text/css', "#log_ui {margin-right: 10px; margin-left: 10px; font-size:80%; margin-top: 10px; margin-bottom: -12px;}"),
                tags$li(class = 'dropdown',
                        radioButtons('geo', ' ',
                                     choices = c('Tanzania' = 'Rufiji',
                                                 'Mozambique' = 'Mopeia'
                                     ),
                                     inline = TRUE,
                                     selected = 'Rufiji')),
                tags$li(class = 'dropdown',
                        uiOutput('log_ui')))),
      dashboardSidebar(
        sidebarMenu(
          id = 'sidebar',
          menuItem(
            text="Main",
            tabName="main",
            icon=icon("archway")),
          menuItem(
            text = 'Operations',
            tabName = 'operations',
            icon = icon('laptop-code'),
            startExpanded = FALSE,
            menuSubItem(
              text="Field monitoring",
              tabName="field_monitoring",
              icon=icon("clipboard")),
            menuSubItem(
              text="Enrollment",
              tabName="enrollment",
              icon=icon("database")),  
            menuSubItem(
              text="Server status",
              tabName="server_status",
              icon=icon("server")),
            menuSubItem(
              text="Cross-site comparison",
              tabName="cross_site",
              icon=icon("globe-africa")),
            menuSubItem(
              text="Geographical tables",
              tabName="geo_tables",
              icon=icon("globe"))),
          menuItem('Research',
                   tabName = 'research',
                   icon = icon('microscope'),
                   startExpanded = FALSE,
                   menuSubItem(text = 'Sneak peek',
                               tabName = 'sneak_peek',
                               icon = icon('glasses')),
                   menuSubItem(
                     text="Demography",
                     tabName="demography",
                     icon=icon("users")),
                   menuSubItem(
                     text="Socioeconomics",
                     tabName="socioeconomics",
                     icon=icon("motorcycle")),
                   menuSubItem(
                     text="Veterinary",
                     tabName="veterinary",
                     icon=icon("piggy-bank")),
                   menuSubItem(
                     text="Environment",
                     tabName="environment",
                     icon=icon("tree")),
                   menuSubItem(
                     text="Health",
                     tabName="health",
                     icon=icon("briefcase-medical")),
                   menuSubItem(
                     text="Malaria",
                     tabName="malaria",
                     icon=icon("procedures"))),
          menuItem('Tracking tools',
                   tabName = 'tracking',
                   icon = icon('list'),
                   startExpanded = FALSE,
                   menuSubItem(
                     text = 'Sheets',
                     tabName = 'tracking_tools',
                     icon = icon('list')
                   ),
                   menuSubItem(
                     text = 'Mark as done',
                     tabName = 'done_hamlets',
                     icon = icon('list'))),
          menuItem('Alerts',
                   tabName = 'alerts',
                   icon = icon('exclamation-circle')),
          menuItem('Download data',
                   tabName = 'download_data',
                   icon = icon('download')),
          menuItem(
            text = 'About',
            tabName = 'about',
            icon = icon("cog", lib = "glyphicon"))
        )),
      dashboardBody(
        add_busy_spinner(
          spin = "folding-cube",#  "self-building-square",
          position = 'bottom-right',
          onstart = TRUE,
          height = '100px',
          width = '100px',
          margins = c(10, 10)#,
          # color = 'de0000'
        ),
        
        # tags$head(
        #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        # ),
        
        tabItems(
          tabItem(
            tabName="main",
            fluidPage(
              fluidRow(column(12, align = 'center',
                              h3('Bohemia Minicensus Dashboard'))),
              fluidRow(column(9,
                              plotOutput('landing_plot_1'),
                              plotOutput('landing_plot_2')),
                       column(3,
                              uiOutput('landing_box_mini'),
                              
                              
                              # br(),
                              # div(tableOutput('landing_box_va'),style='font-size:30px'),
                              # br(),
                              # div(tableOutput('landing_box_error'),style='font-size:30px'),
                              # br(),
                              # div(tableOutput('landing_box_anom'),style='font-size:30px')
                              
                       )
              )
            )
          ),
          tabItem(
            tabName = 'field_monitoring',
            tabsetPanel(tabPanel('Overview',
                                 navbarPage(
                                   title = 'Overview',
                                   tabPanel('Progress by geographical unit',
                                            uiOutput('ui_progress_by_geographical_unit')),
                                   tabPanel('Overall progress',
                                            uiOutput('ui_overall_progress'))
                                 )),
                        tabPanel('Performance',
                                 fluidPage(
                                   navbarPage(title = 'Performance',
                                              tabPanel('Fieldworkers',
                                                       tabsetPanel(
                                                         tabPanel('Individual data',
                                                                  fluidPage(
                                                                    uiOutput('ui_individual_data'),
                                                                    uiOutput('ui_individual_out')
                                                                  )),
                                                         tabPanel('Aggregate data',
                                                                  fluidPage(
                                                                    uiOutput('ui_fw_time_period'),
                                                                    uiOutput('ui_fw_daily')
                                                                  )),
                                                         tabPanel('Dropouts',
                                                                  fluidRow(
                                                                    br() # drop out ui here
                                                                  ))
                                                       )),
                                              tabPanel('Supervisors',
                                                       fluidPage(
                                                         fluidRow(
                                                           column(12, align = 'center',
                                                                  h3('By supervisor'),
                                                                  p('PENDING: need standardized supervisor information from both sites for all fieldworkers.'))
                                                         )
                                                       ))
                                   )
                                 )),
                        tabPanel('VA',
                                 fluidPage(
                                   # br(), br(),
                                   navbarPage(title = 'VA',
                                              tabPanel('VA progress',
                                                       tabsetPanel(
                                                         tabPanel('VA Overall progress',
                                                                  uiOutput('ui_va_overall_progress')),
                                                         tabPanel('VA progress by geographical unit',
                                                                  fluidPage(
                                                                    uiOutput('ui_va_monitoring_by'),
                                                                    br(),
                                                                    uiOutput('va_progress_geo_ui'),
                                                                    uiOutput('va_progress_geo_render_plot_ui'),
                                                                    uiOutput('va_progress_geo_past_due_ui'))),
                                                         tabPanel('Past due VAs',
                                                                  uiOutput('ui_va_progress_past_due')
                                                         )
                                                         
                                                       ))
                                   )
                                 )
                                 
                        ),
                        tabPanel('GPS tracking',
                                 
                                 tabsetPanel(
                                   tabPanel('Individual',
                                            uiOutput('ui_gps')),
                                   tabPanel('Aggregate',
                                            fluidRow(column(12, align = 'center',
                                                            h3('Most recent locations'),
                                                            leafletOutput('most_recent_locations'),
                                                            h3('Tabular data on period'),
                                                            DT::dataTableOutput('traccar_table')))
                                   ))
                        ),
                        tabPanel('Refusals and Absences',
                                 uiOutput('ui_geo_r_and_a'),
                                 uiOutput('ui_date_r_and_a'),
                                 uiOutput('ui_refusals_and_absences')))),
          tabItem(
            tabName="enrollment",
            navbarPage(
              title = 'Enrollment data',
              tabPanel('Aggregated',
                       uiOutput('ui_enrollment'))
            )),
          tabItem(
            tabName = 'cross_site',
            uiOutput('ui_cross_site')
          ),
          tabItem(
            tabName="server_status",
            uiOutput('ui_server_status_date'),
            uiOutput('ui_server_status'),
            checkboxInput('only_pending', 'Show only pending',
                          value = FALSE)),
          tabItem(
            tabName="geo_tables",
            uiOutput('ui_geo_tables')),
          tabItem(
            tabName = 'done_hamlets',
            fluidPage(
              fluidRow(
                uiOutput('ui_done_hamlets_choices')
              ),
              fluidRow(
                column(6,
                       uiOutput('ui_done_hamlets')),
                column(6,
                       uiOutput('ui_done_hamlets2'))
              )
            )
          ),
          tabItem(
            tabName="tracking_tools",
            fluidPage(
              fluidRow(
                column(3,
                       uiOutput('all_locations_ui')),
                column(9, align = 'center',
                       navbarPage(title = 'Sheets',
                                  tabPanel("Visit control sheet", 
                                           fluidPage(
                                             h3("Visit control sheet"),
                                             uiOutput('ui_enumeration_n_hh'),
                                             helpText('The default numbers shown are 25% higher than the number estimated by the village leader.'),
                                             textInput('enumeration_n_teams',
                                                       'Number of teams',
                                                       value = 2),
                                             radioButtons('enumeration_or_minicensus',
                                                          'Type',
                                                          choices = c('Enumeration visit',
                                                                      'Data collection visit')),
                                             helpText('In the case of Mozambique, "Data collection visit" means that the visit control sheet will be populated based only on previously enumerated households from the hamlet (thereby ignoring the estimated number of forms or ID limitations inputs).'),
                                             
                                             uiOutput('ui_id_limit'),
                                             br(), 
                                             downloadButton("download_visit_control_data", "Download spreadsheet of visit control data"),
                                             br(),
                                             downloadButton('render_visit_control_sheet',
                                                            'Generate visit control sheet(s)')
                                           )),
                                  tabPanel("File index and folder location",
                                           fluidPage(
                                             h3("File index and folder location"),
                                             # uiOutput('ui_id_limit_file'),
                                             br(), br(),
                                             downloadButton('render_file_index_list',
                                                            'Generate file index and folder location list(s)')
                                           )),
                                  tabPanel('Consent verification list',
                                           fluidPage(
                                             fluidRow(
                                               column(12, 
                                                      uiOutput('ui_verification_text_filter'))
                                             ),
                                             uiOutput('ui_consent_verification_list_a'),
                                             uiOutput('ui_consent_verification_list')
                                           )),
                                  tabPanel('VA control sheet',
                                           # textInput('va_n_teams', 'Number of teams', value = "1"),
                                           uiOutput('ui_va_list_generation'))
                       ))
              )
            )),
          tabItem(
            tabName="sneak_peek",
            fluidPage(
              fluidRow(
                column(4,
                       selectInput('indicator_time', 'Choose variable', choices = c('Household size','Number of children', 'Ratio of children to adults', 'Number of cattle per household', 'Number of pigs per household', 'Number of mosquito nets per household'))),
              ),
              fluidRow(
                column(6, 
                       plotOutput('average_time'))
              )
            )
          ),
          tabItem(
            tabName="demography",
            uiOutput('ui_demography')),
          tabItem(
            tabName="socioeconomics",
            uiOutput('ui_socioeconomics')),
          tabItem(
            tabName="veterinary",
            uiOutput('ui_veterinary')),
          tabItem(
            tabName="environment",
            uiOutput('ui_environment')),
          tabItem(
            tabName="health",
            uiOutput('ui_health')),
          tabItem(
            tabName="malaria",
            uiOutput('ui_malaria')),
          tabItem(
            tabName = 'download_data',
            uiOutput('download_ui')
          ),
          tabItem(
            tabName = 'alerts',
            navbarPage(
              title = 'Errors & anomalies',
              tabPanel('Alerts table',
                       fluidPage(
                         column(12, align = 'center',
                                tags$img(src = "www/alerts_legend.png", style="width: 600px"),
                                # add_busy_gif(src = "https://jeroen.github.io/images/banana.gif", height = 70, width = 70),
                                uiOutput('anomalies_ui_a'),
                                uiOutput('anomalies_ui')
                         ))),
              tabPanel('Bulk uploader',
                       fluidPage(
                         fluidRow(
                           p('Upload a .csv file of your corrections with at least the following 5 columns: "Instance id", "Id", "Response details", "Resolved by", "Resolution method"')
                         ),
                         
                         fluidRow(
                           column(6,
                                  fileInput("upload_data", "Choose CSV File",
                                            accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")
                                  ))
                         ),
                         fluidRow(
                           column(12,
                                  tableOutput("upload_data_message"))
                         )
                       ))
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
    'www', system.file('app/www', package = 'bohemia')
  )
  
  
  share <- list(
    title = "Bohemia app",
    url = "https://bohemia.team/app/",
    image = "https://www.databrew.cc/images/logo_clear.png",
    description = "Bohemia app",
    twitter_user = "data_brew"
  )
  
  tags$head(
    
    # Facebook OpenGraph tags
    tags$meta(property = "og:title", content = share$title),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:url", content = share$url),
    tags$meta(property = "og:image", content = share$image),
    tags$meta(property = "og:description", content = share$description),
    
    # Twitter summary cards
    tags$meta(name = "twitter:card", content = "summary"),
    tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:title", content = share$title),
    tags$meta(name = "twitter:description", content = share$description),
    tags$meta(name = "twitter:image", content = share$image),
    # 
    golem::activate_js(),
    golem::favicon(),
    # # Add here all the external resources
    # # Google analytics script
    # includeHTML(system.file('app/www/google-analytics-mini.html', package = 'covid19')),
    # includeScript(system.file('app/www/script.js', package = 'covid19')),
    # includeScript(system.file('app/www/dtselect.js', package = 'saint')),
    # includeScript('inst/app/www/script.js'),
    
    # includeScript('www/google-analytics.js'),
    # If you have a custom.css in the inst/app/www
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

##################################################
# SERVER
##################################################
#' @import shiny
#' @import leaflet
#' @import yaml
#' @import lubridate
#' @import gt
#' @import DT
app_server <- function(input, output, session) {
  sizex <- 24
  
  # Define whether local or not
  if(grepl('brew', getwd())){
    is_local <- TRUE
    message('Using local database')
  } else {
    is_local <- FALSE
    message('Using remote database')
  }
  # is_local <- FALSE
  
  # Connect to database
  message('Connecting to database : ', ifelse(is_local, ' local', 'remote'))
  con <- get_db_connection(local = is_local)
  
  
  
  message('Working directory is :', getwd())
  
  # is_local <- FALSE
  use_cached <- TRUE
  
  # Define a default fieldworkers data
  if(!'fids.csv' %in% dir('/tmp')){
    fids_url <- 'https://docs.google.com/spreadsheets/d/1o1DGtCUrlBZcu-iLW-reWuB3PC8poEFGYxHfIZXNk1Q/edit#gid=2059603661'
    fids1 <- gsheet::gsheet2tbl(fids_url) %>% dplyr::select(bohemia_id, first_name, last_name, supervisor, Role = details) %>% dplyr::mutate(country = 'Tanzania')
    fids_url <- 'https://docs.google.com/spreadsheets/d/1o1DGtCUrlBZcu-iLW-reWuB3PC8poEFGYxHfIZXNk1Q/edit#gid=409816186'
    fids2 <- gsheet::gsheet2tbl(fids_url) %>% dplyr::select(bohemia_id, first_name, last_name, supervisor, Role = details) %>% dplyr::mutate(country = 'Mozambique')
    fids_url <- 'https://docs.google.com/spreadsheets/d/1o1DGtCUrlBZcu-iLW-reWuB3PC8poEFGYxHfIZXNk1Q/edit#gid=179257508'
    fids3 <- gsheet::gsheet2tbl(fids_url) %>% dplyr::select(bohemia_id, first_name, last_name, supervisor, Role = details) %>% dplyr::mutate(country = 'Catalonia')
    fids <- bind_rows(fids1, fids2, fids3)
    readr::write_csv(fids, '/tmp/fids.csv')
  } else {
    fids <- readr::read_csv('/tmp/fids.csv')
  }
  
  default_fieldworkers <- fids %>%
    dplyr::rename(id = bohemia_id) %>%
    mutate(name = paste0(first_name, ' ', last_name)) %>%
    dplyr::select(id, name)
  
  
  ###########################################################################
  # REACTIVE OBJECTS
  ###########################################################################
  # Reactive object for seeing if logged in or not
  # (Joe will build log-in functionality later
  session_info <- reactiveValues(logged_in =FALSE, 
                                 user = 'default',
                                 access = c("field_monitoring", "enrollment", 'consent_verification_list', "server_status", "demography", "socioeconomics", "veterinary", "environment", "health", "malaria"),
                                 country = 'MOZ',
                                 already_loaded_errors = FALSE)
  
  # Create some reactive data
  session_data <- reactiveValues(aggregate_table = data.frame(),
                                 anomalies = data.frame(),
                                 fieldworkers = default_fieldworkers,
                                 traccar = data.frame(),
                                 traccar_summary = data.frame(),
                                 va_table = data.frame(),
                                 visit_control_data = data.frame())
  # create reactive object to store odk_data.
  odk_data <- reactiveValues(data = NULL)
  
  # estimated households data
  estimated_households <- reactiveValues(data = bohemia::gps %>% mutate(n_households = ifelse(iso == 'MOZ',
                                                                                              round(n_households * 0.55),
                                                                                              n_households)))
  
  
  
  
  # Text for incorrect log-in, etc.
  reactive_log_in_text <- reactiveVal(value = '')
  
  # Observe the corner log-in / log-out buttons
  observeEvent(input$log_in_button, {
    # See if there was an incorrect user/password combo
    info_text <- reactive_log_in_text()
    make_log_in_modal(info_text = info_text)
  })
  
  country <- reactiveVal(value = 'Tanzania')
  
  
  
  
  observeEvent(input$confirm_log_in,{
    # Run a check on the credentials
    users <- yaml::yaml.load_file('credentials/users.yaml')
    liu <- input$log_in_user
    lip <- input$log_in_password
    ok <- credentials_check(user = liu,
                            password = lip,
                            users = users)
    gg <- input$geo
    if(gg == 'Rufiji'){
      the_country <- 'Tanzania'
    } else {
      the_country <- 'Mozambique'
    }
    country(the_country)
    if(ok){
      message('---Correct user/password. Logged in.')
      # Update sessions table
      sesh <- tibble(user_email = liu,
                     success = TRUE,
                     start_time = Sys.time(),
                     end_time = NA,
                     web = grepl('/srv/shiny-server', getwd(), fixed = TRUE))
      
      
      # con <- get_db_connection(local = is_local)
      message('Writing session info to sessions table.')
      dbAppendTable(conn = con,
                    name = 'sessions',
                    value = sesh)
      
      # # Read in the traccar data (could speed this up by not reading all in)
      # message('Reading in traccar table')
      # # Get the country
      # co <- the_country
      # lat_string <- ifelse(co == 'Mozambique',
      #                      'latitude < -15',
      #                      'latitude > -9')
      # # Get fieldworkers for this country
      # these_fids <- fids %>% filter(country == co)
      # keep_ids <- these_fids$bohemia_id
      # these_fids <- paste0("(",paste0("'",these_fids$bohemia_id,"'", collapse=","),")")
      # # Get traccar data for those fieldworkers
      # device_time <- paste0(Sys.Date() - 7, ' 01:01:01')
      # traccar <- dbGetQuery(conn = con,
      #                       statement = paste0('SELECT * FROM traccar WHERE (unique_id IN ', these_fids, " AND devicetime > '", device_time, "' AND ", lat_string, ")"))
      # session_data$traccar <- traccar
      # # dbDisconnect(con)
      
      
      session_info$logged_in <- TRUE
      reactive_log_in_text('')
      removeModal()
    } else {
      message('---Incorrect user/password. Not logged in.')
      # Update the sessions table
      sesh <- tibble(user_email = liu,
                     success = FALSE,
                     start_time = Sys.time(),
                     end_time = NA,
                     web = grepl('ubuntu', getwd()))
      # con <- get_db_connection(local = is_local)
      dbAppendTable(conn = con,
                    name = 'sessions',
                    value = sesh)
      # dbDisconnect(con)
      session_info$logged_in <- TRUE
      reactive_log_in_text('')
      removeModal()
      session_info$logged_in <- FALSE
      removeModal()
      reactive_log_in_text(span('Incorrect user/password combo. Please try again.', style="color:red"))
      info_text <- reactive_log_in_text()
      make_log_in_modal(info_text = info_text)
    }
  })
  observeEvent(input$log_out_button, {
    session_info$logged_in <- FALSE
    removeModal()
    dbDisconnect(con)
  })
  
  ##################
  # Estimated number of households
  ##################
  listen_estimated_hh <- reactive({
    list(
      odk_data$data,
      input$mark_done,
      input$mark_undone
    )
    
  })
  observeEvent(listen_estimated_hh(), {
    li <- session_info$logged_in
    if(li){
      # Update the in-session estimated_households$data table to make sure values are right
      # message('Overwriting GPS!')
      co <- country()
      if(co == 'Mozambique'){
        pd <- odk_data$data$enumerations
        estimated_hh_per_collection <- pd %>%
          group_by(code = hamlet_code) %>%
          summarise(done_hh = length(unique(agregado)))
      } else {
        pd <- odk_data$data$minicensus_main
        estimated_hh_per_collection <- pd %>%
          group_by(code = hh_hamlet_code) %>%
          summarise(done_hh = length(unique(hh_id)))
      }
      # Join with the estimated_households$data object
      left <- estimated_households$data
      
      # filter to only keep those which are already marked as done
      out <- already_done_hamlets()
      
      # save(pd, estimated_hh_per_collection, co, li, out, left,
      #      file = '/tmp/joe2020.RData')
      
      # Fill in any empties
      if(length(out) > 0){
        for(i in 1:length(out)){
          if(!out[i] %in% estimated_hh_per_collection$code){
            message('Marking ', out[i], ' as done despite having no forms collected...')
            new_row <- tibble(code = out[i],
                              done_hh = 0)
            estimated_hh_per_collection <-
              bind_rows(estimated_hh_per_collection,
                        new_row)
          }
        }
      }
      
      
      estimated_hh_per_collection <- estimated_hh_per_collection %>%
        filter(code %in% out)
      if(nrow(estimated_hh_per_collection) > 0){
        joined <- left_join(left, estimated_hh_per_collection) %>%
          mutate(n_households = ifelse(is.na(done_hh),
                                       n_households,
                                       done_hh)) %>%
          dplyr::select(-done_hh)
        estimated_households$data <- joined
      } 
    }
  })
  
  
  ##################
  # LOCATION HIERARCHY UTILITIES
  ##################
  # Get the location code based on the input hierarchy
  location_code <- reactiveVal(value = NULL)
  hq <- reactiveValues(data = tibble(lng = 35.711553,
                                     lat = -17.979446,
                                     label = 'Mopeia Sede'))
  observeEvent(input$geo, {
    gg <- input$geo
    if(gg == 'Rufiji'){
      the_country <- 'Tanzania'
    } else {
      the_country <- 'Mozambique'
    }
    country(the_country)
    
    # Update HQ location
    if(the_country == 'Mozambique'){
      hq$data <- tibble(lng = 35.711553,
                        lat = -17.979446,
                        label = 'Mopeia Sede')
    } else {
      hq$data <- tibble(lng = 38.984170,
                        lat = -7.947843,
                        label = 'Rufiji Ikwiri')
    }
    
    # Load data     
    li <- session_info$logged_in
    if(li){
      out <- load_odk_data(local = is_local, the_country = the_country, efficient = TRUE, use_cached = use_cached, con = con)
      odk_data$data <- out
      
      # Get anomalies
      # con <- get_db_connection(local = is_local)
      anomalies <- dbGetQuery(conn = con,
                              statement = paste0("SELECT * FROM anomalies WHERE country = '", the_country, "'"))
      session_data$anomalies <- anomalies
      
      # PAUSING TRACCAR STUFF, TOO SLOW, OPTIMIZE LATER
      # Read in the traccar data (could speed this up by not reading all in)
      # message('Reading in traccar table')
      # # Get the country
      # the_country <- country()
      # co <- the_country
      # lat_string <- ifelse(co == 'Mozambique',
      #                      'latitude < -15',
      #                      'latitude > -9')
      # # Get fieldworkers for this country
      # these_fids <- fids %>% filter(country == co)
      # keep_ids <- these_fids$bohemia_id
      # these_fids <- paste0("(",paste0("'",these_fids$bohemia_id,"'", collapse=","),")")
      # # Get traccar data for those fieldworkers
      # device_time <- paste0(Sys.Date() - 7, ' 01:01:01')
      # traccar <- dbGetQuery(conn = con,
      #                       statement = paste0('SELECT * FROM traccar WHERE (unique_id IN ', these_fids, " AND devicetime > '", device_time, "' AND ", lat_string, ")"))
      # session_data$traccar <- traccar
      # # Get traccar summary data
      # message('Retrieving information on workers from traccar')
      # creds <- yaml::yaml.load_file('credentials/credentials.yaml')
      # # dat <- get_traccar_data(url = creds$traccar_server,
      # #                         user = creds$traccar_user,
      # #                         pass = creds$traccar_pass)
      # # 
      # dbDisconnect(con)
      
    }
  })
  
  # Observe log in and load data
  observeEvent(session_info$logged_in, {
    the_country <- country()
    li <- session_info$logged_in
    if(li){
      
      
      
      message('Logged in. Loading data for ', the_country)
      out <- load_odk_data(local = is_local, the_country = the_country, efficient = TRUE, use_cached = use_cached, con = con)
      
      # Get anomalies
      # con <- get_db_connection(local = is_local)
      anomalies <- dbGetQuery(conn = con,
                              statement = paste0("SELECT * FROM anomalies WHERE country = '", the_country, "'"))
      session_data$anomalies <- anomalies
      
      # # PAUSING TRACCAR STUFF, TOO SLOW, OPTIMIZE LATER
      # # Read in the traccar data (could speed this up by not reading all in)
      # message('Reading in traccar table')
      start_time <- Sys.time()
      # # Get the country
      # the_country <- country()
      # co <- the_country
      # lat_string <- ifelse(co == 'Mozambique',
      #                      'latitude < -15',
      #                      'latitude > -9')
      # # Get fieldworkers for this country
      # these_fids <- fids %>% filter(country == co)
      # keep_ids <- these_fids$bohemia_id
      # these_fids <- paste0("(",paste0("'",these_fids$bohemia_id,"'", collapse=","),")")
      # # Get traccar data for those fieldworkers
      # device_time <- paste0(Sys.Date() - 7, ' 01:01:01')
      # traccar <- dbGetQuery(conn = con,
      #                       statement = paste0('SELECT * FROM traccar WHERE (unique_id IN ', these_fids, " AND devicetime > '", device_time, "' AND ", lat_string, ")"))
      # session_data$traccar <- traccar
      # dbDisconnect(con)
      end_time <- Sys.time()
      message('..That took ', round(lubridate::time_length(end_time - start_time, unit = 'second'), 2), ' seconds.')      
      odk_data$data <- out
      # if(grepl('joebrew', getwd())){
      #   save(out, file = '~/Desktop/odk_data_data.RData')
      # }
    }
  })
  
  
  # Error and anomaly detection
  observeEvent(input$sidebar, {
    sidebar <- input$sidebar
    message('Clicked on sidebar: ', sidebar)
    li <- session_info$logged_in
    if(!li){
      info_text <- reactive_log_in_text()
      make_log_in_modal(info_text = info_text)
    }
  })
  output$all_locations_ui <- renderUI({
    fluidRow(
      column(12, 
             # radioButtons('country', 'Country', choices = c('Tanzania', 'Mozambique'), inline = TRUE, selected = 'Mozambique'), 
             uiOutput('region_ui'),
             uiOutput('district_ui'),
             uiOutput('ward_ui'),
             uiOutput('village_ui'),
             uiOutput('hamlet_ui'),
             h4('Location code:'),
             h3(textOutput('location_code_text')))
    )
  })
  
  
  observeEvent(c(country(),
                 input$region,
                 input$district,
                 input$ward,
                 input$village,
                 input$hamlet), {
                   country = country()
                   region = input$region
                   district = input$district
                   ward = input$ward
                   village = input$village
                   hamlet = input$hamlet
                   
                   glc <- get_location_code(country = country,
                                            region = region,
                                            district = district,
                                            ward = ward,
                                            village = village,
                                            hamlet = hamlet,
                                            allow_all = TRUE)
                   
                   location_code(glc)
                 })
  output$location_code_text <- renderText({
    lc <- location_code()
    if(!is.null(lc)){
      if(length(lc) > 1){
        lc <- paste0(sort(lc), collapse = ', ')
      }
    }
    lc
  })
  
  output$region_ui <- renderUI({
    sub_locations <- filter_locations(locations = locations,
                                      country = country())
    choices <- sort(unique(sub_locations$Region))
    choices <- c(choices, 'All')
    country_name <- country()
    if(country_name == 'Mozambique'){
      selectInput('region', 'Região', choices = choices)
    } else {
      selectInput('region', 'Region', choices = choices)
    } 
  })
  
  output$district_ui <- renderUI({
    selected <- input$region
    ok <- FALSE
    if(!is.null(selected)){
      if(selected != 'All'){
        ok <- TRUE
      }
    }
    if(ok){
      sub_locations <- filter_locations(locations = locations,
                                        country = country(),
                                        region = input$region)
      choices <- sort(unique(sub_locations$District))
      choices <- c(choices, 'All')
      country_name <- country()
      if(country_name == 'Mozambique'){
        selectInput('district', 'Distrito', choices = choices)
      } else {
        selectInput('district', 'District', choices = choices)
      }
    } else {
      NULL
    }
  })
  
  output$ward_ui <- renderUI({
    selected <- input$district
    ok <- FALSE
    if(!is.null(selected)){
      if(selected != 'All'){
        selected <- input$region
        if(!is.null(selected)){
          if(selected != 'All'){
            ok <- TRUE
          }
        }
      }
    }
    if(ok){
      sub_locations <- filter_locations(locations = locations,
                                        country = country(),
                                        region = input$region,
                                        district = input$district)
      choices <- sort(unique(sub_locations$Ward))
      choices <- c(choices, 'All')
      country_name <- country()
      if(country_name == 'Mozambique'){
        selectInput('ward', 'Posto administrativo/localidade', choices = choices)
      } else {
        selectInput('ward', 'Ward', choices = choices)
      } 
    } else {
      NULL
    }
  })
  
  output$village_ui <- renderUI({
    selected <- input$ward
    ok <- FALSE
    if(!is.null(selected)){
      if(selected != 'All'){
        selected <- input$district
        if(!is.null(selected)){
          if(selected != 'All'){
            selected <- input$region
            if(!is.null(selected)){
              if(selected != 'All'){
                ok <- TRUE
              }
            }
          }
        }
      }
    }
    if(ok){
      sub_locations <- filter_locations(locations = locations,
                                        country = country(),
                                        region = input$region,
                                        district = input$district,
                                        ward = input$ward)
      choices <- sort(unique(sub_locations$Village))
      choices <- c(choices, 'All')
      country_name <- country()
      if(country_name == 'Mozambique'){
        selectInput('village', 'Povoado', choices = choices)
      } else {
        selectInput('village', 'Village', choices = choices)
      }
    } else {
      NULL
    }
  })
  
  output$hamlet_ui <- renderUI({
    selected <- input$village
    ok <- FALSE
    if(!is.null(selected)){
      if(selected != 'All'){
        selected <- input$ward
        if(!is.null(selected)){
          if(selected != 'All'){
            selected <- input$district
            if(!is.null(selected)){
              if(selected != 'All'){
                selected <- input$region
                if(!is.null(selected)){
                  if(selected != 'All'){
                    ok <- TRUE
                  }
                }
              }
            }
          }
        }
      }
    }
    if(ok){
      sub_locations <- filter_locations(locations = locations,
                                        country = country(),
                                        region = input$region,
                                        district = input$district,
                                        ward = input$ward,
                                        village = input$village)
      choices <- sort(unique(sub_locations$Hamlet))
      choices <- c(choices, 'All')
      country_name <- country()
      if(country_name == 'Mozambique'){
        selectInput('hamlet', 'Bairro', choices = choices)
      } else {
        selectInput('hamlet', 'Hamlet', choices = choices)
      } 
    } else {
      NULL
    }
  })
  
  # Get number of households for hamlet selected
  hamlet_num_hh <- reactive({
    ok <- FALSE
    # hamlet_name <- input$hamlet
    lc <- location_code()
    if(!is.null(lc)){
      ok <- TRUE
    }
    if(ok){
      num_houses <- estimated_households$data %>% filter(code %in% lc) %>% .$n_households
      num_houses <- round(sum(num_houses) * 1.25)
    } else {
      num_houses <- 500
    }
    message('---- retrieved number of houses for hamlet selected')
    return(num_houses)
  })
  
  output$ui_enumeration_n_hh <- renderUI({
    val <- hamlet_num_hh()
    textInput('enumeration_n_hh',
              'Estimated number of forms',
              value = val)
    # message('---- created enumeration text input for estimated numer of forms')
    
  })
  
  output$ui_id_limit <- renderUI({
    val <- hamlet_num_hh()
    lc <- location_code()
    if(length(lc) > 1){
      high_val <- 1000
    } else {
      high_val <- round(val * 2)
    }
    
    fluidPage(
      sliderInput('id_limit', 'Limit IDs to:',
                  min = 1,
                  max = high_val, # round(num_houses),
                  value = c(1, high_val), # c(1, num_houses),
                  step = 1),
      helpText('Normally, do not touch this slider. Adjust it only if you want to exclude certain IDs (ie, in the case of having already printed numbers 1-50, you might set the lower limit of the slider to 51).')
    )
  })
  
  # output$ui_id_limit_file <- renderUI({
  #   val <- hamlet_num_hh()
  #   fluidPage(
  #     sliderInput('id_limit', 'Limit IDs to:',
  #                 min = 1,
  #                 max = val, # round(num_houses),
  #                 value = c(1, val), # c(1, num_houses),
  #                 step = 1),
  #     helpText('Normally, do not touch this slider. Adjust it only if you want to exclude certain IDs (ie, in the case of having already printed numbers 1-50, you might set the lower limit of the slider to 51).')
  #   )
  # })
  
  
  
  ###########################################################################
  # LOG-IN
  ###########################################################################
  # observeEvent(session_info$logged_in,{
  #     li <- session_info$logged_in
  #     if(li){
  #         removeModal()
  #     }
  # })
  
  ###########################################################################
  # UIs
  ###########################################################################
  
  # # Main UI ##########################################################
  
  output$landing_plot_1 <- renderPlot({
    pd <- odk_data$data
    if(is.null(pd)){
      NULL
    } else {
      pd <- pd$minicensus_main
      plot_data <- pd %>% group_by(todays_date) %>% summarise(num_forms = n()) %>%
        mutate(cum_forms = cumsum(num_forms))
      ggplot(plot_data, aes(todays_date, cum_forms)) +
        geom_point() +
        geom_line() + 
        labs(x='Date',
             y= 'Number of forms',
             title = 'Cumulative minicensus forms') +
        theme_bohemia()
    }
    
  })
  
  output$landing_plot_2 <- renderPlot({
    
    an <- session_data$anomalies
    
    if(is.null(an) | nrow(an)==0){
      NULL
    } else {
      plot_data <- an %>% group_by(date) %>% summarise(sum_error = sum(type == 'error', na.rm = TRUE),
                                                       sum_anomaly = sum(type == 'anomaly', na.rm = TRUE)) %>%
        gather(key = key, value=value, -date)
      ggplot(plot_data %>% filter(date >= '2020-08-01'), 
             aes(date, value, fill = key)) +
        geom_bar(position='dodge', stat='identity', alpha = 0.75) +
        scale_fill_manual(name = '',
                          label = c('# of Anomalies', '# of Errors'),
                          values = c('black', 'grey')) +
        labs(x='Date',
             y= '#',
             title = 'Errors and Anomalies') +
        theme_bohemia() +
        theme(legend.position="bottom")
    }
    
  })
  
  
  
  output$landing_box_mini <- renderUI({
    pd <- odk_data$data
    an <- session_data$anomalies
    if(is.null(pd) | is.null(an) | nrow(an)==0){
      NULL
    } else {
      # save(an, file='temp_an.rda')
      # look at corrections and fixes
      corrections <- pd$corrections
      va <- pd$va
      death <- pd$minicensus_repeat_death_info
      pd <- pd$minicensus_main
      pd$day <- weekdays(pd$todays_date)
      
      # create a dataframe with data and day of week from beginning of minicensus to todays date
      date_vec <- seq.Date(from = min(pd$todays_date), to = Sys.Date(), by = 'day')
      day_vec <- weekdays(date_vec)
      all_days <- tibble(todays_date = date_vec, day = day_vec)
      
      # get the most recent sunday
      pd <- pd %>% group_by(todays_date, day) %>% summarise(counts = n()) %>% arrange(todays_date) 
      pd <- left_join(all_days, pd)
      pd$counts[is.na(pd$counts)] <- 0
      
      # get max date equal to monday
      last_sunday = max(pd$todays_date[pd$day=='Sunday'])
      pd_sub <- pd %>% filter(todays_date <= last_sunday)
      last_monday <- max(pd_sub$todays_date[pd_sub$day=='Monday'])
      pd_week <- pd %>% filter(todays_date>= last_monday, todays_date<=last_sunday)
      num_forms_since_monday <-sum(pd_week$counts, na.rm = TRUE)
      forms_complete <- sum(pd$counts)
      date_today <- Sys.Date()
      seconds_in_day <- 86400
      title_mini <- paste0('Minicensus forms finished: ', forms_complete)
      sub_title_mini <- paste0(last_monday, ' - ', last_sunday, ': ', num_forms_since_monday)
      
      # get va info
      va_num_deaths <- length(which(va$death_id %in% death$death_id))
      total_deaths <- length(unique(death$death_id))
      forms_complete <- length(unique(va$death_id))
      
      va$day <- weekdays(va$todays_date)
      va <- va %>% group_by(todays_date, day) %>% summarise(counts = n()) %>% arrange(todays_date) 
      
      va <- left_join(all_days,va) 
      # get max date equal to monday
      last_sunday = max(va$todays_date[va$day=='Sunday'])
      va_sub <- va %>% filter(todays_date <= last_sunday)
      last_monday <- max(va_sub$todays_date[va_sub$day=='Monday'])
      va_week <- va %>% filter(todays_date>= last_monday, todays_date<=last_sunday)
      num_forms_since_monday <-sum(va_week$counts, na.rm = TRUE)
      
      title_va <- paste0('VA forms finished: ', forms_complete, ' out of ', total_deaths)
      sub_title_va <- paste0(last_monday, ' - ', last_sunday, ': ', num_forms_since_monday)
      
      # get error info
      total_errors <- length(which(an$type=='error'))
      error_ids <- an$id[an$type=='error']
      error_corrected <- length(which(corrections$id  %in% error_ids))
      errors <- an %>% filter(type =='error')
      errors$day <- weekdays(errors$date)
      errors <- errors %>% group_by(date, day) %>% summarise(counts = n()) %>% arrange(date) 
      
      errors <- left_join(all_days,errors, by=c('todays_date'= 'date', 'day') ) 
      # get max date equal to monday
      last_sunday = max(errors$todays_date[errors$day=='Sunday'])
      errors_sub <- errors %>% filter(todays_date <= last_sunday)
      last_monday <- max(errors_sub$todays_date[errors_sub$day=='Monday'])
      errors_week <- errors %>% filter(todays_date>= last_monday, todays_date<=last_sunday)
      num_forms_since_monday <-sum(errors_week$counts, na.rm = TRUE)
      
      title_errors <- paste0('Errors resolved: ', error_corrected, ' out of ',total_errors)
      sub_title_errors <- paste0(last_monday, ' - ', last_sunday, ': ', num_forms_since_monday)
      # get corrections in last week
      corrections$date <- as.Date(corrections$submitted_at)
      corrections <- left_join(all_days,corrections, by=c('todays_date'= 'date')) 
      
      # get max date equal to monday
      last_sunday = max(corrections$todays_date[corrections$day=='Sunday'])
      corrections_sub <- corrections %>% filter(todays_date <= last_sunday)
      last_monday <- max(corrections_sub$todays_date[corrections_sub$day=='Monday'])
      corrections_week <- corrections %>% filter(todays_date>= last_monday, todays_date<=last_sunday)
      
      corrections_week <- corrections_week[!is.na(corrections_week$resolution_date),]
      num_correct_since_monday <-nrow(corrections_week)
      
      title_errors <- paste0('Errors resolved: ', error_corrected, ' out of ',total_errors)
      sub_title_errors <- paste0('Errors resolved week of ', last_monday, ' - ', last_sunday, ': ', num_correct_since_monday, ' out of ', num_forms_since_monday)
      
      # get anomaly info
      total_anom <- length(which(an$type=='anomaly'))
      anom_ids <- an$id[an$type=='anomaly']
      anom_corrected <- length(which(corrections$id  %in% anom_ids))
      anom <- an %>% filter(type =='anomaly')
      anom$day <- weekdays(anom$date)
      anom <- anom %>% group_by(date, day) %>% summarise(counts = n()) %>% arrange(date) 
      
      anom <- left_join(all_days,anom,by=c('todays_date'= 'date', 'day')) 
      # get max date equal to monday
      last_sunday = max(anom$todays_date[anom$day=='Sunday'])
      anom_sub <- anom %>% filter(todays_date <= last_sunday)
      last_monday <- max(anom_sub$todays_date[anom_sub$day=='Monday'])
      anom_week <- anom %>% filter(todays_date>= last_monday, todays_date<=last_sunday)
      num_forms_since_monday <-sum(anom_week$counts, na.rm = TRUE)
      sub_title_anom <- paste0(last_monday, ' - ', last_sunday, ': ', num_forms_since_monday)
      
      title_anom <- paste0('Anomalies resolved: ', anom_corrected,' out of ',total_anom)
      
      fluidPage(
        fluidRow(
          br(),
          infoBox(title = title_mini, subtitle = sub_title_mini, icon = icon("bar-chart"),
                  color = "blue", fill = TRUE, width = '100px'),
          br(), br(),br(), br(),
          infoBox(title = title_va, subtitle = sub_title_va, icon = icon("table"),
                  color = "green", fill = TRUE, width = '100px'),
          br(),br(), br(),br(), br(),
          infoBox(title = title_errors, subtitle = sub_title_errors, icon = icon("exclamation-circle"),
                  color = "red", fill = TRUE, width = '100px'),
          br(), br(),br(), br(),
          infoBox(title = title_anom, subtitle = sub_title_anom, icon = icon("exclamation-triangle"),
                  color = "yellow", fill = TRUE, width = '100px')
        )
      )
      
    }
  })
  
  # output$main_plot <- renderPlot({
  #   shp <- bohemia::mop2
  #   geo <- input$geo
  #   if(geo == 'Rufiji'){
  #     shp <- bohemia::ruf2
  #   }
  #   # if(geo == 'Both'){
  #   #   shp = rbind(ruf2, mop2)
  #   #   coords <- coordinates(shp)
  #   #   afr <- rbind(moz0, tza0)
  #   #   plot(afr, col = adjustcolor('black', alpha.f = 1), border = NA)
  #   #   plot(shp, col = 'red', add = T)
  #   #   # points(coords, col = 'red', pch = 16)
  #   #   lines(coords, col = 'red', lty =2)
  #   # } else {
  #   plot(shp, col = 'black')
  #   # }
  # })
  
  # sneak peek #############################################
  output$average_time <- renderPlot({
    
    # TEMOPRARY FUNCTION UNTIL WE FIND A DPLYR SOLUTION
    cummean_date <- function(temp_data, var_name){
      # loop through each date and get mean of variable up until that date
      names(temp_data)[which(names(temp_data)==var_name)] <- 'y'
      unique_dates <- sort(unique(temp_data$todays_date))
      loop_list <- list()
      raw_list <- list()
      for(i in 1:length(unique_dates)){
        this_date <- unique_dates[i]
        
        sub_temp_data <- temp_data %>% dplyr::filter(todays_date<=this_date)
        y_value <- mean(sub_temp_data$y, na.rm = TRUE)
        temp <- tibble('date'= this_date,
                       'y_value'= y_value)
        loop_list[[i]] <- temp
        raw_list[[i]] <- sub_temp_data %>% dplyr::select(date = todays_date, y)
      }
      part1 <- bind_rows(loop_list)
      part2 <- bind_rows(raw_list)
      out <- list(part1, part2)
      return(out)
    }
    
    # get data
    indic <- input$indicator_time
    pd <- odk_data$data
    pd_people <- pd$minicensus_people
    pd <- pd$minicensus_main
    
    # for the people dataset, group by instance id and summarise
    pd_people <- pd_people %>% group_by(instance_id) %>%
      mutate(age = floor(as.numeric(as.Date(Sys.Date()) - as.Date(dob))/ 365.25)) %>%
      summarise(total_people = n(),
                sum_children = length(which(age<18)),
                sum_adults = length(which(age>=18))) %>%
      mutate(child_to_adult = round(sum_children/sum_adults, 2))
    pd_people$child_to_adult[is.infinite(pd_people$child_to_adult)] <- 0
    # join pd and pd_people by
    pd <- left_join(pd, pd_people, by = 'instance_id')
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){
      if(indic=='Household size'){
        var_name <- 'hh_size'
      } else if(indic=='Number of cattle per household'){
        var_name <- 'hh_n_cows_less_than_1_year'
      } else if(indic=='Number of pigs per household'){
        var_name <- 'hh_n_pigs_less_than_6_weeks'
      } else if(indic=='Number of mosquito nets per household'){
        var_name <- 'n_nets_in_hh'
      } else if(indic=='Ratio of children to adults'){
        var_name <- 'child_to_adult'
      } else if(indic=='Number of children'){
        var_name <- 'sum_children'
      }
      plot_data <- cummean_date(pd, var_name = var_name)[[1]]
      raw_data <- cummean_date(pd, var_name = var_name)[[2]]
      
      ggplot() +
        geom_jitter(data = raw_data,
                    aes(x = date,
                        y = y),
                    size = 0.2,
                    alpha = 0.2,
                    height = 0.5) +
        geom_line(data = plot_data, aes(date,y_value),
                  color = 'darkred') +
        geom_point(data = plot_data, aes(date,y_value)) +
        labs(x = 'Date',
             y='Average up until date') +
        theme_bohemia() +
        geom_label(data = plot_data,
                   aes(x = date,
                       y = y_value,
                       label = round(y_value, 2)))
      
    } else {
      NULL
    }
  })
  
  
  
  
  # Field monitoring UI  #############################################
  field_monitoring_geo <- reactiveVal('Ward')
  output$ui_field_monitoring_by <- renderUI({
    
    cn <- input$geo
    if(cn=='Rufiji'){
      cn_choices = c('District',
                     'Ward',
                     'Village',
                     'Hamlet')
    } else {
      cn_choices = c('Distrito' = 'District',
                     'Posto administrativo/localidade' ='Ward',
                     'Povoado' ='Village',
                     'Bairro' ='Hamlet')
    }
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'field_monitoring' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              fmg <- field_monitoring_geo()
              fluidPage(
                column(12, align = 'center',
                       radioButtons('field_monitor_by',
                                    'Geographic level:',
                                    choices = cn_choices,
                                    selected = fmg,
                                    inline = TRUE))
              )
            })
  })
  
  # date slider for progress by geography
  output$ui_progress_by_date <- renderUI({
    
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'field_monitoring' %in% si$access
    pd <- odk_data$data
    pd <- pd$minicensus_main
    co <- country()
    pd <- pd %>% filter(hh_country==co)
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              fluidPage(
                column(6,
                       sliderInput(inputId = 'progress_by_date', 
                                   label = 'Select dates', 
                                   min = min(pd$todays_date), 
                                   max=max(pd$todays_date), 
                                   value = c(min(pd$todays_date), max(pd$todays_date))))
              )
            })
  })
  
  
  
  # Progress by geographical unit
  output$dt_monitor_by_table <- DT::renderDataTable({
    
    # Get the odk data
    pd <- odk_data$data
    
    enum <- pd$enumerations
    va <- pd$va
    ehd <- estimated_households$data
    
    # ref <- pd$refusals
    pd <- pd$minicensus_main
    co <- country()
    the_iso <- ifelse(co == 'Tanzania', 'TZA', 'MOZ')
    # save(pd, enum, va, co, the_iso, file = '/tmp/pd.RData')
    
    # get country
    pd <- pd %>% filter(hh_country==co)
    enum <- enum %>% filter(country==co)
    # va <- va %>% filter(the_country==co)
    # ref <- ref %>% filter(country==co)
    
    # subset by date
    time_period <- input$progress_by_date
    if(is.null(time_period)){
      time_period <- c(as.Date('2020-01-01'), Sys.Date())
    } 
    
    pd <- pd %>% filter(todays_date >= time_period[1],
                        todays_date <=time_period[2]) 
    enum <- enum %>% filter(todays_date >= time_period[1],
                            todays_date <=time_period[2]) 
    va <- va %>% filter(todays_date >= time_period[1],
                        todays_date <=time_period[2]) 
    # save(pd, enum, va, file = 'temp_gps.rda')
    pd_ok <- FALSE
    
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(!pd_ok){
      out <- NULL
    } else {
      # Create a progress table
      # target <- ifelse(co == 'Tanzania', 
      #                  estimated_households$data %>% filter(iso == 'TZA', clinical_trial == 0) %>% summarise(x = sum(n_households)) %>% .$x,
      #                  30803)
      
      target <- #ifelse(co == 'Tanzania', 
        ehd %>% filter(iso == the_iso, clinical_trial == 0) %>% summarise(x = sum(n_households)) %>% .$x#,
      #                30803)
      
      progress_table <- tibble(`Forms finished` = nrow(pd),
                               `Estimated total forms` = target,
                               `Estimated forms remaining` = target - nrow(pd),
                               `Estimated % finished` = round(nrow(pd) / target * 100, digits = 2))
      temp <- estimated_households$data
      # Create a detailed progress table (by hamlet)
      left <- estimated_households$data %>%
        filter(clinical_trial != 1) %>%
        filter(iso == the_iso) %>%
        dplyr::select(code, n_households)
      right <- pd %>%
        group_by(code = hh_hamlet_code) %>%
        summarise(numerator = n())
      right_enum <-enum%>%
        group_by(code = hamlet_code) %>%
        summarise(numerator = n())
      right_va <-va%>%
        mutate(hamlet_code = substr(death_id, 1, 3)) %>%
        group_by(code = hamlet_code) %>%
        summarise(`VA Forms done` = n())
      joined <- left_join(left, right, by = 'code') %>%
        mutate(numerator = ifelse(is.na(numerator), 0, numerator)) %>%
        mutate(p = numerator / n_households * 100) %>%
        mutate(p = round(p, digits = 2))
      joined_enum <-  left_join(left, right_enum, by = 'code') %>%
        mutate(numerator = ifelse(is.na(numerator), 0, numerator)) %>%
        mutate(p = numerator / n_households * 100) %>%
        mutate(p = round(p, digits = 2))
      progress_by_hamlet_enum <- joined_enum %>%
        left_join(locations %>% dplyr::select(code, Hamlet), by = 'code') %>%
        dplyr::select(code, Hamlet, `Enumerations Forms done` = numerator,
                      `Estimated number of forms` = n_households,
                      `Enumerations Estimated percent finished` = p)
      progress_by_hamlet <- joined %>%
        left_join(locations %>% dplyr::select(code, Hamlet), by = 'code') %>%
        dplyr::select(code, Hamlet, `Minicensus Forms done` = numerator,
                      `Estimated number of forms` = n_households) %>% 
        inner_join(progress_by_hamlet_enum) %>%
        # Get % minicensed of enumerated
        mutate(`% minicensed of enumerated` = `Minicensus Forms done` / `Enumerations Forms done` * 100)  %>%
        # add va
        left_join(right_va) %>%
        mutate(`VA Forms done` = ifelse(is.na(`VA Forms done`), 0, `VA Forms done`))
      
      # Transform the estimated number of forms (will be lower for MOZ than TZA since MOZ did buildings, not households)
      transformer <- ifelse(co == 'Mozambique', 0.55, 1)
      
      # Create a progress by geo tables
      apply_summary <- function(df){
        df %>%
          summarise(
            `Target forms (recon)` = sum(`Estimated number of forms`, na.rm = TRUE),
            `Enumeration forms done` = sum(`Enumerations Forms done`, na.rm = TRUE),
            `% Enumerated of target` = round(`Enumeration forms done` / `Target forms (recon)` * 100, digits = 2),
            `Minicensus forms done` = sum(`Minicensus Forms done`, na.rm = TRUE),
            `% Minicensus of enumerated` = round(`Minicensus forms done` / `Enumeration forms done` * 100, digits = 2),
            `% Minicensus form done from the total planned`  = round(sum(`Minicensus Forms done` ) / sum(`Target forms (recon)`) * 100, digits = 2),
            `VA forms done` = sum(`VA Forms done`, na.rm = TRUE),
            .groups = 'keep')
      }
      
      
      progress_by <- progress_by_hamlet %>% left_join(locations %>% 
                                                        dplyr::select(code, District, Ward, Village), by = 'code')
      
      
      progress_by_district <- progress_by %>% group_by(District) %>%
        apply_summary()
      
      progress_by_ward <- progress_by %>% 
        group_by(District, Ward) %>% 
        apply_summary
      progress_by_village <- progress_by %>% 
        group_by(District, Ward, Village) %>% 
        apply_summary
      progress_by_hamlet <- progress_by %>% 
        group_by(District, Ward, Village, Hamlet) %>% 
        apply_summary
      
      
      by_geo <- field_monitoring_geo() 
      
      if(is.null(by_geo)){
        monitor_by_table <- progress_by_district
      } else {
        cn <- input$geo
        cn <- ifelse(cn == 'Rufiji', 'Tanzania', 'Mozambique')
        if(by_geo == 'District'){
          if(cn == 'Mozambique'){
            names(progress_by_district)[1] <- 'Distrito'
            monitor_by_table <- progress_by_district
          } else {
            monitor_by_table <- progress_by_district
          }
        } else if(by_geo == 'Ward'){
          if(cn == 'Mozambique'){
            names(progress_by_ward)[1:2] <- c('Distrito', 'Posto administrativo/localidade')
            monitor_by_table <- progress_by_ward
          } else {
            monitor_by_table <- progress_by_ward
          }
        } else if(by_geo == 'Village'){
          if(cn == 'Mozambique'){
            names(progress_by_village)[1:3] <- c('Distrito', 'Posto administrativo/localidade', 'Povoado')
            monitor_by_table <- progress_by_village
          } else {
            monitor_by_table <- progress_by_village
          }
        } else if(by_geo == 'Hamlet'){
          if(cn == 'Mozambique'){
            names(progress_by_hamlet)[1:4] <- c('Distrito', 'Posto administrativo/localidade', 'Povoado', 'Bairro')
            monitor_by_table <- progress_by_hamlet
          } else {
            monitor_by_table <- progress_by_hamlet
          }
        }
      }
      
      if(cn == 'Tanzania'){
        monitor_by_table <- monitor_by_table[,!grepl('enumer', tolower(names(monitor_by_table)))]
      }
      
      message('---created progess table for Overview by geography')
      out <- bohemia::prettify(monitor_by_table, download_options = TRUE, nrows = nrow(monitor_by_table))
    }
    return(out)
  })
  
  # here
  # Progress by geographical unit
  output$dt_monitor_by_plot <- renderPlot({
    
    # Get the odk data
    pd <- odk_data$data
    
    # ref <- pd$refusals
    enum <- pd$enumerations
    pd <- pd$minicensus_main
    co <- country()
    the_iso <- ifelse(co == 'Tanzania', 'TZA', 'MOZ')
    ehd <- estimated_households$data
    time_period <- input$progress_by_date
    
    # save(pd, co, the_iso, ehd, enum,time_period, file = '/tmp/pd.RData')
    
    # get country
    pd <- pd %>% filter(hh_country==co)
    
    if(is.null(time_period)){
      time_period <- c(as.Date('2020-01-01'), Sys.Date())
    } 
    
    pd <- pd %>% filter(todays_date >= time_period[1],
                        todays_date <=time_period[2]) 
    enum <- enum %>% filter(todays_date >= time_period[1],
                            todays_date <=time_period[2]) 
    
    pd_ok <- FALSE
    
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(!pd_ok){
      out <- NULL
    } else {
      dt_choose_form <- input$dt_choose_form
      make_percent <- input$make_percent
      if(is.null(make_percent)){
        make_percent <- FALSE
      }
      # Create a detailed progress table (by hamlet)
      left <- ehd %>%
        filter(clinical_trial != 1) %>%
        filter(iso == the_iso) %>%
        dplyr::select(code, n_households)
      right <- pd %>%
        group_by(code = hh_hamlet_code) %>%
        summarise(numerator = n())
      right_enum <- enum %>%
        group_by(code = hamlet_code) %>%
        summarise(numerator = n())
      joined <- left_join(left, right, by = 'code') %>%
        mutate(numerator = ifelse(is.na(numerator), 0, numerator)) %>%
        mutate(p = (numerator / n_households) * 100) %>%
        mutate(p = round(p, digits = 2))
      joined_enum <- left_join(left, right_enum, by = 'code') %>%
        mutate(numerator = ifelse(is.na(numerator), 0, numerator)) %>%
        mutate(p = (numerator / n_households) * 100) %>%
        mutate(p = round(p, digits = 2))
      if(dt_choose_form=='Minicensus'){
        # get overall progress by hamlet code
        progress_by <- joined %>% left_join(locations %>% dplyr::select(code, Hamlet, District, Ward, Village), by = 'code')
      } else if(dt_choose_form=='Enumerations') {
        # get overall progress by hamlet code
        progress_by <- joined_enum %>% left_join(locations %>% dplyr::select(code, Hamlet, District, Ward, Village), by = 'code')
      } else {
        joined_enum$data <- 'Enumerations'
        joined$data <- 'Minicensus'
        joined <- rbind(joined, joined_enum)
        progress_by <- joined %>% left_join(locations %>% dplyr::select(code, Hamlet, District, Ward, Village), by = 'code')
      }
      
      if(dt_choose_form =='Both'){
        # by district
        progress_by_district <- progress_by %>% group_by(District,data) %>% summarise(n_households = sum(n_households, na.rm=TRUE), numerator=sum(numerator, na.rm = TRUE)) %>% mutate(`Percent finished` = round((numerator/n_households)*100,2))
        # by ward
        progress_by_ward <- progress_by %>% group_by(Ward,data) %>% summarise(n_households = sum(n_households, na.rm=TRUE), numerator=sum(numerator, na.rm = TRUE)) %>% mutate(`Percent finished` = round((numerator/n_households)*100,2))
        # by village
        progress_by_village <- progress_by %>% group_by(Village,data) %>% summarise(n_households = sum(n_households, na.rm=TRUE), numerator=sum(numerator, na.rm = TRUE)) %>% mutate(`Percent finished` = round((numerator/n_households)*100,2))
        # by hamlet
        progress_by_hamlet <- progress_by %>%ungroup %>% group_by(Hamlet,data) %>% summarise(n_households = sum(n_households, na.rm=TRUE), numerator=sum(numerator, na.rm = TRUE), .groups = 'drop') %>% mutate(`Percent finished` = round((numerator/n_households)*100,2))
        
      } else {
        # by district
        progress_by_district <- progress_by %>% group_by(District) %>% summarise(n_households = sum(n_households, na.rm=TRUE), numerator=sum(numerator, na.rm = TRUE)) %>% mutate(`Percent finished` = round((numerator/n_households)*100,2))
        # by ward
        progress_by_ward <- progress_by %>% group_by(Ward) %>% summarise(n_households = sum(n_households, na.rm=TRUE), numerator=sum(numerator, na.rm = TRUE)) %>% mutate(`Percent finished` = round((numerator/n_households)*100,2))
        # by village
        progress_by_village <- progress_by %>% group_by(Village) %>% summarise(n_households = sum(n_households, na.rm=TRUE), numerator=sum(numerator, na.rm = TRUE)) %>% mutate(`Percent finished` = round((numerator/n_households)*100,2))
        # by hamlet
        progress_by_hamlet <- progress_by %>%ungroup %>% group_by(Hamlet) %>% summarise(n_households = sum(n_households, na.rm=TRUE), numerator=sum(numerator, na.rm = TRUE), .groups = 'drop') %>% mutate(`Percent finished` = round((numerator/n_households)*100,2))
        
      }
      
      by_geo <- field_monitoring_geo() 
      
      if(is.null(by_geo)){
        monitor_plot <- progress_by_district
      } else {
        cn <- input$geo
        cn <- ifelse(cn == 'Rufiji', 'Tanzania', 'Mozambique')
        if(by_geo == 'District'){
          if(cn == 'Mozambique'){
            names(progress_by_district)[1] <- 'Distrito'
            monitor_plot <- progress_by_district
          } else {
            monitor_plot <- progress_by_district
          }
        } else if(by_geo == 'Ward'){
          if(cn == 'Mozambique'){
            names(progress_by_ward)[1] <- c('Posto administrativo/localidade')
            monitor_plot <- progress_by_ward
          } else {
            monitor_plot <- progress_by_ward
          }
        } else if(by_geo == 'Village'){
          if(cn == 'Mozambique'){
            names(progress_by_village)[1] <- c('Povoado')
            monitor_plot <- progress_by_village
          } else {
            monitor_plot <- progress_by_village
          }
        } else if(by_geo == 'Hamlet'){
          if(cn == 'Mozambique'){
            names(progress_by_hamlet)[1] <- c('Bairro')
            monitor_plot <- progress_by_hamlet
          } else {
            monitor_plot <- progress_by_hamlet
          }
        }
      }
      
      # condition text size on which geography was chosen
      if(by_geo=='Hamlet'){
        text_size = 4
      } else {
        text_size = 7
      }
      
      message('---created progess plot for Overview by geography')
      names(monitor_plot)[1] <- 'location'
      # paste together numerator and percent finished for every plot text
      monitor_plot$plot_text <- paste0(monitor_plot$numerator, '\n', round(monitor_plot$`Percent finished`))
      if(dt_choose_form=='Both'){
        out <- ggplot(monitor_plot, aes(reorder(location, -numerator), numerator, fill=data)) + geom_bar(stat='identity',alpha=0.7) +
          scale_fill_manual(name='', values=c('darkblue', 'darkgreen')) +
          # geom_text(aes(label=numerator), position = 'stack', vjust=1.5)+
          labs(x = '',
               y ='') + 
          theme_bohemia(base_size = 18) +
          theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = sizex),
                axis.ticks.x = element_blank(),
                axis.text.y = element_text(size = sizex))
        
      } else {
        if(make_percent){
          out <- ggplot(monitor_plot, aes(reorder(location,-`Percent finished`), `Percent finished`)) + geom_bar(stat='identity', fill = 'grey', alpha=0.5) +
            geom_text(aes(label=numerator), vjust=1, hjust = 0, size = text_size, angle = 90)+
            geom_text(aes(label=`Percent finished`), vjust=0, hjust = 1, size = text_size, angle = 90)+
            labs(x = '',
                 y ='') + 
            theme_bohemia(base_size = 18) +
            theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = sizex),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_text(size = sizex)) +
            ylim(0, max(monitor_plot$`Percent finished`, na.rm = TRUE) * 1.2)
          
        } else {
          out <- ggplot(monitor_plot, aes(reorder(location, -numerator), numerator)) + geom_bar(stat='identity', fill='grey', alpha=0.7) +
            geom_text(aes(label=numerator), vjust=1, hjust = 0, size = text_size, angle = 90)+
            geom_text(aes(label=`Percent finished`), vjust=0, hjust = 1, size = text_size, angle = 90)+            
            labs(x = '',
                 y ='') + 
            theme_bohemia(base_size = 18) +
            theme(axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size = sizex),
                  axis.ticks.x = element_blank()) +
            ylim(0, max(monitor_plot$numerator, na.rm = TRUE) * 1.2)
          
        }
        
      }
      
      
      
    }
    return(out)
  })
  
  
  # Map of progress by geography
  output$leaf_lx <- renderLeaflet({
    # Get the odk data
    pd <- odk_data$data
    pd <- pd$minicensus_main
    co <- country()
    the_iso <- ifelse(co == 'Tanzania', 'TZA', 'MOZ')
    pd <- pd %>% filter(hh_country == co)
    # save(pd, the_iso, co, file = ''/tmp/leaf.RData'/tmp/leaf.RData')
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(!pd_ok){
      out <- NULL
    } else {
      ll <- extract_ll(pd$hh_geo_location)
      pd$lng <- ll$lng; pd$lat <- ll$lat
      pdx <- pd %>%
        mutate(x = lng,
               y = lat)
      
      # Get location level
      # get input for which location to view
      map_location = field_monitoring_geo()
      message('---Field monitoring map location level is ', map_location)
      # get country to translate names for map title
      cn <- input$geo
      cn <- ifelse(cn == 'Rufiji', 'Tanzania', 'Mozambique')
      right <- estimated_households$data %>% left_join(locations %>% dplyr::select(code, district = District))
      if(is.null(map_location)){
        pdx$id <- paste0(pdx$hh_ward, ' ', pdx$hh_village, ' ', pdx$hh_hamlet)
        right$id <- paste0(right$ward, ' ', right$village, ' ', right$hamlet)
      } else {
        if(map_location == 'District'){
          pdx$id <- pdx$hh_district
          right$id <- right$district
        }
        if(map_location=='Hamlet'){
          pdx$id <- paste0(pdx$hh_ward, ' ', pdx$hh_village, ' ', pdx$hh_hamlet)
          right$id <- paste0(right$ward, ' ', right$village, ' ', right$hamlet)
        } else if(map_location=='Village'){
          pdx$id <- paste0(pdx$hh_ward, ' ', pdx$hh_village)
          right$id <- paste0(right$ward, ' ', right$village)
        } else if(map_location=='Ward'){
          pdx$id <- pdx$hh_ward
          right$id <- right$ward
        }
      }
      
      
      # Group by id and get number done
      df_pdx <-
        pdx %>%
        group_by(id) %>%
        tally %>%
        left_join(right %>% dplyr::group_by(id) %>% summarise(n_households = sum(n_households, na.rm = TRUE)), by = 'id') %>%
        mutate(p = n / n_households * 100) %>%
        mutate(p = ifelse(is.na(p), 0, p))
      pdx <- pdx %>% filter(!is.na(x), !is.na(y))
      coordinates(pdx) <- ~x+y
      proj4string(pdx) <- proj4string(moz1)
      out <- voronoi(pdx)
      out@data <- left_join(out@data, df_pdx)
      
      hqx <- hq$data
      
      l <- leaflet() %>% addTiles() %>%
        addMarkers(data = hqx,  
                   popup = hqx$label)
      
      # pal_all <- pal_hamlet <- colorNumeric(
      #   palette = c("black","darkred", 'red', 'darkorange', 'blue'),
      #   domain = 0:ceiling(max(out@data$p))
      # )
      bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf)
      pal_all <- colorBin("Spectral", domain = out@data$p, bins = bins)
      map_text <- paste(
        "Percent finished: ",  round(ifelse(is.na(out@data$p), 0, out@data$p),2),"<br>",
        as.character(out@data$id),"<br/>",
        sep="") %>%
        lapply(htmltools::HTML)
      # create map for hamlet
      leaflet_height <- 1000
      lxd <- leaflet(data = out, height = leaflet_height) %>%
        addProviderTiles(providers$Stamen.Toner) %>%
        addPolygons(
          fillColor = ~pal_all(p),
          opacity = 0.5,
          weight = 0.5,
          color = 'white',
          dashArray = '3',
          fillOpacity = 0.5,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = map_text,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
        addLegend(pal = pal_all, values = ~p, opacity = 0.7, title = NULL,
                  position = "bottomright")
      out <- lxd
    }
    return(out)
  })
  
  output$ui_progress_by_geographical_unit <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              fluidPage(
                h3('Progress by geographical unit'),
                uiOutput('ui_field_monitoring_by'),
                uiOutput('ui_progress_by_date'),
                column(12, #align = 'center',
                       tabsetPanel(
                         tabPanel('Table',
                                  div(DT::dataTableOutput('dt_monitor_by_table'), style = "font-size:80%")),
                         tabPanel('Plot',
                                  selectInput('dt_choose_form', 'Choose form', choices = c('Minicensus', 'Enumerations', 'Both')),
                                  checkboxInput(inputId = 'make_percent', label = 'Show percentage', value = FALSE),
                                  tags$div(style='overflow-x: scroll; position: relative', plotOutput('dt_monitor_by_plot', height = '800px', width = '4500px'))
                         ),
                         tabPanel('Map',
                                  leafletOutput('leaf_lx', height = 800)))))
            })
  })
  
  # Overall progress ui elements ####################################
  output$gt_overview <- gt::render_gt({
    
    # Get the odk data
    pd <- odk_data$data
    pd <- pd$minicensus_main
    co <- country()
    # save(pd, file = '/tmp/pd.RData')
    pd <- pd %>% filter(hh_country == co)
    ehd <- estimated_households$data
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){
      # List of fieldworkers
      fid_options <- sort(unique(pd$wid))
      fid_choices <- session_data$fieldworkers
      fid_choices <- fid_choices %>% dplyr::filter(as.numeric(id) %in% as.numeric(fid_options))
      x = as.character(fid_choices$name)
      y = as.character(fid_choices$id)
      fid_choices <- as.numeric(y)
      
      # Some pre-processing
      dr <- as.Date(range(pd$todays_date, na.rm = TRUE))
      n_days = as.numeric(1 + (max(dr)-min(dr)))
      the_iso <- iso <- ifelse(co == 'Tanzania', 'TZA', 'MOZ')
      # target <- sum(gps$n_households[gps$iso == iso], na.rm = TRUE)
      # target <- ifelse(iso == 'TZA', 
      #                  estimated_households$data %>% filter(iso == 'TZA', clinical_trial == 0) %>% summarise(x = sum(n_households)) %>% .$x,
      #                  30803)
      target <- #ifelse(co == 'Tanzania', 
        ehd %>% filter(iso == the_iso, clinical_trial == 0) %>% summarise(x = sum(n_households)) %>% .$x#,
      #                30803)
      
      # save(pd, file = 'temp_pd.rda')
      # create a placeholder for number of fieldworkers. 
      # num_fws <- length(unique(pd$wid)) #-12 - get average number of unique fieldworkers per day
      if(co == 'Tanzania'){
        num_fws <- 73
      } else {
        num_fws <- 86
      }
      
      
      # Create table of overview
      overview <- pd %>%
        summarise(Type = 'Observed',
                  `No. FWs` = num_fws,
                  `Daily forms/FW` = round(nrow(pd) / `No. FWs` / n_days, digits = 1),
                  `Weekly forms/FW` = round(`Daily forms/FW` * 7, digits = 1),
                  `Total forms/FW` = round(nrow(pd) / `No. FWs`, digits = 1),
                  `Total Daily forms/country` = round(nrow(pd) / n_days, digits = 1),
                  `Total Weekly forms/country` = round(`Total Daily forms/country` * 7, digits = 1),
                  `Overall target/country` = nrow(pd),
                  `# Total weeks` = round(target/`Total Weekly forms/country`, digits = 1)) %>%
        mutate(`Estimated date` = (`# Total weeks` * 7) + as.Date(dr[1]))
      message('---Created "observed" row for overall progress table')
      
      # # Get a second row for targets
      # hard coded values for each country
      if(co=='Tanzania'){
        num_fws <- 77
        daily_forms_fw <- 13
        weekly_forms_fw <- daily_forms_fw*5
        total_forms_fw <- 599
        total_daily_country <- 1001
        total_weekly_country <- total_daily_country*5
        total_forms <- estimated_households$data %>% filter(iso == 'TZA', clinical_trial == 0) %>% summarise(x = sum(n_households)) %>% .$x
        total_weeks <- round(total_forms/total_weekly_country,2)
        total_days <- total_weeks*7
        est_date <- Sys.Date()+total_days
      } else {
        num_fws <- 86
        daily_forms_fw <- 10
        weekly_forms_fw <- daily_forms_fw*5
        total_forms_fw <- 500
        total_daily_country <- 1000
        total_weekly_country <- total_daily_country*5
        # total_forms <- 30803
        total_forms <- estimated_households$data %>% filter(iso == 'MOZ', clinical_trial == 0) %>% summarise(x = sum(n_households)) %>% .$x
        
        total_weeks <- round(total_forms/total_weekly_country,2)
        total_days <- total_weeks*7
      }
      second_row <- 
        tibble(Type = 'Target',
               `No. FWs` = num_fws,  
               `Daily forms/FW` = daily_forms_fw,
               `Weekly forms/FW` = weekly_forms_fw,
               `Total forms/FW` = total_forms_fw,
               `Total Daily forms/country` = total_daily_country,
               `Total Weekly forms/country` = total_weekly_country,
               `Overall target/country` = total_forms,
               `# Total weeks` = total_weeks, #as.character(target_helper$end_date))
               `Estimated date` = as.character(Sys.Date()+total_days))
      message('---Created "target" row for overall progress table')
      
      third_row <- 
        tibble(Type = 'Percentage',
               `No. FWs` = round(overview$`No. FWs`/second_row$`No. FWs` * 100),  
               `Daily forms/FW` = round(overview$`Daily forms/FW`/second_row$`Daily forms/FW`, 2)  * 100,
               `Weekly forms/FW` = round(overview$`Weekly forms/FW`/second_row$`Weekly forms/FW`,2) * 100,
               `Total forms/FW` = round(overview$`Total forms/FW`/second_row$`Total forms/FW`,2) * 100,
               `Total Daily forms/country` = round(overview$`Total Daily forms/country`/second_row$`Total Daily forms/country`,2) * 100,
               `Total Weekly forms/country` = round(overview$`Total Weekly forms/country`/second_row$`Total Weekly forms/country`,2) * 100,
               `Overall target/country` = round(overview$`Overall target/country`/total_forms,2)*100,
               `# Total weeks` = ' ', #round(overview$`# Total weeks`/second_row$`# Total weeks`,2),
               `Estimated date` = '')
      message('---Created "percentage" row for overall progress table')
      
      # save(pd, overview,target_helper, second_row, file = '/tmp/overview.RData')
      for(j in 1:ncol(overview)){
        overview[,j] <- as.character(overview[,j])
      }
      for(j in 1:ncol(second_row)){
        second_row[,j] <- as.character(second_row[,j])
      }
      
      for(j in 1:ncol(third_row)){
        third_row[,j] <- as.character(third_row[,j])
      }
      overview <- bind_rows(overview, second_row, third_row)
      message('---Created estimated targets table')
    } else {
      overview <- NULL
    }
    return(overview)
  })
  
  output$plot_weekly_progress <- renderPlot({
    # Get the odk data
    pd <- odk_data$data
    co <- country()
    ehd <- estimated_households$data
    # save(pd, file = '/tmp/pd.RData')
    enums <- pd$enumerations
    va <- pd$va
    pd <- pd$minicensus_main
    
    if(co == 'Mozambique'){
      the_iso <- 'MOZ'
      the_server <- 'https://sap.manhica.net:4442/ODKAggregate'
    } else {
      the_iso <- 'TZA'
      the_server <- 'https://bohemia.ihi.or.tz'
    }
    
    pd <- pd %>% filter(hh_country == co)
    va <- va %>% filter(server == the_server)
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){
      target <- #ifelse(co == 'Tanzania', 
                       ehd %>% filter(iso == the_iso, clinical_trial == 0) %>% summarise(x = sum(n_households)) %>% .$x#,
       #                30803)
      x <- pd %>%
        group_by(date = as.Date(todays_date)) %>%
        tally %>%
        mutate(denom = target) %>%
        mutate(cs = cumsum(n)) %>%
        mutate(p = cs / denom * 100)
      y <- enums %>%
        group_by(date = as.Date(todays_date)) %>%
        summarise(enums = n())
      z <- va %>%
        group_by(date = as.Date(todays_date)) %>%
        summarise(vas = n())
      joined <- 
        full_join(x, y) %>% full_join(z)
      plot_data <- joined %>%
        dplyr::select(date, Minicensus = n,
                      Enumerations = enums,
                      VA = vas) %>%
        tidyr::gather(key, value, Minicensus:VA) %>%
        mutate(week = lubridate::ceiling_date(date, unit = 'weeks')) %>%
        group_by(week, key) %>%
        summarise(value = sum(value, na.rm = TRUE))
      g <- ggplot(data = plot_data,
                  aes(x = week,
                      y = value)) +
        geom_bar(stat = 'identity') +
        facet_wrap(~key, scales = 'free_y') +
        theme_bohemia() +
        labs(x = 'Week ending on',
             y = 'Forms') +
        theme(axis.text.x = element_text(angle = 90, 
                                         vjust = 1,
                                         hjust = 0.5)) +
        scale_x_date(labels = sort(unique(plot_data$week)),
                     breaks = sort(unique(plot_data$week))) +
        geom_text(aes(label = value,
                      hjust = 1),
                  alpha = 0.6,
                  angle = 90,
                  color = 'white')
    } else {
      g <- ggplot() 
    }
    message('---Created weekly progress plot ')
    return(g)
  })
  
  output$plot_progress <- renderPlot({
    # Get the odk data
    pd <- odk_data$data
    pd <- pd$minicensus_main
    co <- country()
    ehd <- estimated_households$data
    
    if(co == 'Mozambique'){
      the_iso <- 'MOZ'
    } else {
      the_iso <- 'TZA'
    }
    # save(pd, file = '/tmp/pd.RData')
    pd <- pd %>% filter(hh_country == co)
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){
      # target <- ifelse(co == 'Tanzania', 
      #                  estimated_households$data %>% filter(iso == 'TZA', clinical_trial == 0) %>% summarise(x = sum(n_households)) %>% .$x,
      #                  30803)
      target <- #ifelse(co == 'Tanzania', 
        ehd %>% filter(iso == the_iso, clinical_trial == 0) %>% summarise(x = sum(n_households)) %>% .$x#,
      #                30803)
      x <- pd %>%
        group_by(date = as.Date(todays_date)) %>%
        tally %>%
        mutate(denom = target) %>%
        mutate(cs = cumsum(n)) %>%
        mutate(p = cs / denom * 100)
      g <- ggplot(data = x,
                  aes(x = date,
                      y = p)) +
        geom_point() +
        geom_area(alpha = 0.3, fill = 'red', color = 'black') +
        theme_bohemia() +
        labs(x = 'Date',
             y = 'Percent of target completed',
             title = 'Cumulative percent of target completed') +
        ylim(0,100)
    } else {
      g <- ggplot() 
    }
    message('---Created overall progress plot ')
    return(g)
  })
  output$gt_progress_table <- gt::render_gt({
    
    # Get the odk data
    pd <- odk_data$data
    pd <- pd$minicensus_main
    co <- country()
    ehd <- estimated_households$data
    
    # save(pd, co, ehd, file = '/tmp/pd.RData')
    pd <- pd %>% filter(hh_country == co)
    if(co == 'Mozambique'){
      the_iso <- 'MOZ'
    } else {
      the_iso <- 'TZA'
    }
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){

      target <- ehd %>% 
        filter(iso == the_iso, clinical_trial == 0) %>% 
        summarise(x = sum(n_households)) %>% 
        .$x
      
      progress_table <- tibble(`Forms finished` = nrow(pd),
                               `Estimated total forms` = target,
                               `Estimated forms remaining` = target - nrow(pd),
                               `Estimated % finished` = round(nrow(pd) / target * 100, digits = 2))
    } else {
      progress_table <- data.frame(a = 0)
    }
    message('---Created overall progress table ')
    return(progress_table)
  })
  
  output$ui_overall_progress <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              fluidPage(
                gt::gt_output('gt_overview'),
                fluidRow(column(12,
                                h3('Weekly progress plot'),
                                plotOutput('plot_weekly_progress'))),
                fluidRow(
                  column(6,
                         h3('Overall progress plot'),
                         plotOutput('plot_progress')),
                  column(6,
                         h3('Overall progress table'),
                         gt::gt_output('gt_progress_table'))
                ))
            })
  })
  
  # Individual data
  output$leaf_fid <- renderLeaflet({
    # Get the odk data
    who <- input$fid_gps
    pd <- odk_data$data
    pd <- pd$minicensus_main
    co <- country()
    pd <- pd %>% filter(hh_country == co)
    if(co == 'Mozambique'){
      shp_file = mop3
    } else {
      shp_file = ruf3
    }
    # save(pd, file = 'fid_pd.rda')
    if(is.null(who)){
      who <- 0
    }
    pd <- pd %>% filter(wid==who)
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    
    hqx <- hq$data
    
    if(pd_ok){
      ll <- extract_ll(pd$hh_geo_location)
      pd$lng <- ll$lng; pd$lat <- ll$lat
      
      # get shape files 
      # round table not here, and here make sure map works (not saving data below)
      l <- leaflet(shp_file) %>%
        # addProviderTiles(providers$Stamen.Toner) %>%
        # addProviderTiles(providers$Esri.WorldImagery) %>%
        addTiles() %>%
        addPolygons(weight = 1,label = shp_file@data$NAME_3) %>%
        clearMarkers() %>%
        addMarkers(data = hqx,  
                   popup = hqx$label) %>%
        addCircleMarkers(data = pd, lng = pd$lng, lat = pd$lat)
      
    } else {
      l <- leaflet() %>% 
        addProviderTiles(providers$Esri.WorldImagery) %>% # addProviderTiles(providers$Stamen.Toner)
        
        addMarkers(data = hqx,  
                   popup = hqx$label) 
      
    }
    l
  })
  
  
  # table for anomaly description
  output$table_individual_errors <- renderTable({
    an <- session_data$anomalies
    who <- input$fid
    the_country <- country()
    
    an <- an %>% filter(wid==who)
    # an <- an %>% filter(country==the_country)
    
    if(nrow(an)>0){
      pd <- an %>%
        group_by(type, description) %>%
        summarise(Occurrences = n()) %>%
        dplyr::arrange(desc(Occurrences))
      pd
    } else {
      NULL
    }
  })
  
  output$table_individual_details <- DT::renderDataTable({
    
    # Get the odk data
    pd <- odk_data$data
    # save(pd, file='/tmp/temp_ odk_dat.rda')
    enum <- pd$enumerations
    va <- pd$va
    ref <- pd$refusals
    pd <- pd$minicensus_main
    co <- country()
    # pd <- pd %>% filter(hh_country == co)
    # enum <- enum %>% filter(country==co)
    # va <- va %>% filter(the_country==co)
    # ref <- ref %>% filter(country==co)
    min_date <- min(pd$todays_date, na.rm = TRUE)
    max_date <- max(pd$todays_date, na.rm = TRUE)
    seq_days <- seq(min_date, max_date, by = 1)
    seq_days <- seq_days[!weekdays(seq_days) %in% c('Saturday', 'Sunday')]
    n_days <- length(seq_days)
    # get anomaly dataset
    an <- session_data$anomalies
    # save(pd, co, an,
    #      file = '/tmp/fw.RData')
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){
      if(co=='Tanzania'){
        num_fws <- 77
        daily_forms_fw <- 13
        weekly_forms_fw <- daily_forms_fw*5
        total_forms_fw <- 599
        
      } else {
        num_fws <- 100
        daily_forms_fw <- 10
        weekly_forms_fw <- daily_forms_fw*5
        total_forms_fw <- 500
      }
      who <- input$fid
      who <- as.character(who)
      
      pd <- pd %>%
        mutate(end_time = lubridate::as_datetime(end_time))
      if(nrow(an) > 0){
        an$date <- as.Date(an$date, format='%Y-%m-%d')
        an <- an %>% filter(as.character(wid)==who)
        num_anomaly <- length(which(an$type=='anomaly'))
        num_error <- length(which(an$type=='error'))
      } else {
        num_anomaly <- 0
        num_error <- 0
      }
      
      if(is.null(who)){
        who <- '0'
      }
      
      id <- who
      pd <- pd %>% filter(as.character(wid) == id)
      enum <- enum %>% filter(as.character(wid)==id)
      va <- va %>% filter(as.character(wid)==id)
      ref <- ref %>% filter(as.character(wid)==id)
      
      last_upload <- as.character(max(pd$end_time, na.rm = TRUE))
      sup_name <- as.character(fids$supervisor[fids$bohemia_id == id])
      total_forms <- nrow(pd)
      average_time <- mean(pd$end_time - pd$start_time)
      average_time <- paste0(round(as.numeric(average_time), digits = 2), ' ', attr(average_time, 'units'))
      daily_work_hours <- '(pending)'
      rolling_per = round((total_forms / n_days) / daily_forms_fw * 100, digits = 2)
      total_per = round(total_forms/total_forms_fw * 100,2)
      message('---Created FW performance individual table ')
      
      # get number of enumeration, refusals, and va forms filled 
      num_enum = nrow(enum)
      num_va = nrow(va)
      num_ref = nrow(ref)
      
      # last_days <-paste0('Last ', time_period, ' days')
      bohemia::prettify(
        tibble(` ` = c('Supervisor','% of rolling minicensus target','% of total minicensus target','# of minicensus forms','# of enumeration forms','# of va forms','# of refusals','# of anomalies', '# of errors','Average time/form', 'Daily work hours'), `   ` = c(sup_name, rolling_per, total_per, total_forms, num_enum, num_va, num_ref,num_anomaly, num_error,average_time, daily_work_hours)),
        nrows = 11
      )
    } else {
      NULL
    }
    
  })
  
  output$plot_individual_errors <- renderPlot({
    an <- session_data$anomalies
    who <- input$fid
    the_country <- country()
    
    an <- an %>% filter(wid==who)
    an <- an %>% filter(country==the_country)
    
    if(nrow(an)>0){
      an <- an %>%
        mutate(date = strsplit(as.character(date), ",")) %>%
        unnest(date) %>%
        group_by(date) %>%
        summarise(`# of anomalies` = sum(type == 'error', na.rm=TRUE),
                  `# of errors` = sum(type == 'anomaly', na.rm=TRUE)) %>%
        gather(key=key, value=value, -date) %>%
        mutate(date = as.Date(date))
      ggplot(an, aes(date, value, fill=key)) +
        geom_bar(stat = 'identity', position = 'dodge') +
        scale_fill_manual(name = '',
                          values = c('black', 'grey')) +
        labs(x='Date', y='', title='Number of errors and anomalies') +
        theme_bohemia() +
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              plot.title = element_text(size = 20))
    } else {
      NULL
    }
    
    
  })
  
  
  output$plot_individual_target <- renderPlot({
    
    # Get the odk data
    pd <- odk_data$data
    pd <- pd$minicensus_main
    co <- country()
    pd <- pd %>% filter(hh_country == co)
    
    if(co=='Mozambique'){
      daily_forms_fw <- 10
      total_forms_fw <- 500
    } else {
      daily_forms_fw <- 13
      total_forms_fw <- 599
    }
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){
      who <- input$fid
      if(is.null(who)){
        who <- 0 
      }
      
      min_date <- min(pd$todays_date, na.rm = TRUE)
      max_date <- max(pd$todays_date, na.rm = TRUE)
      seq_days <- seq(min_date, max_date, by = 1)
      seq_days <- seq_days[!weekdays(seq_days) %in% c('Saturday', 'Sunday')]
      
      left <- tibble(date = seq_days) %>%
        mutate(target = daily_forms_fw *(1:length(seq_days)))
      
      plot_data <- pd %>% filter(wid == who)
      if(nrow(plot_data) > 0){
        plot_data <- plot_data %>%
          group_by(date = todays_date) %>%
          tally %>%
          ungroup 
        joined <- left_join(left, plot_data) %>%
          mutate(n = ifelse(is.na(n), 0, n)) %>%
          mutate(cs = cumsum(n)) %>%
          dplyr::select(date, Target = target, Observed = cs ) %>%
          tidyr::gather(key, value, Target:Observed)
        
        g <- ggplot(data = joined,
                    aes(x = date,
                        y = value)) +
          geom_step(aes(color = key,
                        group = key)) +
          geom_point(aes(color = key)) +
          labs(x = 'Date',
               y = 'Forms collected',
               title = 'Forms collected by date') +
          theme_bohemia() +
          scale_color_manual(name = '',
                             values = c('red', 'blue')) +
          theme(legend.position = 'bottom') +
          theme(axis.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                plot.title = element_text(size = 20))
        
        
      } else {
        g <- NULL
      }
      
    } else {
      g <- NULL
    }
    return(g)
    
  })
  
  output$plot_individual_details <- renderPlot({
    
    # Get the odk data
    pd <- odk_data$data
    pd <- pd$minicensus_main
    co <- country()
    pd <- pd %>% filter(hh_country == co)
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){
      who <- input$fid
      if(is.null(who)){
        who <- 0 
      }
      plot_data <- pd %>% filter(wid == who)
      if(nrow(plot_data) > 0){
        plot_data <- plot_data %>%
          group_by(date = todays_date) %>%
          tally
        
        g <- ggplot(data = plot_data,
                    aes(x = date,
                        y = n)) +
          geom_bar(stat = 'identity',
                   fill = 'black') +
          labs(x = 'Date',
               y = 'Forms collected',
               title = 'Forms collected by date') +
          theme_bohemia() +
          theme(axis.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                plot.title = element_text(size = 20))
        
        
      } else {
        g <- NULL
      }
      
    } else {
      g <- NULL
    }
    return(g)
    
  })
  
  output$plot_individual_form_time <- renderPlot({
    
    # Get the odk data
    pd <- odk_data$data
    pd <- pd$minicensus_main
    co <- country()
    pd <- pd %>% filter(hh_country == co)
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){
      who <- input$fid
      if(is.null(who)){
        who <- 0 
      }
      plot_data <- pd %>% filter(wid == who)
      if(nrow(plot_data) > 0){
        plot_data$time_taken <- plot_data$end_time - plot_data$start_time
        units_taken <- attr(plot_data$time_taken, 'units')
        avg <- mean(plot_data$time_taken)
        avg <- round(avg, digits = 2)
        n <- nrow(plot_data)
        g <- ggplot(data = plot_data,
                    aes(x = time_taken)) +
          geom_histogram(fill = 'darkred', alpha = 0.6, color = 'black') +
          labs(x = paste0('Time per form (', units_taken, ')'),
               y = 'Density',
               title = paste0('Distribution of time taken per form for FW ',
                              who, '; ', n, ' forms, ', avg, ' ', units_taken)) +
          theme_bohemia() +
          theme(axis.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                plot.title = element_text(size = 20))
        
        
      } else {
        g <- NULL
      }
      
    } else {
      g <- NULL
    }
    return(g)
    
  })
  
  
  
  output$ui_individual_data <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              # Get the odk data
              pd <- odk_data$data
              pd <- pd$minicensus_main
              co <- country()
              # save(pd, file = '/tmp/pd.RData')
              pd <- pd %>% filter(hh_country == co)
              
              # Define choices for leaflet fw selection
              co <- country()
              sub_fids <- fids %>% 
                filter(country == co)
              the_choices <- sub_fids$bohemia_id
              names(the_choices) <- paste0(sub_fids$bohemia_id, '. ',
                                           sub_fids$first_name, ' ',
                                           sub_fids$last_name)
              
              
              fluidPage(
                # fluidRow(
                #   infoBox(title = 'Number of detected anomalies',
                #           icon = icon('microscope'),
                #           color = 'black',
                #           width = 6,
                #           h1(0)),
                #   infoBox(title = 'Number of detected errors',
                #           icon = icon('address-book'),
                #           color = 'black',
                #           width = 6,
                #           h1('0%'))
                # ),
                fluidRow(
                  column(12,
                         selectInput('fid',
                                     'Fieldworker ID',
                                     choices = the_choices)))
              )
            })
  })
  
  output$plot_individual_time_matrix <- renderPlot({
    
    # Get the odk data
    pd <- odk_data$data
    enum <- pd$enumerations
    # refs <- pd$refusals
    va <- pd$va
    pd <- pd$minicensus_main
    co <- country()
    pd <- pd %>% filter(hh_country == co)
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){
      who <- input$fid
      if(is.null(who)){
        who <- 0 
      }
      # save(pd, co, pd_ok, enum, va, who, file = '/tmp/time_matrix.RData')
      plot_data <- 
        bind_rows(
          pd %>% filter(wid == who) %>% dplyr::select(start_time, end_time, instance_id) %>% mutate(Form = 'Minicensus'),
          enum %>% filter(wid == who) %>% dplyr::select(start_time, end_time, instance_id) %>% mutate(Form = 'Enumerations'),
          # refs %>% filter(wid == who) %>% mutate(Form = 'Refusals'),
          va %>% filter(wid == who) %>% dplyr::select(start_time, end_time, instance_id) %>% mutate(Form = 'VA'),
        )
      if(nrow(plot_data) > 0){
        plot_data$time_taken <- plot_data$end_time - plot_data$start_time
        units_taken <- attr(plot_data$time_taken, 'units')
        plot_data$date <- plot_data$start_date <- as.Date(plot_data$start_time)
        plot_data$end_date <- as.Date(plot_data$end_time)
        plot_data$start_val <- lubridate::hour(plot_data$start_time) +
          (lubridate::minute(plot_data$start_time)/60)
        plot_data$end_val <- lubridate::hour(plot_data$end_time) +
          (lubridate::minute(plot_data$end_time)/60)
        plot_data <- plot_data %>%
          mutate(end_val = ifelse(end_date != start_date, 24, end_val))
        # plot_data$start_val <- round(plot_data$start_val, 1)
        # plot_data$end_val <- round(plot_data$end_val, 1)
        # plot_data$end_val <- ifelse(plot_data$end_val == plot_data$start_val,
        #                             plot_data$end_val + 0.1, plot_data$end_val)
        # plot_data$end_val <- ifelse(plot_data$end_date != plot_data$date,
        #                             24, plot_data$end_val)
        # expanded_list <- list()
        # for(i in 1:nrow(plot_data)){
        #   print(i)
        #   this_row <- plot_data[i,]
        #   these_times <- seq(this_row$start_val, this_row$end_val, by = 0.1)
        #   this_out <- tibble(times = these_times,
        #                      Form = this_row$Form,
        #                      date = this_row$date)
        #   expanded_list[[i]] <- this_out
        # }
        # expanded <- bind_rows(expanded_list)
        # right <- expanded %>%
        #   group_by(hour = times, Form, date) %>%
        #   tally
        # 
        # date_seq <- seq(min(plot_data$date),
        #                 max(plot_data$date),
        #                 by = 1)
        # left <- expand.grid(hour = seq(0, 24, by = 0.1),
        #                     date = date_seq,
        #                     Form = sort(unique(right$Form)))
        # joined <- left_join(left, right)# %>%
        #   # mutate(n = ifelse(is.na(n), 0, n))
        # 
        # g <- ggplot(data = joined,
        #             aes(x = hour,
        #                 y = date,
        #                 color = Form,
        #                 size = n)) +
        #   geom_point() +
        #   theme_bohemia() +
        #   scale_y_date(breaks = date_seq, labels = date_seq) +
        #   labs(x = 'Hour of day',
        #        y = 'Date') +
        #   scale_size_area(name = 'Number of open forms')
        
        cols <- rainbow(length(unique(plot_data$instance_id)))
        cols <- sample(cols, length(cols))
        g <- ggplot(data = plot_data,
                    aes(x = start_val,
                        xend = end_val,
                        y = start_time,
                        yend = start_time,
                        color = instance_id)) +
          geom_segment(size = 1,
                       alpha = 0.6) +
          geom_point(aes(x = start_val,
                         y = start_time,
                         color = instance_id),
                     pch = 5) +
          geom_point(aes(x = end_val,
                         y = start_time,
                         color = instance_id),
                     pch = 3) +
          theme_bohemia() +
          labs(x = 'Time of day (24 hour clock)',
               y = 'Date') +
          scale_color_manual(name = '', values = cols) +
          theme(legend.position = 'none') +
          labs(subtitle = 'Diamond: start time; Cross: end time') +
          theme(axis.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                plot.title = element_text(size = 20))
        
      } else {
        g <- NULL
      }
      
    } else {
      g <- NULL
    }
    return(g)
    
    
    
  })
  
  output$ui_individual_out <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              fluidPage(
                fluidRow(
                  column(6,
                         DT::dataTableOutput('table_individual_details')),
                  column(6,
                         tableOutput('table_individual_errors'))),
                fluidRow(
                  column(12,
                         plotOutput('plot_individual_time_matrix',
                                    height = '600px'))
                ),
                fluidRow(
                  column(6,
                         plotOutput('plot_individual_target')),
                  column(6,
                         plotOutput('plot_individual_errors'))
                ),
                fluidRow(
                  column(6,
                         plotOutput('plot_individual_details')),
                  column(6,
                         plotOutput('plot_individual_form_time'))
                )
              )})
  })
  
  # Text above VA table
  output$va_text <- renderText({
    pd <- session_data$va_table
    out <- NULL
    if(nrow(pd) == 0){
      out <- 'No unfinished VA forms for the location(s) selected. Consider modifying locations (left) and then clicking "Update VA sheet" above.'
    }
    return(out)
  })
  
  # VA DATA
  
  
  observeEvent({
    # x = location_code()
    input$update_va_sheet
    # y = input$va_n_teams
  },{
    
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    if(li){
      pd <- odk_data$data
      pd <- pd$minicensus_main
      cn <- country()
      pd <- pd %>% filter(hh_country == cn)
      
      pd_ok <- FALSE
      if(!is.null(pd)){
        if(nrow(pd) > 0){
          pd_ok <- TRUE
        }
      }
      if(pd_ok){
        # va table
        people <- odk_data$data$minicensus_people
        deaths <- odk_data$data$minicensus_repeat_death_info
        deaths <- deaths %>% filter(instance_id %in% pd$instance_id)
        # va_refusals <- odk_data$data$va_refusals
        # save(people, va_refusals, deaths, pd, cn, file = '/tmp/va.RData')
        # Conditional mourning period
        mourning_period <- ifelse(cn == 'Mozambique', 30, 40)
        va <- left_join(deaths %>%
                          left_join(pd %>%
                                      dplyr::select(instance_id, todays_date), by = 'instance_id') %>%
                          mutate(todays_date = as.Date(todays_date),
                                 death_dod = as.Date(death_dod)) %>%
                          mutate(old = (todays_date - death_dod) > mourning_period) %>%
                          mutate(time_to_add = ifelse(old, 7, mourning_period)) %>%
                          mutate(xx = todays_date + time_to_add#, # this needs to be 7 days after hh visit date if death was <40 days prior to hh visit date | 40 days after hh visit date if the death was >40 days after hh visit date
                                 # yy = Sys.Date() - todays_date
                          ) %>%
                          # Note: in case the "date of death" is unknown (the form has that option): let's just calculate the "latest date to do VA" by adding 40 days (Tanzania) and 30 days (Moz) to the "date of the hh visit", to be safe.
                          mutate(safe_bet = todays_date + mourning_period) %>%
                          mutate(xx = ifelse(is.na(xx), safe_bet, xx)) %>%
                          mutate(xx = as.Date(xx, origin = '1970-01-01')) %>%
                          mutate(yy = Sys.Date() - xx) %>% # "time elapsed" means time between lastest date to collect and today
                          dplyr::select(instance_id,
                                        death_initials,
                                        `Date of death` = death_dod,
                                        `Latest date to collect VA form` = xx,
                                        `ID of deceased person` = death_id,
                                        `Time elapsed` = yy),
                        pd %>%
                          mutate(num = as.numeric(hh_head_id)) %>%
                          dplyr::select(instance_id,
                                        District = hh_district,
                                        Ward = hh_ward,
                                        Village = hh_village,
                                        Hamlet = hh_hamlet,
                                        `HH ID` = hh_id,
                                        `FW ID` = wid,
                                        num, # for getting the initials of household head
                                        `HH visit date` = todays_date), by = 'instance_id') %>%
          left_join(people %>% dplyr::select(instance_id,
                                             initials,
                                             pid,
                                             num), by = c('instance_id', 'num')) %>%
          mutate(`FW ID` = ' ') %>%
          dplyr::select(-instance_id) %>%
          mutate(`HH head initials and ID` = paste0(pid, ' (',initials, ')'))
        
        # Remove those which have already been collected
        already_done <- unique(odk_data$data$va$death_id)
        va <- va %>% filter(!`ID of deceased person` %in% already_done)
        
        # Remove those not in the selected location hierarchy
        lc <- location_code()
        va <- va %>% filter(substr(`HH ID`, 1, 3) %in% lc)
        
        if(nrow(va) > 0){
          va <- va %>% dplyr::select(District,
                                     Ward,
                                     Village,
                                     Hamlet,
                                     `HH ID`,
                                     `HH head initials and ID`,
                                     # `FW ID`,
                                     `Initials of deceased person` = death_initials,
                                     `ID of deceased person`,
                                     `Date of death`,
                                     `Latest date to collect VA form`
          ) %>%
            mutate(`FW / Supervisor ID` = ' ',
                   `Date of VA visit` = ' ',
                   `ICF signed?` = 'Yes__ No__',
                   `VA form completed?` = 'Yes__ No__',
                   `If HH not visited or VA form not completed, why` = '                 ')
          if(cn=='Mozambique'){
            va <- va %>%  
              dplyr::mutate(
                `ICF signed?` = 'Sim__ Não__',
                `VA form completed?` = 'Sim__ Não__',
              ) %>%
              rename(Distrito = District,
                     `PA / Localidade`=Ward,
                     Povoado=Village,
                     Bairro=Hamlet,
                     `Iniciais do chefe do agregado e Ext_ID` = `HH head initials and ID`,
                     `Iniciais do(a) falecido(a)` = `Initials of deceased person`,
                     `Ext_ID Falecido(a)` = `ID of deceased person`,
                     `Data da morte` = `Date of death`,
                     `Última data para VA` = `Latest date to collect VA form`,
                     `ID Inquiridor` = `FW / Supervisor ID`,
                     `Data da visita` = `Date of VA visit`,
                     `Consentimiento informado assinado?`= `ICF signed?`,
                     `Formulária realizado` = `VA form completed?`,
                     `Se não foi visitado ou entrevistado, porque?` = `If HH not visited or VA form not completed, why`)
          } else {
            # TZA, populate ward supervisors automatically
            va <- va %>%
              left_join(tza_ward_supervisors) %>%
              mutate(`FW / Supervisor ID` =  paste0(Supervisor_Name, ' (id:', wid, ')')) %>%
              dplyr::rename(`Supervisor name (ID)` = `FW / Supervisor ID`) %>%
              dplyr::select(-wid, -Supervisor_Name)
          }
          
          # # Capture n teams
          # n_teams <- input$va_n_teams
          # if(is.null(n_teams)){
          #   n_teams <- 1
          # }
          # n_teams <- as.numeric(n_teams)
          # if(n_teams < 1 | n_teams > 100){
          #   n_teams <- 1
          # }
          # if(is.na(n_teams)){
          #   n_teams <- 1
          # }
          # team_vector <- sort(rep(1:n_teams, length = nrow(va)))
          # va$`Team` <- team_vector
          message('---Created list generation table for VA')
          # Save for use in other places
          session_data$va_table <- va
        } else {
          session_data$va_table <- data.frame()
        }
      } else {
        session_data$va_table <- data.frame()
      }
    } else {
      session_data$va_table <- data.frame()
    }
    
  })
  
  # VA list generation table
  output$table_va_list_generation <- DT::renderDataTable({
    # x <- input$va_n_teams
    va <- session_data$va_table
    if(nrow(va) > 0){
      # Make datatable
      final <- DT::datatable(va, 
                             options = list(pageLength = nrow(va), 
                                            dom = "Bfrtip", buttons = list("copy", "print", 
                                                                           list(extend = "collection", buttons = "csv", 
                                                                                text = "Download"))), rownames = FALSE, extensions = "Buttons") %>%
        formatStyle(names(va)[1:6],
                    backgroundColor = '#ffcccb') %>%
        formatStyle(names(va)[7:10],
                    backgroundColor = '#fed8b1') %>%
        formatStyle(names(va)[11:(ncol(va))],
                    backgroundColor = '#ADD8E6')
    } else {
      final <- DT::datatable(data.frame(`O` = 'No data'))
    }
    
    return(final)
  },
  options = list(scrollX = TRUE))
  # VA List Generation UI
  output$ui_va_list_generation <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              pd <- session_data$va_table
              any_va <- nrow(pd) > 0
              update_va_button <- actionButton('update_va_sheet',
                                               'Update VA sheet')
              if(any_va){
                fluidPage(
                  update_va_button, br(),
                  column(12, align = 'center',
                         downloadButton('render_control_sheet',
                                        'Print VA control sheet'),
                         br(), br(),
                         div(DT::dataTableOutput('table_va_list_generation'), style = "font-size:60%")
                  )
                )
              } else {
                fluidPage(
                  column(12, align = 'center',
                         update_va_button, br(),
                         textOutput('va_text'),
                  ))
              }
            })
  })
  
  
  # VA geo monitoring UI  #############################################
  va_monitoring_geo <- reactiveVal('Ward')
  output$ui_va_monitoring_by <- renderUI({
    
    cn <- input$geo
    if(cn=='Rufiji'){
      cn_choices = c('District',
                     'Ward',
                     'Village',
                     'Hamlet')
    } else {
      cn_choices = c('Distrito' = 'District',
                     'Posto administrativo/localidade' ='Ward',
                     'Povoado' ='Village',
                     'Bairro' ='Hamlet')
    }
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              
              fmg <- va_monitoring_geo()
              fluidPage(
                fluidRow(
                  column(12, align = 'center',
                         radioButtons('va_monitor_by',
                                      'Geographic level:',
                                      choices = cn_choices,
                                      selected = fmg,
                                      inline = TRUE))
                )
              )
            })
  })
  
  # ui for va overall progress
  output$ui_va_overall_progress <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              
              # Get the overall va progress table
              pd <- odk_data$data
              pd <- pd$minicensus_main
              co <- country()
              pd <- pd %>% 
                filter(hh_country == co)
              deaths <- odk_data$data$minicensus_repeat_death_info
              # save(deaths, file = '/tmp/deaths.RData')
              deaths <- deaths %>% filter(instance_id %in% pd$instance_id,
                                          !is.na(death_number))
              pd <- pd %>%
                dplyr::select(district = hh_district,
                              ward = hh_ward,
                              village = hh_village,
                              hamlet = hh_hamlet, instance_id)
              # # Get VA info
              va <- odk_data$data$va
              # save(va, pd, co, deaths, file = '/tmp/tmp.RData')
              
              # grouper <- 'district'
              
              va_progress <- deaths %>%
                left_join(pd) %>%
                # filter(!is.na(hamlet)) %>%
                # group_by_(grouper) %>%
                summarise(`VA forms collected` = nrow(va),
                          `Deaths reported` = n()) %>%
                mutate(`% VA forms completed` = round(`VA forms collected` / 
                                                        `Deaths reported` * 100)) %>%
                select(`VA forms collected`, `Deaths reported`, `% VA forms completed`)
              
              fluidPage(
                fluidRow(
                  h2('Overall progress table'),
                  prettify(va_progress)
                )
              )
            })
  })
  
  # ui for VA progress by geography.
  output$va_progress_geo_ui <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              
              # Get the overall va progress table
              pd <- odk_data$data
              va <- pd$va
              deaths <- pd$minicensus_repeat_death_info
              pd <- pd$minicensus_main
              co <- country()
              # save(pd, va, deaths, file = '/tmp/va.RData')
              pd <- pd %>%
                filter(hh_country == co)
              
              deaths <- deaths %>% filter(instance_id %in% pd$instance_id,
                                          !is.na(death_number))
              pd <- pd %>%
                dplyr::select(district = hh_district,
                              ward = hh_ward,
                              village = hh_village,
                              hamlet = hh_hamlet, instance_id)
              
              grouper <- input$va_monitor_by
              # save(grouper, file = '/tmp/grouper.RData')
              
              # Get location in va
              va <- va %>%
                mutate(code = substr(hh_id, 1, 3)) %>%
                left_join(locations %>% dplyr::select(code, region = Region, district = District,
                                                      ward = Ward, village = Village, hamlet = Hamlet))
              
              
              if(is.null(grouper)){
                grouper <- 'district'
              } else {
                grouper <- tolower(grouper)
              }
              va_agg <- va %>%
                group_by_(grouper) %>%
                summarise(`VA forms collected` = n())
              
              va_progress_geo <- deaths %>%
                left_join(pd) %>%
                # filter(!is.na(hamlet)) %>%
                group_by_(grouper) %>%
                summarise(`Deaths reported` = n()) %>%
                full_join(va_agg) %>%
                mutate(`VA forms collected` = ifelse(is.na(`VA forms collected`), 0, `VA forms collected`)) %>%
                mutate(`Deaths reported` = ifelse(is.na(`Deaths reported`), 0, `Deaths reported`)) %>%
                mutate(`% VA forms completed` = round(`VA forms collected` /
                                                        `Deaths reported` * 100))
              message('---Created progess table for VA by geography')
              
              
              if(co == 'Mozambique'){
                if(grouper=='district'){
                  names(va_progress_geo)[1] <- 'Distrito'
                } else if(grouper=='ward'){
                  names(va_progress_geo)[1] <- 'Posto administrativo/localidade'
                } else if (grouper=='hamlet'){
                  names(va_progress_geo)[1] <- 'Bairro'
                } else if(grouper =='village'){
                  names(va_progress_geo)[1] <- 'Povaodo'
                }
              }
              # save(va_progress_geo, file='temp_va.rda')
              fluidPage(
                fluidRow(
                  h2(paste0('Progress by ',  names(va_progress_geo)[1])),
                  prettify(va_progress_geo)
                )
              )
            })
  })
  
  # ui for VA progress by geography.
  output$va_progress_geo_plot_ui <- renderPlot({
    
    # Get the overall va progress table
    pd <- odk_data$data
    va <- pd$va
    deaths <- pd$minicensus_repeat_death_info
    pd <- pd$minicensus_main
    co <- country()
    # save(pd, va, deaths, co, file = '/tmp/vajoe.RData')
    pd <- pd %>%
      filter(hh_country == co)
    
    deaths <- deaths %>% filter(instance_id %in% pd$instance_id,
                                !is.na(death_number))
    pd <- pd %>%
      dplyr::select(district = hh_district,
                    ward = hh_ward,
                    village = hh_village,
                    hamlet = hh_hamlet, instance_id)
    
    grouper <- input$va_monitor_by
    # save(grouper, file = '/tmp/grouper.RData')
    
    # Get location in va
    va <- va %>%
      mutate(code = substr(hh_id, 1, 3)) %>%
      left_join(locations %>% dplyr::select(code, region = Region, district = District,
                                            ward = Ward, village = Village, hamlet = Hamlet))
    
    
    if(is.null(grouper)){
      grouper <- 'district'
    } else {
      grouper <- tolower(grouper)
    }
    va_agg <- va %>%
      group_by_(grouper) %>%
      summarise(`VA forms collected` = n())
    
    va_progress_geo <- deaths %>%
      left_join(pd) %>%
      # filter(!is.na(hamlet)) %>%
      group_by_(grouper) %>%
      summarise(`Deaths reported` = n()) %>%
      full_join(va_agg) %>%
      mutate(`VA forms collected` = ifelse(is.na(`VA forms collected`), 0, `VA forms collected`)) %>%
      mutate(`Deaths reported` = ifelse(is.na(`Deaths reported`), 0, `Deaths reported`)) %>%
      mutate(`% VA forms completed` = round(`VA forms collected` /
                                              `Deaths reported` * 100))
    message('---Created progess table for VA by geography')
    
    if(co == 'Mozambique'){
      if(grouper=='district'){
        names(va_progress_geo)[1] <- 'Distrito'
      } else if(grouper=='ward'){
        names(va_progress_geo)[1] <- 'Posto administrativo/localidade'
      } else if (grouper=='hamlet'){
        names(va_progress_geo)[1] <- 'Bairro'
      } else if(grouper =='village'){
        names(va_progress_geo)[1] <- 'Povaodo'
      }
    }
    names(va_progress_geo)[1] <- 'location'
    # save(va_progress_geo, file = '/tmp/va_progress_geo.RData')
    
    va_progress_geo <- va_progress_geo %>%
      mutate(`% VA forms pending` = 100 - `% VA forms completed`) %>%
      tidyr::gather(key=key, value=value, -c(location))
    
    plot <- ggplot(data = va_progress_geo %>%
                     filter(grepl('%', key)),
                   aes(x = "",
                       y = value,
                       fill = key)) +
      geom_bar(stat = 'identity', width = 1) +
      theme_void() +
      coord_polar("y", start=0) +
      # geom_text(data = va_progress_geo %>%
      #             filter(!grepl('%', key)),
      #           aes(y = value, label = value, fill = NA, color = NA), color = "white", size=6) +
      facet_wrap(~location) +
      theme(legend.position = 'bottom') +
      scale_fill_manual(name='', values = c('lightblue', 'darkorange')) 
    # plot <- ggplot(va_progress_geo, aes(reorder(location, -value), value, fill=key)) + geom_bar(stat = 'identity', alpha=0.6, width = 1) +
    #   coord_polar() +
    #   labs(x='', y ='Number of forms', title = 'Deaths reported vs forms collected') +
    #   scale_fill_manual(name='', values = c('darkred', 'darkblue')) +
    #   theme_bohemia() +
    #   theme(axis.text = element_text(size = 10, angle=90, vjust=0.5),
    #         axis.title = element_text(size = 14),
    #         plot.title = element_text(size = 20))
    
    return(plot)
    
  })
  
  output$va_progress_geo_render_plot_ui <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              plotOutput('va_progress_geo_plot_ui', height = '700px')
            })
  })
  
  
  output$plot_va_progress_by_geo_past_due <- renderPlot({
    # Get the odk data
    pd <- odk_data$data
    pd <- pd$minicensus_main
    cn <- country()
    pd <- pd %>% filter(hh_country == cn)
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){
      # va table
      
      deaths <- odk_data$data$minicensus_repeat_death_info
      deaths <- deaths %>% filter(instance_id %in% pd$instance_id)
      va <- odk_data$data$va
      # save(pd, cn, deaths, va, file = '/tmp/vajoe.RData')
      # Conditional mourning period
      mourning_period <- ifelse(cn == 'Mozambique', 30, 40)
      out <- left_join(deaths %>% 
                         left_join(pd %>% dplyr::select(instance_id, todays_date), by = 'instance_id') %>%
                         mutate(todays_date = as.Date(todays_date),
                                death_dod = as.Date(death_dod)) %>%
                         mutate(old = (todays_date - death_dod) > mourning_period) %>%
                         mutate(time_to_add = ifelse(old, 7, mourning_period)) %>%
                         mutate(xx = todays_date + time_to_add#, # this needs to be 7 days after hh visit date if death was <40 days prior to hh visit date | 40 days after hh visit date if the death was >40 days after hh visit date
                                # yy = Sys.Date() - todays_date
                         ) %>%
                         # Note: in case the "date of death" is unknown (the form has that option): let's just calculate the "latest date to do VA" by adding 40 days (Tanzania) and 30 days (Moz) to the "date of the hh visit", to be safe.
                         mutate(safe_bet = todays_date + mourning_period) %>%
                         mutate(xx = ifelse(is.na(xx), safe_bet, xx)) %>%
                         mutate(xx = as.Date(xx, origin = '1970-01-01')) %>%
                         mutate(yy = Sys.Date() - xx) %>% # "time elapsed" means time between lastest date to collect and today
                         dplyr::select(instance_id,
                                       `Date of death` = death_dod,
                                       `Latest date to collect VA form` = xx,
                                       `PERM ID` = death_id,
                                       `Time elapsed` = yy),
                       pd %>%
                         dplyr::select(instance_id,
                                       District = hh_district,
                                       Ward = hh_ward,
                                       Village = hh_village,
                                       Hamlet = hh_hamlet,
                                       `HH ID` = hh_id,
                                       `FW ID` = wid,
                                       `HH visit date` = todays_date), by = 'instance_id') %>%
        # mutate(`FW ID` = ' ') %>%
        dplyr::select(-instance_id)
      
      # Remove those which have already been done
      out <- out %>%
        filter(!`PERM ID` %in% va$death_id)
      
      if(nrow(out) > 0){
        out <- out %>% dplyr::select(District, Ward, Village, Hamlet,
                                     `HH ID`, `FW ID`, `PERM ID`,
                                     `HH visit date`, `Date of death`,
                                     `Latest date to collect VA form`,
                                     `Time elapsed`
        ) %>%
          arrange(desc(`HH visit date`))
        if(cn=='Mozambique'){
          out <- out %>%  rename(Distrito = District,
                                 `Posto administrativo/localidade`=Ward,
                                 Povoado=Village,
                                 Bairro=Hamlet,
                                 `FW ID (minicensus)` = `FW ID`) 
        } else {
          out <- out %>%
            left_join(
              bohemia::tza_ward_supervisors %>%
                dplyr::mutate(wid = paste0(Supervisor_Name, ' (',
                                           wid, ')')) %>%
                dplyr::select(Ward, wid),
              by = 'Ward'
            ) %>%
            mutate(`FW ID` = wid) %>%
            dplyr::select(-wid) %>%
            dplyr::rename(`Assigned to`  = `FW ID`) 
        }
        grouper <- input$va_monitor_by
        
        if(is.null(grouper)){
          if(co=='Mozambique'){}
          grouper <- 'ward'
        } else {
          grouper <-tolower(grouper)
        }
        out$`Time elapsed` <- NULL
        if(cn=='Mozambique'){
          grouper <- ifelse(grouper == 'ward', '`Posto administrativo/localidade`', 
                            ifelse(grouper =='district', 'Distrito',
                                   ifelse(grouper=='village', 'Povoado', 'Bairro')))
          plot_data <- out %>% group_by_(grouper) %>% summarise(counts=n())
          names(plot_data)[1] <- 'V1'
          plot <- ggplot(plot_data, aes(reorder(V1, -counts), counts)) + geom_bar(stat='identity') +
            labs(x = '',
                 y='# of forms', title = 'VAs past due') +
            theme_bohemia() +
            theme(axis.text = element_text(size = 14, angle=90, vjust=0.5),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 20))
          
          return(plot)
        } else {
          out$district <- paste0(out$District, ' - ', out$`Assigned to`)
          out$ward <- paste0(out$Ward, ' - ', out$`Assigned to`)
          out$village <- paste0(out$Village, ' - ', out$`Assigned to`)
          out$hamlet<- paste0(out$Hamlet, ' - ', out$`Assigned to`)
          
          plot_data <- out %>% group_by_(grouper) %>% summarise(counts=n())
          names(plot_data)[1] <- 'V1'
          plot <- ggplot(plot_data, aes(reorder(V1, -counts), counts)) + geom_bar(stat='identity') +
            labs(x = '',
                 y='# of forms', title = 'VAs past due') +
            theme_bohemia() +
            theme(axis.text.x = element_text(angle=90, vjust=0.5, size = sizex)) +
            theme(axis.text = element_text(size = sizex, angle=90, vjust=0.5),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 20))
          
          return(plot)
        }
        
      } 
    } else {
      return(NULL)
    }
  })
  output$va_progress_geo_past_due_ui <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              plotOutput('plot_va_progress_by_geo_past_due', height = '700px')
            })
  })
  
  output$table_va_progress_past_due <- DT::renderDataTable({
    # Get the odk data
    pd <- odk_data$data
    pd <- pd$minicensus_main
    cn <- country()
    pd <- pd %>% filter(hh_country == cn)
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){
      # va table
      
      deaths <- odk_data$data$minicensus_repeat_death_info
      deaths <- deaths %>% filter(instance_id %in% pd$instance_id)
      va <- odk_data$data$va
      # save(pd, cn, deaths, va, file = '/tmp/vajoe.RData')
      # Conditional mourning period
      mourning_period <- ifelse(cn == 'Mozambique', 30, 40)
      out <- left_join(deaths %>% 
                         left_join(pd %>% dplyr::select(instance_id, todays_date), by = 'instance_id') %>%
                         mutate(todays_date = as.Date(todays_date),
                                death_dod = as.Date(death_dod)) %>%
                         mutate(old = (todays_date - death_dod) > mourning_period) %>%
                         mutate(time_to_add = ifelse(old, 7, mourning_period)) %>%
                         mutate(xx = todays_date + time_to_add#, # this needs to be 7 days after hh visit date if death was <40 days prior to hh visit date | 40 days after hh visit date if the death was >40 days after hh visit date
                                # yy = Sys.Date() - todays_date
                         ) %>%
                         # Note: in case the "date of death" is unknown (the form has that option): let's just calculate the "latest date to do VA" by adding 40 days (Tanzania) and 30 days (Moz) to the "date of the hh visit", to be safe.
                         mutate(safe_bet = todays_date + mourning_period) %>%
                         mutate(xx = ifelse(is.na(xx), safe_bet, xx)) %>%
                         mutate(xx = as.Date(xx, origin = '1970-01-01')) %>%
                         mutate(yy = Sys.Date() - xx) %>% # "time elapsed" means time between lastest date to collect and today
                         dplyr::select(instance_id,
                                       `Date of death` = death_dod,
                                       `Latest date to collect VA form` = xx,
                                       `PERM ID` = death_id,
                                       `Time elapsed` = yy),
                       pd %>%
                         dplyr::select(instance_id,
                                       District = hh_district,
                                       Ward = hh_ward,
                                       Village = hh_village,
                                       Hamlet = hh_hamlet,
                                       `HH ID` = hh_id,
                                       `FW ID` = wid,
                                       `HH visit date` = todays_date), by = 'instance_id') %>%
        # mutate(`FW ID` = ' ') %>%
        dplyr::select(-instance_id)
      
      # Remove those which have already been done
      out <- out %>%
        filter(!`PERM ID` %in% va$death_id)
      
      if(nrow(out) > 0){
        out <- out %>% dplyr::select(District, Ward, Village, Hamlet,
                                     `HH ID`, `FW ID`, `PERM ID`,
                                     `HH visit date`, `Date of death`,
                                     `Latest date to collect VA form`,
                                     `Time elapsed`
        ) %>%
          arrange(desc(`HH visit date`))
        if(cn=='Mozambique'){
          out <- out %>%  rename(Distrito = District,
                                 `Posto administrativo/localidade`=Ward,
                                 Povoado=Village,
                                 Bairro=Hamlet,
                                 `FW ID (minicensus)` = `FW ID`) 
        } else {
          out <- out %>%
            left_join(
              bohemia::tza_ward_supervisors %>%
                dplyr::mutate(wid = paste0(Supervisor_Name, ' (',
                                           wid, ')')) %>%
                dplyr::select(Ward, wid),
              by = 'Ward'
            ) %>%
            mutate(`FW ID` = wid) %>%
            dplyr::select(-wid) %>%
            dplyr::rename(`Assigned to`  = `FW ID`) 
        }
        out$`Time elapsed` <- NULL
        print(head(out))        
        message('---Created past due table for VA')
        out <- out %>% filter(`Latest date to collect VA form` <= Sys.Date())
        return(bohemia::prettify(out, nrows = nrow(out), download_options = TRUE))
      } 
    } else {
      return(NULL)
    }
  })
  output$ui_va_progress_past_due <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              DT::dataTableOutput('table_va_progress_past_due')
            })
  })
  # 
  
  # percent complete map input UI  #############################################
  # map_complete_geo<- reactiveVal('Hamlet')
  # output$ui_map_complete_by <- renderUI({
  #   
  #   cn <- input$geo
  #   if(cn=='Rufiji'){
  #     cn_choices = c('Ward',
  #                    'Village',
  #                    'Hamlet')
  #   } else {
  #     cn_choices = c('Posto administrativo/localidade' ='Ward',
  #                    'Povoado' ='Village',
  #                    'Bairro' ='Hamlet')
  #   }
  #   # See if the user is logged in and has access
  #   si <- session_info
  #   li <- si$logged_in
  #   ac <- 'field_monitoring' %in% si$access
  #   # Generate the ui
  #   make_ui(li = li,
  #           ac = ac,
  #           ok = {
  #             mcg <- map_complete_geo()
  #             
  #             fluidPage(
  #               column(6,
  #                      selectInput('map_complete_by',
  #                                   'Geographic level for map:',
  #                                   choices = cn_choices,
  #                                   selected = mcg))
  #             )
  #           })
  # })
  observeEvent(input$field_monitor_by,{
    x <- input$field_monitor_by
    field_monitoring_geo(x)
    
  })
  
  # Table of drop outs for fieldworkers
  output$drop_out_ui <- renderUI({
    options(dplyr.summarise.inform = FALSE)
    
    # Get the fieldworkers for the country in question
    co <- country()
    sub_fids <- fids %>% filter(country == co)
    # Get the minicensus data for the fieldworkers in question
    pd <- odk_data$data
    pd <- pd$minicensus_main
    pd$todays_date <- as.Date(pd$todays_date)
    # Detect all "drop outs" for each fieldworker
    pd_ids <- sort(unique(pd$wid))
    out_list <- list()
    for(i in 1:length(pd_ids)){
      this_pd <- pd %>% filter(wid == pd_ids[i])
      right <- this_pd %>%
        group_by(date = todays_date) %>%
        tally
      left <- tibble(date = seq(from = min(this_pd$todays_date, na.rm = TRUE),
                                to = Sys.Date(), #max(this_pd$todays_date, na.rm = TRUE),
                                by = 1)) %>%
        mutate(wd = weekdays(date))
      joined <- left_join(left, right, by = 'date')
      joined <- joined %>% filter(!wd %in% c('Saturday', 'Sunday'))
      # Identify episodes of non-activity
      joined$roller <- ave(joined$n, cumsum(!is.na(joined$n)), FUN = seq_along) - 1
      joined$episode_start <- ifelse(joined$roller == 1, 1, 0)
      joined$episode <- cumsum(joined$episode_start)
      joined$episode <- ifelse(!is.na(joined$n), NA, joined$episode)
      min_date <- min(joined$date)
      out <- joined %>%
        filter(is.na(n)) %>%
        group_by(episode) %>%
        summarise(n_days = max(roller),
                  start_date = min(date),
                  end_date = max(date)) %>%
        ungroup %>%
        mutate(period = paste0(format(start_date, '%b %d'), '-', format(end_date, '%b %d'))) %>%
        mutate(start_working = min_date) %>%
        mutate(fwid =  pd_ids[i]) %>%
        dplyr::select(`FW ID` = fwid,
                      `FW start date` = start_working,
                      `Drop-out episode number` = episode,
                      `Days missing` = n_days,
                      Dates = period)
      out_list[[i]] <- out
    }
    out <- bind_rows(out_list)
    out <- left_join(out, 
                     sub_fids %>% dplyr::select(`FW ID` = bohemia_id,
                                                Supervisor = supervisor))
    
    fluidPage(
      h2('Drop-outs table'),
      bohemia::prettify(out)
    )
  })
  
  # create an input for fw daily table
  output$ui_fw_time_period <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              # pd <- odk_data$data
              # pd <- pd$minicensus_main
              # co <- country()
              # # save(pd, file = '/tmp/pd.RData')
              # pd <- pd %>% filter(hh_country == co)
              # 
              start_at <- as.Date('2020-09-20')
              end_at <- Sys.Date()
              # fluidPage(
              fluidRow(
                #   column(12,
                sliderInput(inputId = 'fw_time_period', 
                            label = 'Previous days to include:', 
                            min = start_at, 
                            max=end_at, 
                            value = c(start_at, end_at)))
              #   )
              # ) 
            })
  })
  
  
  
  output$ui_fw_plot <- renderPlot({
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              # Get the odk data
              pd <- odk_data$data
              an <- session_data$anomalies
              
              co <- country()
              enumerations <- pd$enumerations
              va <- pd$va
              pd <- pd$minicensus_main
              # save(pd, co, an,enumerations, va, file = '/tmp/pd.RData')
              
              if(nrow(an) > 0){
                an$date <- as.Date(an$date, format = '%Y-%m-%d')
              }
              
              pd_ok <- FALSE
              if(!is.null(pd)){
                if(nrow(pd) > 0){
                  pd_ok <- TRUE
                }
              }
              if(pd_ok){
                # Date filters
                min_date <- as.Date('2020-09-20')   
                max_date <- Sys.Date() 
                time_period <- input$fw_time_period
                if(is.null(time_period)){
                  time_period <- c(min_date,
                                   max_date)
                } 
                pd <- pd %>%
                  filter(todays_date >= time_period[1],
                         todays_date <= time_period[2])
                va <- va %>%
                  filter(todays_date >= time_period[1],
                         todays_date <= time_period[2])
                enumerations <- enumerations %>%
                  filter(todays_date >= time_period[1],
                         todays_date <= time_period[2])
                
                # get daily target
                if(co=='Mozambique'){
                  fw_daily_target <- 10
                } else {
                  fw_daily_target <- 13
                }
                # Get the fieldworkers for the country in question
                sub_fids <- fids %>% filter(country == co)
                left <- tibble(wid = sort(unique(as.character(sub_fids$bohemia_id))))
                
                # Join to minicensus
                pd <- left %>%
                  left_join(
                    pd %>%
                      group_by(wid = as.character(wid)) %>%
                      summarise(minicensus = n(),
                                minicensus_days = length(unique(todays_date)))
                  )
                
                # Join to enumerations forms and days
                pd <- pd %>% 
                  left_join(
                    enumerations %>% mutate(wid = as.character(wid)) %>%
                      group_by(wid) %>%
                      summarise(enumerations = n(),
                                enumerations_days = length(unique(todays_date)))
                  )
                # Join to VA
                pd <- pd %>% 
                  left_join(
                    va %>% mutate(wid = as.character(wid)) %>%
                      group_by(wid) %>%
                      summarise(va = n(),
                                va_days = length(unique(todays_date)))
                  )
                
                # Restructure
                ui_fw_plot_form <- input$ui_fw_plot_form
                if(is.null(ui_fw_plot_form)){
                  ui_fw_plot_form <- 'Minicensus'
                }
                pdx <- pd %>%
                  mutate(wid = as.numeric(wid)) 
                
                if(ui_fw_plot_form == 'Minicensus'){
                  ui_show_percent <- input$ui_show_percent
                  if(is.null(ui_show_percent)){
                    ui_show_percent <- FALSE
                  } 
                  if(ui_show_percent){
                    pdx <- pdx %>%
                      mutate(forms = minicensus,
                             days = minicensus_days)
                    pdx <- pdx %>%
                      mutate(per_day = forms / days)
                    pdx <- pdx %>% filter(!is.na(forms))
                    pdx <- pdx %>% arrange(desc(per_day))
                    pdx$wid <- factor(pdx$wid, levels = unique(pdx$wid))
                    # save(pdx, fids, file = '/tmp/dec2.RData')
                    right <- fids %>%
                      mutate(wid = bohemia_id) %>%
                      dplyr::select(wid, Role, first_name, last_name) %>%
                      mutate(wid = as.numeric(as.character(wid)))
                    pdx <- left_join(
                      pdx %>% mutate(wid = as.numeric(as.character(wid))),
                      right)%>%
                      mutate(wid = paste0(wid,' ', first_name, ' ', last_name, ' (', Role, ')'))
                    # add denominator for daily target
                    pdx$daily_target <- fw_daily_target
                    pdx$percent_of_daily_target <- round((pdx$per_day/pdx$daily_target)*100, 2)
                    # create color vector for manual legend
                    colors <- c('Number of forms' ='blue','Working days'='red')
                    p <- ggplot(data = pdx,
                                aes(x = wid,
                                    y = percent_of_daily_target)) +
                      geom_bar(stat = 'identity', alpha=0.6) +
                      labs(x = 'FW',
                           y = '% of daily target') +
                      theme_bohemia() +
                      geom_text(aes(label = forms, color='Number of forms'),
                                nudge_y = 0.5, size = 5, alpha = 0.8, angle=90) +
                      geom_text(aes(label = days, color='Working days'),
                                nudge_y = -0.5, size = 5, alpha = 0.8,angle = 90) +
                      scale_color_manual(name ='', 
                                         values =colors) +
                      theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5, size = sizex),
                            legend.position = 'top',
                            legend.text = element_text(size = 18))
                    p
                    return(p)
                  } else {
                    pdx <- pdx %>%
                      mutate(forms = minicensus,
                             days = minicensus_days)
                  }
                  
                } else if(ui_fw_plot_form == 'Enumerations'){
                  pdx <- pdx %>%
                    mutate(forms = enumerations,
                           days = enumerations_days)
                } else if(ui_fw_plot_form == 'VA'){
                  pdx <- pdx %>%
                    mutate(forms = va,
                           days = va_days)
                }  
                pdx <- pdx %>%
                  mutate(per_day = forms / days) 
                pdx <- pdx %>% filter(!is.na(forms))
                pdx <- pdx %>% arrange(desc(per_day))
                pdx$wid <- factor(pdx$wid, levels = unique(pdx$wid))
                # save(pdx, fids, file = '/tmp/dec2.RData')
                right <- fids %>%
                  mutate(wid = bohemia_id) %>%
                  dplyr::select(wid, Role, first_name, last_name) %>%
                  mutate(wid = as.numeric(as.character(wid)))
                pdx <- left_join(
                  pdx %>% mutate(wid = as.numeric(as.character(wid))),
                  right)%>%
                  mutate(wid = paste0(wid,' ', first_name, ' ', last_name, ' (', Role, ')'))
                
                # remove NA in parantheses
                pdx <- pdx %>% filter(!grepl('NA', wid))
                colors <- c('Number of forms' ='blue','Working days'='red')
                
                if(nrow(pdx)==0){
                  NULL
                } else {
                  # save(pdx, file = '/tmp/pdx.RData')
                  ggplot(data = pdx,
                         aes(x = reorder(wid, -per_day),
                             y = per_day)) +
                    geom_bar(stat = 'identity',
                             alpha = 0.6) +
                    labs(x = 'FW',
                         y = 'Forms per (working) day') +
                    theme_bohemia() +
                    geom_text(aes(label = forms, color='Number of forms'),
                              nudge_y = max(pdx$per_day, na.rm = TRUE) * 0.1, size = sizex * 0.3, alpha = 0.8, angle = 90) +
                    geom_text(aes(label = days, color='Working days'),
                              nudge_y = max(pdx$per_day, na.rm = TRUE) * -0.1, size = sizex * 0.3, alpha = 0.8, angle = 90) +
                    scale_color_manual(name ='', 
                                       values =colors) +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = sizex)) +
                    theme(axis.text = element_text(size = 12),
                          axis.title = element_text(size = 14),
                          plot.title = element_text(size = 20),
                          legend.position = 'top',
                          legend.text = element_text(size = 16))
                }
                
                
              } else {
                NULL
              }
              
            })
  })
  
  output$ui_fw_plot2 <- renderPlot({
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              # Get the odk data
              pd <- odk_data$data
              an <- session_data$anomalies
              
              co <- country()
              enumerations <- pd$enumerations
              va <- pd$va
              pd <- pd$minicensus_main
              # save(pd, co, an,enumerations, va, file = '/tmp/pd.RData')
              
              if(nrow(an) > 0){
                an$date <- as.Date(an$date, format = '%Y-%m-%d')
              }
              
              pd_ok <- FALSE
              if(!is.null(pd)){
                if(nrow(pd) > 0){
                  pd_ok <- TRUE
                }
              }
              if(pd_ok){
                
                # Date filters
                min_date <- as.Date('2020-09-20')   
                max_date <- Sys.Date() 
                time_period <- input$fw_time_period
                if(is.null(time_period)){
                  time_period <- c(min_date,
                                   max_date)
                } 
                pd <- pd %>%
                  filter(todays_date >= time_period[1],
                         todays_date <= time_period[2])
                va <- va %>%
                  filter(todays_date >= time_period[1],
                         todays_date <= time_period[2])
                enumerations <- enumerations %>%
                  filter(todays_date >= time_period[1],
                         todays_date <= time_period[2])
                
                left <- tibble(todays_date =seq(time_period[1],
                                                time_period[2],
                                                by = 1))
                
                # Join to minicensus
                pd <- left %>%
                  left_join(
                    pd %>%
                      group_by(todays_date) %>%
                      summarise(minicensus = n(),
                                minicensus_workers = length(unique(wid)))
                  )
                
                # Join to enumerations forms and days
                pd <- pd %>% 
                  left_join(
                    enumerations %>% 
                      group_by(todays_date) %>%
                      summarise(enumerations = n(),
                                enumerations_workers = length(unique(wid)))
                  )
                # Join to VA
                pd <- pd %>% 
                  left_join(
                    va %>% 
                      group_by(todays_date) %>%
                      summarise(va = n(),
                                va_workers = length(unique(wid)))
                  )
                
                
                # Restructure
                ui_fw_plot_form <- input$ui_fw_plot_form
                if(is.null(ui_fw_plot_form)){
                  ui_fw_plot_form <- 'Minicensus'
                }
                pdx <- pd
                
                if(ui_fw_plot_form == 'Minicensus'){
                  pdx <- pdx %>%
                    mutate(forms = minicensus,
                           workers = minicensus_workers)
                } else if(ui_fw_plot_form == 'Enumerations'){
                  pdx <- pdx %>%
                    mutate(forms = enumerations,
                           workers = enumerations_workers)
                } else if(ui_fw_plot_form == 'VA'){
                  pdx <- pdx %>%
                    mutate(forms = va,
                           workers = va_workers)
                }  
                pdx <- pdx %>%
                  mutate(per_worker = forms / workers) %>%
                  mutate(label = paste0(forms, '    ',
                                        workers, ''))
                pdx <- pdx %>% filter(!is.na(forms))
                pdx <- pdx %>% arrange(desc(per_worker))
                ggplot(data = pdx,
                       aes(x = reorder(todays_date, -per_worker),
                           y = per_worker)) +
                  geom_bar(stat = 'identity') +
                  labs(x = 'FW',
                       y = 'Forms per worker',
                       caption = 'Numbers show forms on top, workers that day on bottom. A "working day" is a day on which that worker submitted any form of that type.') +
                  theme_bohemia() +
                  geom_text(aes(label = label),
                            nudge_y = 0, size = 5, alpha = 0.8,
                            angle = 90) +
                  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5, size = sizex)) +
                  theme(axis.text = element_text(size = 12),
                        axis.title = element_text(size = 14),
                        plot.title = element_text(size = 20),
                        plot.caption = element_text(size = 14))
                
              } else {
                NULL
              }
              
            })
    
  })
  
  
  output$ui_fw_table <- DT::renderDataTable({
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              # Get the odk data
              pd <- odk_data$data
              an <- session_data$anomalies
              
              co <- country()
              enumerations <- pd$enumerations
              full_va <- va <- pd$va
              full_pd <- pd <- pd$minicensus_main
              # save(pd, co, an,enumerations, va, file = '/tmp/pd.RData')
              
              # pd <- pd %>% filter(hh_country == co)
              
              min_date <- as.Date('2020-09-20')   #min(pd$todays_date, na.rm = TRUE)
              max_date <- Sys.Date() #  max(pd$todays_date, na.rm = TRUE)
              seq_days <- seq(min_date, max_date, by = 1)
              seq_days <- seq_days[!weekdays(seq_days) %in% c('Saturday', 'Sunday')]
              n_days <- length(seq_days)
              # get anomaly dataset
              if(nrow(an) > 0){
                an$date <- as.Date(an$date, format = '%Y-%m-%d')
              }
              # save(an, enumerations, pd, va, min_date, max_date, n_days, co, file='/tmp/temp_an.rda')
              
              pd_ok <- FALSE
              if(!is.null(pd)){
                if(nrow(pd) > 0){
                  pd_ok <- TRUE
                }
              }
              if(pd_ok){
                # join fids with pd to ge supervisor info
                # Get the fieldworkers for the country in question
                sub_fids <- fids %>% filter(country == co)
                pd <- left_join(pd %>%
                                  mutate(wid = as.character(wid)), 
                                sub_fids %>%
                                  mutate(bohemia_id = as.character(bohemia_id)),
                                by=c('wid'= 'bohemia_id')) 
                pd <- pd %>% filter(!is.na(country)) %>%
                  filter(country == co) %>%
                  mutate(end_time = lubridate::as_datetime(end_time),
                         start_time = lubridate::as_datetime(start_time)) %>%
                  mutate(time = end_time - start_time)
                
                if(co=='Mozambique'){
                  daily_forms_fw <- 10
                  weekly_forms_fw <- daily_forms_fw*5
                  total_forms_fw <- 500
                } else {
                  daily_forms_fw <- 13
                  weekly_forms_fw <- daily_forms_fw*5
                  total_forms_fw <- 599
                }
                
                time_period <- input$fw_time_period
                if(is.null(time_period)){
                  time_period <- c(min_date,
                                   max_date)
                } 
                time_range <- time_period
                
                seq_days <- seq(min(time_period), max(time_period), by = 1)
                seq_days <- seq_days[!weekdays(seq_days) %in% c('Saturday', 'Sunday')]
                n_days <- length(seq_days)
                n_days <- ifelse(n_days < 1, 1, n_days)
                
                an <- an %>% group_by(wid = as.character(wid)) %>% 
                  filter(date >= time_range[1],
                         date <=time_range[2]) %>%
                  summarise(num_errors = sum(type=='error'),
                            num_anomalies = sum(type == 'anomaly'))
                if(nrow(enumerations) > 0){
                  enumerations <- enumerations %>% 
                    filter(todays_date >= time_range[1],
                           todays_date <=time_range[2]) %>%
                    group_by(wid = as.character(wid)) %>%
                    summarise(num_enumerations = n())
                }
                if(nrow(va) > 0){
                  va <- va %>% 
                    filter(todays_date >= time_range[1],
                           todays_date <=time_range[2]) %>%
                    group_by(wid = as.character(wid)) %>%
                    summarise(num_va = n())
                }
                
                # Get all fieldworkers
                left <- 
                  sub_fids %>%
                  dplyr::rename(wid = bohemia_id) %>%
                  dplyr::select(-country) %>%
                  mutate(wid = as.character(wid))
                
                if(nrow(va) > 0){
                  left <- left %>% left_join(va, by = 'wid')
                } else {
                  left <- left %>% mutate(num_va = 0)
                }
                if(nrow(enumerations) > 0){
                  left <- left %>% left_join(enumerations, by = 'wid')
                } else {
                  left <- left %>% mutate(num_enumerations = 0)
                }
                if(nrow(an) > 0){
                  left <- left %>% left_join(an, by ='wid')
                } else {
                  left <- left %>% mutate(
                    num_anomalies = 0,
                    num_errors = 0
                  )
                }
                
                right <- 
                  pd %>%
                  mutate(fw_name = paste0(first_name, ' ', last_name)) %>%
                  mutate(todays_date = as.Date(todays_date)) %>%
                  mutate(end_time = lubridate::as_datetime(end_time)) %>%
                  filter(todays_date >= time_range[1],
                         todays_date <=time_range[2])%>%
                  group_by(wid = as.character(wid)) %>%
                  summarise(`Minicensus forms` = n(),
                            `Average time per minicensus form (minutes)` = round(mean(time, na.rm = TRUE), 1),
                            `% rolling target (minicensus)` = (`Minicensus forms` / n_days) / daily_forms_fw * 100) 
                make_na <- function(x){ifelse(is.na(x), 0, x)}
                if(co=='Mozambique'){
                  fwt_daily <-left_join(left, right) %>%
                    mutate(num_enumerations = make_na(num_enumerations),
                           num_va = make_na(num_va),
                           num_anomalies = make_na(num_anomalies),
                           num_errors = make_na(num_errors)) %>%
                    mutate(fw = paste0(first_name, ' ', last_name)) %>%
                    dplyr::select(`FW ID` = wid,
                                  `FW` = fw,
                                  Supervisor = supervisor,
                                  Role,
                                  `Minicensus forms`,
                                  `Average time per minicensus form (minutes)`,
                                  `% rolling target (minicensus)`,
                                  `Enumeration forms` = num_enumerations,
                                  `VA forms` = num_va,
                                  `Anomalies` = num_anomalies,
                                  `Errors` = num_errors) 
                  # remove rows that have 'NA NA' in FW column
                  fwt_daily <- fwt_daily %>% filter(!grepl('NA NA', fwt_daily$FW))
                  
                } else {
                  fwt_daily <-left_join(left, right) %>%
                    mutate(num_enumerations = make_na(num_enumerations),
                           num_va = make_na(num_va),
                           num_anomalies = make_na(num_anomalies),
                           num_errors = make_na(num_errors)) %>%
                    mutate(fw = paste0(first_name, ' ', last_name)) %>%
                    dplyr::select(`FW ID` = wid,
                                  `FW` = fw,
                                  Supervisor = supervisor,
                                  Role,
                                  `Minicensus forms`,
                                  `Average time per minicensus form (minutes)`,
                                  `% rolling target (minicensus)`,
                                  `Enumeration forms` = num_enumerations,
                                  `VA forms` = num_va,
                                  `Anomalies` = num_anomalies,
                                  `Errors` = num_errors) 
                  # remove rows that have 'NA NA' in FW column
                  fwt_daily <- fwt_daily %>% filter(!grepl('NA NA', fwt_daily$FW))
                  # save(fwt_daily, full_va, full_pd, file = '/tmp/joedec.RData')
                  # GET done by ward
                  # Get the overall va progress table
                  deaths <- full_pd$minicensus_repeat_death_info
                  
                  sups <- as.data.frame(bohemia::tza_ward_supervisors)
                  # Get location in va
                  va <- full_va %>%
                    mutate(code = substr(hh_id, 1, 3)) %>%
                    left_join(bohemia::locations %>% dplyr::select(code, region = Region, district = District,
                                                                   ward = Ward, village = Village, hamlet = Hamlet))
                  va <- dplyr::left_join(va, sups)
                  right <- va %>%
                    group_by(`FW ID` = as.character(wid)) %>%
                    summarise(`VAs assigned` = n())
                  fwt_daily <- left_join(fwt_daily,
                                         right) %>%
                    mutate(`VA forms done as % assigned` = round(`VA forms` / `VAs assigned` * 100, 2))
                }
                bohemia::prettify(fwt_daily, nrows = nrow(fwt_daily),
                                  download_options = TRUE)
                
              }
              
            })
  })
  
  output$ui_fw_daily <- renderUI({
    # See if the user is logged in and has access
    fluidPage(
      fluidRow(column(6,
                      selectInput('ui_fw_plot_form',
                                  'Form to show in chart',
                                  choices = c('Minicensus',
                                              'Enumerations', 
                                              'VA'),
                                  selected = 'Minicensus')),
               column(6,
                      checkboxInput(inputId = 'ui_show_percent', label = 'Show percentage on y-axis', value = FALSE))
      ),
      
      fluidRow(column(12,
                      tags$div(style='overflow-x: scroll; position: relative',plotOutput('ui_fw_plot', height = '700px', width = '2500px')))),
      fluidRow(column(12,
                      tags$div(style='overflow-x: scroll; position: relative',plotOutput('ui_fw_plot2', height = '700px', width = '2500px')))),
      fluidRow(column(12,
                      DT::dataTableOutput('ui_fw_table'))))
  })
  
  # Leaflet of fieldworkers
  output$fid_leaf <- renderLeaflet({
    NULL
  })
  observeEvent(input$fid, {
    xfids <- input$fid
    if(is.null(xfids)){
      xfids <- 1:700
    }
    xfids <- as.numeric(xfids)
    # Get the aggregate table # (fake)
    aggregate_table <- session_data$aggregate_table
    
    # Get the odk data
    pd <- odk_data$data
    pd <- pd$minicensus_main
    co <- country()
    # save(cpd, file = '/tmp/pd.RData')
    pd <- pd %>% filter(hh_country == co)
    
    pd_ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        pd_ok <- TRUE
      }
    }
    if(pd_ok){
      ll <- extract_ll(pd$hh_geo_location)
      pd$lng <- ll$lng; pd$lat <- ll$lat
      pd <- pd %>% filter(wid %in% xfids)
      # round table not here, and here make sure map works (not saving data below)
      leafletProxy('fid_leaf') %>%
        clearMarkers() %>%
        addMarkers(data = pd, lng = pd$lng, lat = pd$lat)
    } else {
      message('here instead')
      leafletProxy('fid_leaf') %>%
        clearMarkers() 
    }
    
  })
  
  # TRACCAR GPS UI
  output$traccar_plot_1 <- renderLeaflet({
    # mapviewOptions(#basemaps = c("Esri.WorldShadedRelief", "OpenStreetMap.DE"),
    #   # raster.palette = grey.colors,
    #   # vector.palette = colorRampPalette(c("snow", "cornflowerblue", "grey10")),
    #   # na.color = "magenta",
    #   layers.control.pos = "topright")
    # 
    
    pd <- odk_data$data
    the_form <- input$forms_gps
    enum <- pd$enumerations
    va <- pd$va
    pd <- pd$minicensus_main
    fid <- input$fid_gps
    # [1] 344 316 420 331 373 400 406 359 402 392 438 383 378 368 303 390 421 343 414 412 376 366 340 326 310 348 405
    # [28] 360 345 322 419 380 427 599 434 395
    # con <- get_db_connection(local = TRUE)
    # Get the traccar data for that country
    traccar <- dbGetQuery(conn = con,
                          statement = paste0('SELECT * FROM traccar WHERE unique_id = ', fid))
    # dbDisconnect(con)
    # traccar <- session_data$traccar
    date_slider <- input$gps_slider
    # save(pd, enum,va, file = 'temp_gps.rda')
    
    # save(traccar, date_slider, fid, file = '/tmp/dec1b.RData')
    ok <- TRUE
    if(is.null(traccar) | is.null(date_slider) | is.null(fid)){
      ok <- FALSE
    }
    if(ok){
      if(nrow(traccar) == 0){
        ok <- FALSE
      }
    }
    if(ok){
      # Get sub-data
      sub_data <- traccar %>% filter(as.numeric(as.character(unique_id)) == as.numeric(as.character(fid)))
      sub_data$date <- as.Date(sub_data$devicetime, 'EST')
      sub_data <- sub_data %>% 
        # filter(date == date_slider)
        filter(date >= date_slider[1],
               date <= date_slider[2])
      if(nrow(sub_data) == 0){
        ok <- FALSE
      }
    }
    
    # hqx <- hq$data
    
    if(ok){
      sub_data$time_of_day <- lubridate::round_date(sub_data$devicetime, 'hour')
      sub_data$day <- lubridate::round_date(sub_data$devicetime, 'day')
      
      sub_data$time_of_day <- as.character(sub_data$time_of_day)
      sub_data$day <- as.character(sub_data$day)
      
      pts = st_as_sf(data.frame(sub_data), coords = c("longitude", "latitude"), crs = 4326) %>% points_to_line2()
      l <- mapview::mapview(pts["grp"],
                            legend = FALSE,
                            layer.name = 'Date-time') 
      
      m <- l@map
      # subset by wid and dates 
      sub_mini <- pd %>%  filter(wid == fid) %>% filter(todays_date >= date_slider[1],
                                                        todays_date <= date_slider[2])
      enum<- enum %>%  filter(wid == fid) %>% filter(todays_date >= date_slider[1],
                                                     todays_date <= date_slider[2])
      va <- va %>%  filter(wid == fid) %>% filter(todays_date >= date_slider[1],
                                                  todays_date <= date_slider[2])
      
      if(nrow(sub_mini)==0 & nrow(enum)==0 & nrow(va)==0){
        m <- l@map
      } else  {
        if(nrow(sub_mini) > 0 & 'Minicensus' %in% the_form){
          pd_locs <- extract_ll(sub_mini$hh_geo_location)
          sub_mini$lng <- pd_locs$lng
          sub_mini$lat <- pd_locs$lat
          m <- m %>%
            addCircleMarkers(data = sub_mini,
                             lng = sub_mini$lng, 
                             lat = sub_mini$lat, 
                             color = 'blue',
                             popup = c(sub_mini$start_time))
          return(m)
          
        }
        if(nrow(enum) > 0 & 'Enumerations' %in% the_form){
          pd_locs <- extract_ll(enum$location_gps)
          enum$lng <- pd_locs$lng
          enum$lat <- pd_locs$lat
          m <- m %>%
            addCircleMarkers(data = enum,
                             lng = enum$lng, 
                             lat = enum$lat, 
                             color = 'green',
                             popup = enum$start_time) 
          return(m)
        }
        if(nrow(va) > 0 & 'VA' %in% the_form) {
          pd_locs <- extract_ll(va$location_gps)
          va$lng <- pd_locs$lng
          va$lat <- pd_locs$lat
          m <- m %>%
            addCircleMarkers(data = va,
                             lng = va$lng, 
                             lat = va$lat, 
                             color = 'green',
                             popup = va$start_time) 
          return(m)
        } 
      }
    } else {
      NULL
    }
    
  })
  
  # Map of most recent gps 
  output$traccar_recent <- renderLeaflet({
    co = country()
    traccar <- dbGetQuery(conn = con,
                          statement = 'SELECT unique_id, devicetime, latitude, longitude, valid FROM traccar INNER JOIN(SELECT unique_id, MAX(devicetime) as devicetime FROM traccar GROUP BY unique_id) AS t1 USING(unique_id, devicetime)')
    # create a days ago variable 
    traccar$date <- lubridate::as_date(traccar$devicetime)
    todays_date <- Sys.Date()
    traccar$days_ago <-as.numeric(todays_date)- as.numeric(traccar$date)
    sub_traccar <- traccar %>% filter(unique_id %in% fids$bohemia_id[fids$country == co])
    pts = st_as_sf(data.frame(sub_traccar), coords = c("longitude", "latitude"), crs = 4326)
    names(pts)[1] <- 'FW ID'
    hqx <- hq$data
    
    # Make the plot
    l <- leaflet() %>%
      addTiles() %>%
      addMarkers(data = hqx,  
                 popup = hqx$label)
    if(!is.null(sub_traccar)){
      if(nrow(sub_traccar) > 0){
        color_pal <- colorNumeric(palette =  colorRampPalette(c('red', 'green'))(length(pts$days_ago)),domain = sort(unique(pts$days_ago)), reverse = T)
        
        
        l <- l %>%leaflet() %>%
          addTiles() %>%
          addCircleMarkers(data = pts,
                           color  = ~color_pal(days_ago),
                           # fillColor = pts$status,
                           popup = pts %>% dplyr::select(`FW ID`,devicetime, valid),
                           group = "pts") %>%
          addLegend("bottomright", 
                    colors =color_pal(sort(unique(pts$days_ago))),
                    labels=sort(unique(pts$days_ago)),
                    opacity = 1)
        
      }
    }
    
    l
  })
  
  
  output$traccar_leaf <- renderLeaflet({
    # leaflet() %>% addTiles()
    # Get the traccar data for that country
    # traccar <- session_data$traccar
    the_worker <- input$fid_gps
    co = country()
    if(!is.null(the_worker)){
      sub_traccar <- dbGetQuery(conn = con,
                                statement = paste0('SELECT * FROM traccar WHERE unique_id = ', the_worker))
      pts = st_as_sf(data.frame(sub_traccar), coords = c("longitude", "latitude"), crs = 4326)
    }
    if(co == 'Mozambique'){
      shp_file = mop3
    } else {
      shp_file = ruf3
    }
    hqx <- hq$data
    # Make the plot
    l <- leaflet(shp_file) %>%
      addPolygons(weight = 1, label = shp_file@data$NAME_3) %>%
      addTiles() %>%
      addMarkers(data = hqx,  
                 popup = hqx$label)
    if(!is.null(sub_traccar)){
      if(nrow(sub_traccar) > 0){
        l <- l %>%
          addGlPoints(data = pts,
                      fillColor = 'red',
                      # fillColor = pts$status,
                      popup = pts %>% dplyr::select(devicetime, valid),
                      group = "pts")
      }
    }
    
    l
  })
  
  output$traccar_table_1 <- DT::renderDataTable({
    
    # Get the traccar data for that country
    
    date_slider <- input$gps_slider
    fid <- input$fid_gps
    
    sub_traccar <- dbGetQuery(conn = con,
                              statement = paste0('SELECT * FROM traccar WHERE unique_id = ', fid))
    
    # save(traccar, date_slider, fid, file = '/tmp/dec1x.RData')
    ok <- TRUE
    if(is.null(sub_traccar) | is.null(date_slider) | is.null(fid)){
      ok <- FALSE
    }
    if(ok){
      if(nrow(sub_traccar) == 0){
        ok <- FALSE
      }
    }
    if(ok){
      # Get sub-data
      sub_data <- sub_traccar %>% filter(as.numeric(as.character(unique_id)) == as.numeric(as.character(fid)))
      sub_data$date <- as.Date(sub_data$devicetime, 'EST')
      sub_data <- sub_data %>% 
        # filter(date == date_slider)
        filter(date >= date_slider[1],
               date <= date_slider[2])
      sub_data$time_of_day <- lubridate::round_date(sub_data$devicetime, 'hour')
      sub_data$day <- lubridate::round_date(sub_data$devicetime, 'day')
      
      sub_data$time_of_day <- as.character(sub_data$time_of_day)
      sub_data$day <- as.character(sub_data$day)
      out <- sub_data %>%
        group_by(Day = day) %>%
        summarise(`GPS pings` = n())
      
    } else {
      NULL
    }
    
  })
  
  
  
  observeEvent(input$traccar_table_1_rows_selected, ignoreNULL = FALSE, {
    
    sr <- input$traccar_table_1_rows_selected
    
    if(length(sr) == 0){
      leafletProxy('traccar_plot_1') %>%
        clearMarkers() 
    } else {
      message('selected rows for traccar table 1:\n ', paste0(sr, collapse = ', '))
      
      # Get the traccar data for that country
      date_slider <- input$gps_slider
      fid <- input$fid_gps
      sub_traccar <- dbGetQuery(conn = con,
                                statement = paste0('SELECT * FROM traccar WHERE unique_id = ', fid))
      # save(traccar, date_slider, fid, file = '/tmp/dec1z.RData')
      ok <- TRUE
      if(is.null(sub_traccar) | is.null(date_slider) | is.null(fid)){
        ok <- FALSE
      }
      if(ok){
        if(nrow(sub_traccar) == 0){
          ok <- FALSE
        }
      }
      if(ok){
        # Get sub-data
        sub_data <- sub_traccar %>% filter(as.numeric(as.character(unique_id)) == as.numeric(as.character(fid)))
        sub_data$date <- as.Date(sub_data$devicetime, 'EST')
        sub_data$day <- lubridate::round_date(sub_data$devicetime, 'day')
        sub_data <- sub_data %>% 
          # filter(date == date_slider)
          filter(date >= date_slider[1],
                 date <= date_slider[2])
        sub_data$time_of_day <- lubridate::round_date(sub_data$devicetime, 'hour')
        sub_data$time_of_day <- as.character(sub_data$time_of_day)
      }
      if(nrow(sub_data) < 1){
        ok <- FALSE
      }
      if(ok){
        out <- sub_data %>%
          group_by(Day = day) %>%
          summarise(`GPS pings` = n())
        selected_hours <- out[sr,]
        done <- sub_data[sub_data$day %in% selected_hours$Day,]
        leafletProxy('traccar_plot_1') %>%
          clearMarkers() %>%
          addMarkers(data = done, lng = done$longitude, lat = done$latitude,
                     popup = done$devicetime)
        
      } 
    }
  })
  
  
  
  output$traccar_table <- DT::renderDataTable({
    
    date_slider <- input$gps_slider
    # fid <- input$fid_gps
    
    # sub_traccar <- dbGetQuery(conn = con,
    #                           statement = paste0('SELECT * FROM traccar WHERE unique_id = ', fid)) 
    the_country <- country()
    if(the_country == 'Mozambique'){
      country_string <- ' WHERE unique_id >=301'
    } else {
      country_string <- ' WHERE unique_id <300'
    }
    
    sub_traccar <- dbGetQuery(conn = con,
                              statement = paste0("SELECT * FROM traccar ", country_string))
    
    # save(sub_traccar, fid, date_slider, fid, file = '/tmp/dec17x.RData')
    ok <- TRUE
    if(is.null(sub_traccar) | is.null(date_slider)){
      ok <- FALSE
    }
    if(ok){
      if(nrow(sub_traccar) == 0){
        ok <- FALSE
      }
    }
    if(ok){
      # Get sub-data
      # sub_data <- sub_traccar %>% filter(as.numeric(as.character(unique_id)) == as.numeric(as.character(fid)))
      sub_data <- sub_traccar
      sub_data$date <- as.Date(sub_data$devicetime, 'EST')
      sub_data <- sub_data %>% 
        # filter(date == date_slider)
        filter(date >= date_slider[1],
               date <= date_slider[2])
      sub_data$time_of_day <- lubridate::round_date(sub_data$devicetime, 'hour')
      sub_data$day <- lubridate::round_date(sub_data$devicetime, 'day')
      
      sub_data$time_of_day <- as.character(sub_data$time_of_day)
      sub_data$day <- as.character(sub_data$day)
      
      
      out <- sub_data %>%
        group_by(Day = day) %>%
        traccar_transform() %>%
        traccar_summarise(fids = fids)
      out
    } else {
      NULL
    }
    
  })
  
  output$most_recent_locations <- renderLeaflet({
    date_slider <- input$gps_slider
    # fid <- input$fid_gps
    
    # sub_traccar <- dbGetQuery(conn = con,
    #                           statement = paste0('SELECT * FROM traccar WHERE unique_id = ', fid)) 
    the_country <- country()
    if(the_country == 'Mozambique'){
      country_string <- ' WHERE unique_id >=301'
    } else {
      country_string <- ' WHERE unique_id <300'
    }
    hqx <- hq$data
    sub_traccar <- dbGetQuery(conn = con,
                              statement = paste0("SELECT * FROM traccar ", country_string))
    
    # save(sub_traccar, fid, date_slider, fid, file = '/tmp/dec17x.RData')
    ok <- TRUE
    if(is.null(sub_traccar) | is.null(date_slider)){
      ok <- FALSE
    }
    if(ok){
      if(nrow(sub_traccar) == 0){
        ok <- FALSE
      }
    }
    if(ok){
      # Get sub-data
      # sub_data <- sub_traccar %>% filter(as.numeric(as.character(unique_id)) == as.numeric(as.character(fid)))
      sub_data <- sub_traccar
      sub_data$date <- as.Date(sub_data$devicetime, 'EST')
      sub_data <- sub_data %>% 
        # filter(date == date_slider)
        filter(date >= date_slider[1],
               date <= date_slider[2])
      
      
      out <- sub_data %>%
        mutate(bohemia_id = as.numeric(as.character(unique_id))) %>%
        left_join(fids %>% dplyr::select(bohemia_id, 
                                         first_name, 
                                         last_name,
                                         Role)) %>%
        mutate(Fieldworker = paste0(bohemia_id, ' ',
                                    first_name, ' ',
                                    last_name, ' (',
                                    Role, ')')) %>%
        group_by(Fieldworker) %>%
        filter(devicetime == max(devicetime))
      
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = out,
                   popup = paste0(out$Fieldworker, ' ',
                                  out$devicetime)) %>%
        addCircleMarkers(data = hqx, popup = 'HQ')
      
    } else {
      NULL
    }
  })
  
  ### 401 errors, just commenting out for now
  # output$traccar_live_view <- renderUI({
  #   # See if the user is logged in and has access
  #   si <- session_info
  #   li <- si$logged_in
  #   ac <- TRUE
  #   # Generate the ui
  #   make_ui(li = li,
  #           ac = ac,
  #           ok = {
  #             if(grepl('brew', getwd())){
  #               fluidRow(h3('You are in dev mode. Not showing up here.'))
  #             } else {
  #               creds <- yaml::yaml.load_file('credentials/credentials.yaml')
  #               user = creds$traccar_read_only_user
  #               password = creds$traccar_read_only_pass
  #               rurl <- paste0('http://bohemia.fun/?token=', creds$traccar_read_only_token)
  #               r = GET(rurl,
  #                       authenticate(user = user,
  #                                    password = password, 
  #                                    type = 'basic'),
  #                       accept_json())
  #               rcontent <- content(r)
  #               
  #               tags$iframe(
  #                 height = 800, width = 1200,
  #                 seamless="seamless",
  #                 src = paste0('https://bohemia.fun/?token=', creds$traccar_read_only_token))
  #             }
  #             
  #           })})
  
  output$ui_gps <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    co <- country()
    sub_fids <- fids %>% 
      filter(country == co)
    the_choices <- sub_fids$bohemia_id
    names(the_choices) <- paste0(sub_fids$bohemia_id, '. ',
                                 sub_fids$first_name, ' ',
                                 sub_fids$last_name)
    # save(ac, li, si, co, sub_fids, the_choices, file = '/tmp/dec1.RData')
    
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              
              
              fluidPage(
                fluidRow(
                  column(4,
                         sliderInput(inputId = 'gps_slider', 
                                     label = 'Select dates', 
                                     min = as.Date('2020-09-01'), 
                                     max= Sys.Date(), 
                                     value = c(Sys.Date()-30, Sys.Date()-1)# c(as.Date('2020-09-01'), Sys.Date())
                         )),
                  column(4,
                         selectInput('fid_gps',
                                     'Fieldworker ID',
                                     choices = the_choices)),
                  column(4,
                         checkboxGroupInput('forms_gps',
                                            'Choose form',
                                            choices = c('Minicensus', 'Enumerations', 'VA'))), 
                ),
                # fluidRow(column(12, align = 'center',
                #                 h3('Live view'),
                #                 uiOutput('traccar_live_view'))),
                fluidRow(
                  column(3,
                         align = 'center',
                         DT::dataTableOutput('traccar_table_1')),
                  column(9, align = 'center',
                         leafletOutput('traccar_plot_1',
                                       height = 600))),
                
                fluidRow(column(6, align = 'center',
                                h3('Locations visited by FW (all time)'),
                                leafletOutput('traccar_leaf',
                                              height = 500)),
                         column(6,
                                h3('Locations of minicensus forms submitted by FW'),
                                leafletOutput('leaf_fid',
                                              height = 500)
                         )),
                fluidRow(column(6, align='center',
                                h3('Last recorded GPS location'),
                                leafletOutput('traccar_recent',
                                              height = 500)))
                
              )
              
            })
  })
  output$ui_geo_r_and_a <- renderUI({
    cn <- input$geo
    if(cn=='Rufiji'){
      cn_choices = c('District',
                     'Ward',
                     'Village',
                     'Hamlet',
                     'Household level')
    } else {
      cn_choices = c('Distrito' = 'District',
                     'Posto administrativo/localidade' ='Ward',
                     'Povoado' ='Village',
                     'Bairro' ='Hamlet',
                     'Household level')
    }
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              fmg <- field_monitoring_geo()
              fluidPage(
                column(12, align = 'center',
                       radioButtons('geo_r_and_a',
                                    'Geographic level:',
                                    choices = cn_choices,
                                    selected = fmg,
                                    inline = TRUE))
              )
            })
  })
  
  # date range for refusals and absences
  output$ui_date_r_and_a <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              # pd <- odk_data$data
              # pd <- pd$minicensus_main
              fluidPage(
                column(6,
                       sliderInput(inputId = 'date_r_and_a', 
                                   label = 'Select dates', 
                                   min= as.Date('2020-09-01'), #  min(pd$todays_date), 
                                   max= Sys.Date(), #max(pd$todays_date), 
                                   value = c(as.Date('2020-09-01'), Sys.Date())))
              )
              
            })
  })
  
  # Refusals and absences UI
  output$ui_refusals_and_absences <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              # Get the country
              co <- country()
              
              #get date range 
              date_range <- input$date_r_and_a
              if(is.null(date_range)){
                date_range <- c(as.Date('2020-09-01'), Sys.Date())
              }
              
              geo_r_and_a <- input$geo_r_and_a
              if(is.null(geo_r_and_a)){
                NULL
              } else {
                
                if(geo_r_and_a=='Household level'){
                  grouper <- 'household'
                } else{
                  grouper <- tolower(geo_r_and_a)
                }
                # # get refusals data
                rf = odk_data$data$refusals
                # save(rf, file = '/tmp/rf.RData')
                odk <- odk_data$data
                # save(odk, file = '/tmp/odk.RData')
                # Keep only the country in question
                rf <- rf %>% dplyr::filter(country == co)
                
                # Remove those refusals for which a minicensus form ended up being collected
                rf <- rf %>%
                  filter(!hh_id %in% odk$minicensus_main$hh_id)
                
                # HERE make it work with week
                # Get agg
                rf_agg <- rf %>%
                  mutate(free_text = reason_no_participate) %>%
                  mutate(reason_no_participate = ifelse(reason_no_participate %in% 
                                                          c('SEM COMENTARIO',
                                                            'He didnt want to do it',
                                                            'Dont know',
                                                            'refused'),
                                                        'refused',
                                                        'not_present')) %>%
                  group_by(district, ward, village, hamlet, hh_id, reason_no_participate, free_text) %>%
                  summarise(n = n(),
                            date = max(as.Date(todays_date), na.rm = TRUE)) %>%
                  ungroup
                
                out_rf <- rf_agg %>%
                  filter(reason_no_participate == 'refused') %>%
                  filter(date >= date_range[1], date<=date_range[2]) %>%
                  dplyr::select(district, ward, village, hamlet, hh_id,
                                `Refusal date` = date,
                                `Description` = free_text)
                
                out_ab <- rf_agg %>%
                  filter(reason_no_participate == 'not_present',
                         reason_no_participate != 'refused')  %>%
                  filter(date >= date_range[1], date<=date_range[2]) %>%
                  dplyr::select(district, ward, village, hamlet, hh_id,
                                `Visits` = n,
                                `Last visit` = date)
                
                if(grouper!='household'){
                  # here make sure numbers are right and then apply to other graphs
                  # get the time series version
                  out_rf_time <- out_rf  %>% 
                    mutate(week = lubridate::week(`Refusal date`)) %>%
                    group_by(week) %>%
                    mutate(week_label = min(`Refusal date`)) %>%
                    group_by(week_label) %>% summarise(`Number of refusals` = n())
                  
                  out_ab_time <- out_ab  %>% 
                    mutate(week = lubridate::week(`Last visit`)) %>%
                    group_by(week) %>%
                    mutate(week_label = min(`Last visit`)) %>%
                    group_by(week_label) %>% summarise(`Number of absences` = n())
                  
                  out_rf <- out_rf %>% 
                    group_by_(grouper) %>% 
                    summarise(`Number of refusals` = n())
                  
                  out_ab <- out_ab %>% 
                    group_by_(grouper) %>% 
                    summarise(`Number of absences` = n())
                  
                  # create time plots
                  # finished ref, now do ab below this and then make plots when condition is households (belwo)
                  output$ref_plot_time <- renderPlot({
                    ggplot(out_rf_time, aes(as.factor(week_label), `Number of refusals`)) + geom_bar(stat = 'identity') +
                      labs(x='') + theme_bohemia() +
                      theme(axis.text = element_text(angle=90, vjust=0.5, size = sizex),
                            axis.title = element_text(size = 14),
                            plot.title = element_text(size = 20))
                  })
                  
                  output$ab_plot_time <- renderPlot({
                    ggplot(out_ab_time, aes(as.factor(week_label), `Number of absences`)) + geom_bar(stat = 'identity') +
                      labs(x='') + theme_bohemia() +
                      theme(axis.text = element_text(angle=90, vjust=0.5, size = sizex),
                            axis.title = element_text(size = 14),
                            plot.title = element_text(size = 20))
                  })
                  
                  # only create plots if household level not selected
                  # create plot for refusals
                  output$ref_plot <- renderPlot({
                    names(out_rf)[1] <- 'V1'
                    ggplot(out_rf, aes(reorder(V1, -`Number of refusals`), `Number of refusals`)) + geom_bar(stat = 'identity') +
                      labs(x='') + theme_bohemia() +
                      theme(axis.text = element_text(angle=90, vjust=0.5, size = sizex),
                            axis.title = element_text(size = 14),
                            plot.title = element_text(size = 20))
                  })
                  
                  output$ab_plot <- renderPlot({
                    names(out_ab)[1] <- 'V1'
                    ggplot(out_ab, aes(reorder(V1, -`Number of absences`), `Number of absences`)) + geom_bar(stat = 'identity') +
                      labs(x='') + theme_bohemia() +
                      theme(axis.text = element_text(angle=90, vjust=0.5, size = sizex),
                            axis.title = element_text(size = 14),
                            plot.title = element_text(size = 20))
                  })
                } 
                
                
                
                if(co=='Mozambique'){
                  if(grouper == 'household'){
                    names(out_rf)[1:4] <-c('Distrito','Posto administrativo/localidade','Povoado','Bairro')
                    names(out_ab)[1:4] <-c('Distrito','Posto administrativo/localidade','Povoado','Bairro')
                  } else if(grouper =='hamlet'){
                    names(out_rf)[1] <-'Bairro'
                    names(out_ab)[1] <- 'Bairro'
                  } else if(grouper=='district'){
                    names(out_rf)[1] <- 'Distrito'
                    names(out_ab)[1] <- 'Distrito'
                  } else if(grouper=='village'){
                    names(out_rf)[1] <- 'Povoado'
                    names(out_ab)[1] <- 'Povoado'
                  } else if(grouper=='ward'){
                    names(out_rf)[1] <- 'Posto administrativo/localidade'
                    names(out_ab)[1] <- 'Posto administrativo/localidade'
                  } 
                  
                }
                
                
                tabsetPanel(
                  tabPanel(title = 'Refusals',
                           fluidPage(
                             fluidRow(
                               bohemia::prettify(out_rf,
                                                 nrows = nrow(out_rf),
                                                 download_options = TRUE)
                             ),
                             fluidRow(
                               column(6, 
                                      plotOutput('ref_plot', height = '700px')),
                               column(6, 
                                      plotOutput('ref_plot_time', height = '700px')),
                               
                               
                             )
                           )),
                  tabPanel(title = 'Absences',
                           fluidPage(
                             fluidRow(
                               bohemia::prettify(out_ab,
                                                 nrows = nrow(out_ab),
                                                 download_options = TRUE)
                             ),
                             fluidRow(
                               column(6, 
                                      plotOutput('ab_plot', height = '700px')),
                               column(6, 
                                      plotOutput('ab_plot_time', height = '700px')),
                               
                               
                             )
                           ))
                )
                
              }
              
            })
  })
  
  # Observe the fix submission
  observeEvent(input$submit_fix,{
    sr <- input$anomalies_table_rows_selected
    action <- anomalies_dt()
    # save(action, file = '/tmp/action.RData')
    this_row <- action[sr,]
    # fids <- read.csv('/tmp/fids.csv')
    gg <- input$geo
    if(gg=='Mopeia'){
      co <- 'Mozambique'
    } else {
      co <- 'Tanzania'
    }
    
    fids <- fids %>% 
      filter(country==co)
    the_choices <- fids$bohemia_id
    names(the_choices) <- paste0(fids$bohemia_id, '. ',
                                 fids$first_name, ' ',
                                 fids$last_name)
    
    # Must be just one row
    just_one <- FALSE
    if(nrow(this_row) == 1){
      just_one <- TRUE
    }
    if(just_one){
      this_row$date <- as.character(this_row$date)
      this_row <- tidyr::gather(this_row, key, value)
      this_row <- this_row %>%
        dplyr::filter(key %in% c('type', 'days_ago', 'FW', 'id', 'description', 'incident',
                                 'instance_id', 'supervisor'))
      this_row$key <- toupper(this_row$key)
      showModal(
        modalDialog(
          title = 'Anomaly/error resolution',
          size = 'l',
          easyClose = TRUE,
          fade = TRUE,
          footer = modalButton('Go back'),
          fluidPage(
            fluidRow(h3('The problem:')),
            fluidRow(HTML(knitr::kable(this_row, format = 'html'))),
            fluidRow(h3('The response:')),
            fluidRow(textAreaInput('response_details', 'Response details:')),
            fluidRow(selectizeInput('fix_source', 'Resolved by', choices = the_choices,
                                    options = list(create=TRUE))),
            fluidRow(selectizeInput('fix_method', 'Method of resolution',
                                    choices = c('By going to HH', 'By phone', 'Self-resolved'),
                                    options = list(create = TRUE))),
            fluidRow(dateInput('resolution_date', 'Resolution date',
                               min = '2020-10-01',
                               max = Sys.Date())),
            fluidRow(column(12, align = 'center',
                            actionButton('send_fix',
                                         'Submit response')))
          )
        )
      )
    } else {
      showModal(
        modalDialog(
          title = 'Not quite...',
          size = 'm',
          easyClose = TRUE,
          fade = TRUE,
          footer = modalButton('Go back'),
          fluidPage(
            p(paste0('You have selected ',
                     nrow(this_row), 
                     ' rows.')),
            p('Before submitting a correction / confirmation, you must select exactly one row.')
          )
        )
      )
    }
    
  })
  
  
  
  # # Observe the notification disregard
  # observeEvent(input$discard_notification,{
  #   sr <- input$notifications_table_rows_selected
  #   notifications <- session_data$notifications
  #   message('sr is ', sr)
  #   vals <- 1:nrow(notifications)
  #   vals <- vals[!vals %in% sr]
  #   notifications <- notifications[vals,]
  #   session_data$notifications <- notifications
  # })
  
  # Confirm a fix send
  observeEvent(input$send_fix,{
    sr <- input$anomalies_table_rows_selected
    action <- session_data$anomalies
    joined <- anomalies_dt()
    
    # Apply the filters
    hide_submitted <- input$hide_submitted
    if(!is.null(hide_submitted)){
      if(hide_submitted){
        joined <- joined %>% filter(!resolution_submitted) 
      }
    }
    anomalies_show <- input$anomalies_show
    message('anomalies show is:')
    if(!is.null(anomalies_show)){
      joined <- joined %>% filter(type %in% anomalies_show)
    }
    
    
    # Get the fix row
    this_row <- joined[sr,]
    # Get the fix text
    response_details <- paste0(input$response_details)
    fix_source <- input$fix_source
    fix_method <- input$fix_method
    resolution_date <- input$resolution_date
    log_in_user <- input$log_in_user
    fix <-
      tibble(id = this_row$id,
             instance_id = this_row$instance_id,
             response_details = response_details,
             resolved_by = fix_source,
             resolution_method = fix_method,
             resolution_date = as.character(resolution_date),
             submitted_by = log_in_user,
             submitted_at = Sys.time())
    # CONNECT TO THE DATABASE AND ADD FIX
    message('Connecting to the database in order to add a fix to the corrections table')
    # save(fix, fix_source, fix_method, resolution_date, log_in_user, response_details, this_row, action, sr, file = '/tmp/sunday.RData')
    # con <- get_db_connection(local = is_local)
    dbAppendTable(conn = con,
                  name = 'corrections',
                  value = fix)
    message('Done. now disconnecting from database')
    # dbDisconnect(con)
    # AND THEN MAKE SURE TO UPDATE THE IN-SESSION STUFF
    # save(this_row, sr, action, fix, odk_data, resolution_details, file = '/tmp/this_row.RData')
    message('Now uploading the in-session data')
    old_corrections <- odk_data$data$corrections 
    new_correction <- fix
    new_corrections <- bind_rows(old_corrections, new_correction)
    odk_data$data$corrections  <- new_corrections
    # save(new_corrections, file = '/tmp/new_corrections.RData')
    
    # message('sr is ', sr)
    # vals <- 1:nrow(action)
    # vals <- vals[!vals %in% sr]
    # action <- action[vals,]
    # session_data$anomalies <- action
    removeModal()
  })
  
  
  # Data management UI  ##############################################
  output$ui_enrollment <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'enrollment' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              
              # Get the country
              co <- input$geo
              co <- ifelse(co == 'Rufiji', 'Tanzania', 'Mozambique')
              
              # Get places visisted so far
              pd <- odk_data$data
              pd <- pd$minicensus_main
              
              # get enumerations and refusals data
              en = odk_data$data$enumerations
              rf = odk_data$data$refusals
              # save(pd, en, rf, file = 'saved.RData')
              # save(en, file = 'enum.rda')
              # save(pd, file = 'pd_en.rda')
              # save(rf, file = 'refs.rda')
              # save(co, pd, en, rf, file = 'joe.RData')
              
              # subset by country
              pd <- pd %>% dplyr::filter(hh_country == co) %>%
                group_by(hh_id) %>%
                summarise(num_mini = n(),
                          last_date_mini = max(todays_date, na.rm=TRUE),
                          loc = dplyr::first(hh_geo_location)) 
              rf <- rf %>% dplyr::filter(country == co) %>% 
                group_by(hh_id, reason_no_participate) %>%
                summarise(num_ref = n(),
                          last_date_ref = max(todays_date, na.rm=TRUE),
                          loc = dplyr::first(hh_geo_location)) 
              en <- en %>% dplyr::filter(country == co) %>% 
                group_by(agregado) %>%
                summarise(num_enum = n(),
                          last_date_enum = max(todays_date, na.rm=TRUE),
                          loc = dplyr::first(location_gps)) 
              
              # Extract locations
              pd_locs <- extract_ll(pd$loc)
              pd$lng_minicensus <- pd_locs$lng; pd$lat_minicensus <- pd_locs$lat; pd$loc <- NULL
              en_locs <- extract_ll(en$loc)
              en$lng_enumerations <- en_locs$lng; en$lat_enumerations <- en_locs$lat; en$loc <- NULL
              rf_locs <- extract_ll(rf$loc)
              rf$lng_refusals <- rf_locs$lng; rf$lat_refusals <- rf_locs$lat; rf$loc <- NULL
              
              # get list of all unique house ids
              all_hh_ids <- tibble(hh_id = sort(unique(c(pd$hh_id, rf$hh_id, en$agregado))))
              
              # join with rest of data
              dat <- left_join(all_hh_ids, pd)
              dat <- left_join(dat, en, by = c('hh_id'='agregado'))
              dat <- left_join(dat, rf)
              
              # See if there is any geocoding for many houses
              dat <- dat %>% mutate(any_geocode = !(is.na(lng_minicensus) &
                                                      is.na(lng_enumerations) &
                                                      is.na(lng_refusals))) %>%
                # See if there is a minicensus without enumeration
                mutate(minicensus_wo_enumeration = !is.na(last_date_enum) & is.na(last_date_mini)) %>%
                # See if there is enumeration without minicensus
                mutate(enumeration_wo_minicensus = is.na(last_date_enum) & !is.na(last_date_mini)) %>%
                # Get average time between enumeration and minicensus
                mutate(time_bw_enumeration_and_minicensus = as.numeric(last_date_mini - last_date_enum))
              
              # save(dat, file = '/tmp/dat.RData')
              # create summary stats off of dat
              
              sub_dat <- dat %>%
                summarise(`Minicensus forms collected` = sum(num_mini, na.rm = TRUE),
                          `Unique minicensus HH IDs` = length(which(!is.na(last_date_mini))),
                          `Enumeration forms collected` = sum(num_enum, na.rm = TRUE),
                          `Unique enumeration HH IDs` = length(which(!is.na(last_date_enum))),
                          `Refusals` = sum(num_ref[reason_no_participate %in% c('SEM COMENTARIO',
                                                                                'He didnt want to do it',
                                                                                'Dont know',
                                                                                'refused')], na.rm = TRUE),
                          `Unique households` = nrow(all_hh_ids),
                          `Households geocoded` = length(which(any_geocode)),
                          `Avg days between enumeration and minicensus` = mean(time_bw_enumeration_and_minicensus, na.rm = TRUE),
                          `Households enumerated but not minicensed` = length(which(enumeration_wo_minicensus))) %>%
                mutate(`%` = `Unique minicensus HH IDs` / `Unique enumeration HH IDs` * 100)
              message('---Created table for aggregated enrollment data')
              # save(dat, file = 'temp_dat.rda')
              sub_stats <- dat %>%
                summarise(`Minicensus response rate`= (length(which(!is.na(num_mini)))/nrow(dat))*100,
                          `# of enumeration refusals` = length(which(is.na(num_enum)&is.na(num_mini))),
                          `# of minicensus refusals`= length(which(!is.na(num_enum)&is.na(num_mini)&!is.na(num_ref))),
                          `# of hh ever absent` = sum(num_ref[reason_no_participate %in% c("Wasnt there",
                                                                                           'not_present' )], na.rm = TRUE),
                          `# of refusals due to 3 absences` = length(which(`# of hh ever absent`>3)),
                          `% refused` = (sum(`# of enumeration refusals`,`# of minicensus refusals`,`# of refusals due to 3 absences`)/nrow(dat))*100,
                          `# of households absent then present` = length(which(last_date_ref>last_date_mini)),
                          `% successful follow up`=(`# of households absent then present`/`# of hh ever absent`))
              # Get raw data table for display
              dat <- dat %>%
                mutate(lng = ifelse(is.na(lng_minicensus),
                                    ifelse(is.na(lng_enumerations),
                                           ifelse(is.na(lng_refusals),
                                                  NA,
                                                  lng_refusals),
                                           lng_enumerations),
                                    lng_minicensus)) %>%
                mutate(lat = ifelse(is.na(lat_minicensus),
                                    ifelse(is.na(lat_enumerations),
                                           ifelse(is.na(lat_refusals),
                                                  NA,
                                                  lat_refusals),
                                           lat_enumerations),
                                    lat_minicensus)) %>%
                mutate(status = ifelse(!is.na(last_date_ref),
                                       'Refused',
                                       ifelse(!is.na(last_date_mini),
                                              'Minicensed',
                                              ifelse(!is.na(last_date_enum),
                                                     'Enumerated',NA)))) %>%
                dplyr::select(hh_id, status, num_mini, last_date_mini,
                              num_enum, last_date_enum,
                              num_ref, last_date_ref, reason_no_participate,
                              lng, lat) 
              
              geo_dat <- dat %>%
                filter(!is.na(lng)) %>%
                filter(lng > 1,
                       lat < -1)
              hqx <- hq$data
              pts = st_as_sf(data.frame(geo_dat), coords = c("lng", "lat"), crs = 4326)
              dat_leaf <- leaflet() %>%
                addProviderTiles(provider = providers$Esri.WorldImagery) %>%
                addMarkers(data = hqx,  
                           popup = hqx$label) %>%
                addGlPoints(data = pts,
                            # fillColor = 'red',
                            fillColor = pts$status,
                            popup = pts,
                            group = "pts") %>%
                addLegend("bottomright", 
                          colors =c("#FFFF00",  "#871F78", "#00FFFF"),
                          labels= c("Refused", "Enumerated","Minicensed"),
                          title= "Status",
                          opacity = 1)
              # setView(lng = 35.7, lat = 18, zoom = 4) %>%
              # addLayersControl(overlayGroups = "pts")
              # # Add markers
              # icon_enumerations <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue', iconColor = 'black')
              # icon_refusals <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa', iconColor = 'black')
              # icon_minicensus <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion')
              
              # ref_geo <- geo_dat %>% filter(status == 'Refused')
              # en_geo <- geo_dat %>% filter(status == 'Enumerated')
              # mc_geo <- geo_dat %>% filter(status == 'Minicensed')
              # 
              # if(nrow(ref_geo) > 0){
              #   pts = st_as_sf(data.frame(ref_geo), coords = c("lng", "lat"), crs = 4326)
              #   dat_leaf <- dat_leaf %>%
              #     addGlPoints(data = pts, fillColor = 'red', group = "pts")
              #     # addAwesomeMarkers(data = ref_geo,
              #     #                   label = ref_geo$hh_id,
              #     #                   labelOptions = labelOptions(noHide = TRUE),
              #     #                   icon = icon_refusals)
              # }
              # if(nrow(en_geo) > 0){
              #   pts2 = st_as_sf(data.frame(en_geo), coords = c("lng", "lat"), crs = 4326)
              #   dat_leaf <- dat_leaf %>%
              #     addGlPoints(data = pts2, fillColor = 'blue', group = "pts2") 
              #     # addAwesomeMarkers(data = en_geo,
              #     #                   label = en_geo$hh_id,
              #     #                   labelOptions = labelOptions(noHide = TRUE),
              #     #                   icon = icon_enumerations)
              # }
              # if(nrow(mc_geo) > 0){
              #   pts3 = st_as_sf(data.frame(mc_geo), coords = c("lng", "lat"), crs = 4326)
              #   dat_leaf <- dat_leaf %>%
              #     addGlPoints(data = pts, fillColor = 'green', group = "pts3") 
              #     # addAwesomeMarkers(data = mc_geo,
              #     #                   label = mc_geo$hh_id,
              #     #                   labelOptions = labelOptions(noHide = TRUE),
              #     #                   icon = icon_minicensus)
              # }
              # dat_leaf <- dat_leaf %>%
              #   addLayersControl(overlayGroups = "pts3")
              output$dat_leaf_output <- renderLeafgl({
                dat_leaf
              })
              message('---Created Enrollment map')
              
              
              fluidPage(
                fluidRow(
                  column(12, align = 'center',
                         h2('Aggregated enrollment data'),
                         p('The below table shows a summary of enrollment'),
                         prettify(sub_dat, download_options = TRUE),
                         h2('Enrollment stats'),
                         prettify(sub_stats, download_options = TRUE),
                         br(),
                         h2('Enrollment map'),
                         leafglOutput('dat_leaf_output')#,
                         # h2('De-aggregated enrollment data'),
                         # p('The below table shows the status of each household'),
                         # prettify(dat, download_options = TRUE)
                  ),
                  
                  
                )
              )
              
              
            }
            
    )
  })
  
  
  # Done hamlets UI  ################################################
  output$done_hamlet_table <- DT::renderDataTable({
    sb <- input$sidebar
    liu <- input$log_in_user
    done_button <- input$mark_done
    not_done_button <- input$mark_undone
    # the_authorized_users <- c("iirema@ihi.or.tz",
    #                           "eldo.elobolobo@manhica.net",
    #                           "joe@databrew.cc")
    # if(liu %in% the_authorized_users){
    # con <- get_db_connection(local = is_local)
    done_hamlets <- dbGetQuery(conn = con,
                               statement = paste0("SELECT * FROM done_hamlets"))
    # dbDisconnect(con)
    bohemia::prettify(done_hamlets,
                      nrows = nrow(done_hamlets))
    # } else {
    #   bohemia::prettify(data.frame(o = 'You are not authorized to mark hamlets as done.'))
    # }
  })
  
  already_done_hamlets <- reactiveVal(value = c())
  listen_already_done <- reactive({
    list(as.character(input$mark_done),
         as.character(input$mark_undone),
         as.character(input$the_done_hamlet))
  })
  observeEvent(listen_already_done(),{
    sidebar <- input$sidebar
    # if(sidebar == 'done_hamlets'){
    
    # con <- get_db_connection(local = is_local)
    if(exists('con')){
      message('Updating the already_done_hamlets object')
      already <- dbReadTable(conn = con, 'done_hamlets')
      print(already)
      out <- as.character(already$code)
      print(out)
      already_done_hamlets(out)
    }
    
    # dbDisconnect(con)
    # }
  })
  
  output$ui_done_hamlets_choices <- renderUI({
    si <- session_info
    li <- si$logged_in
    ac <- 'server_status' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              
              the_country <- country()
              the_choices <- bohemia::locations$code[bohemia::locations$Country == the_country]
              the_choices <- sort(unique(the_choices))
              
              fluidPage(
                fluidRow(h3('Mark a hamlet as done')),
                fluidRow(p('MOZ: "Done" means that all enumerations for that hamlet have been finished.')),
                fluidRow(p('TZA: "Done" means that all minicensus forms for that hamlet have been finished.')),
                fluidRow(
                  column(12,
                         selectInput('the_done_hamlet',
                                     'Hamlet',
                                     choices = the_choices))
                  
                )
              )
            })
  })
  
  output$ui_done_hamlets <- renderUI({
    si <- session_info
    li <- si$logged_in
    ac <- 'server_status' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              
              liu <- input$log_in_user
              the_authorized_users <- c("iirema@ihi.or.tz",
                                        "eldo.elobolobo@manhica.net",
                                        'joao.dasilva@manhica.net',
                                        "joe@databrew.cc")
              
              # See if the selected hamlet has already been marked as done or not
              the_hamlet <- input$the_done_hamlet
              
              adh <- already_done_hamlets()
              already_done <- the_hamlet %in% adh
              # save(already_done, adh, the_hamlet, the_authorized_users, liu,
              #      file = '/tmp/done_hamlets.RData')
              if(is.null(already_done)){
                already_done <- FALSE
              }
              if(length(already_done) == 0){
                already_done <- FALSE
              }
              if(is.na(already_done)){
                already_done <- FALSE
              }
              if(liu %in% the_authorized_users){
                ok_to_mark <- TRUE
              } else {
                ok_to_mark <- FALSE
              }
              if(ok_to_mark){
                if(!already_done){
                  mark_button <- actionButton('mark_done',
                                              'Mark as done')
                } else {
                  mark_button <- actionButton('mark_undone',
                                              'Change to NOT done')
                }
                
              } else {
                mark_button <- actionButton('mark_done_fake',
                                            'Clicking here will do nothing')
              }
              
              fluidPage(
                fluidRow(
                  mark_button
                )
                
              )
            })
  })
  
  output$ui_done_hamlets2 <- renderUI({
    si <- session_info
    li <- si$logged_in
    ac <- 'server_status' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              DT::dataTableOutput('done_hamlet_table')
            })})
  
  
  
  observeEvent(input$mark_done, {
    the_hamlet <- input$the_done_hamlet
    liu <- input$log_in_user
    the_data <- tibble(code = the_hamlet,
                       done_by = liu,
                       done_at = Sys.time())
    # con <- get_db_connection(local = is_local)
    dbAppendTable(conn = con,
                  name = 'done_hamlets',
                  value = the_data)
    already <- dbReadTable(conn = con, 'done_hamlets')
    out <- as.character(already$code)
    already_done_hamlets(out)
    # dbDisconnect(con)
  })
  
  observeEvent(input$mark_undone, {
    the_hamlet <- input$the_done_hamlet
    liu <- input$log_in_user
    # con <- get_db_connection(local = is_local)
    dbSendQuery(conn = con,
                statement = paste0("DELETE FROM done_hamlets WHERE code ='", the_hamlet, "'"))
    already <- dbReadTable(conn = con, 'done_hamlets')
    out <- as.character(already$code)
    already_done_hamlets(out)
    # dbDisconnect(con)
  })
  
  # GEO TABLES UI ######################################################
  output$ui_geo_tables <- renderUI({
    si <- session_info
    li <- si$logged_in
    ac <- 'server_status' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              the_country <- country()
              the_iso <- ifelse(the_country == 'Tanzania', 'TZA', 'MOZ')
              
              pd <- bohemia::gps %>% left_join(bohemia::locations %>% 
                                                 dplyr::select(-clinical_trial))
              
              if(the_iso == 'MOZ'){
                pd <- pd %>%
                  mutate(n_households = round(n_households * 0.55))
              }
              
              pdx <- pd <- pd %>%
                filter(iso == the_iso,
                       clinical_trial == 0)
              pdx <- pdx %>%
                dplyr::select(code, District, Region, Ward, village, hamlet, n_households)
              
              pd <- pd %>%
                summarise(Hamlets = length(unique(code)),
                          Villages = length(unique(village)),
                          Wards = length(unique(ward)),
                          Districts = length(unique(District)),
                          `HH (estimated from recon)` = sum(n_households))
              
              fluidPage(
                fluidRow(h3('Overview')),
                fluidRow(bohemia::prettify(pd)),
                fluidRow(h3('Locations hierarchy')),
                fluidRow(bohemia::prettify(pdx, nrows = nrow(pdx), download_options = TRUE))
              )
              
            })
  })
  
  # Cross site UI ####################################################
  output$ui_cross_site <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- TRUE
    
    # Load the data
    the_country <- country()
    the_other_country <- ifelse(the_country == 'Mozambique', 'Tanzania', 'Mozambique')
    out1 <- odk_data$data
    out2 <- load_odk_data(local = is_local, the_country = the_other_country, efficient = TRUE, use_cached = use_cached, con = con)
    # save(out1, out2, file = '/tmp/out.RData')
    minicensus_main <- bind_rows(
      out1$minicensus_main,
      out2$minicensus_main
    )
    minicensus_people <- bind_rows(
      out1$minicensus_people,
      out2$minicensus_people
    )
    anomalies <- dbGetQuery(conn = con,
                            statement = paste0("SELECT * FROM anomalies"))
    corrections <- dbGetQuery(conn = con,
                              statement = "SELECT * FROM corrections;")
    fixes <- dbGetQuery(conn = con,
                        statement = "SELECT * FROM fixes;")
    fixes_ad_hoc <- dbGetQuery(conn = con,
                        statement = "SELECT * FROM fixes_ad_hoc;")
    # save(fixes, anomalies, corrections, fixes_ad_hoc, file = '/tmp/fixes.RData')
    
    # Meta stuff
    country_cols <- c('darkorange', 'lightblue')
    
    # get fixes which aren't related to corrections
    fix_no_co <- fixes %>% filter(!id %in% corrections$id)
    
    output$fixes_table <-
      DT::renderDataTable({
        tibble(
          `Anomalies detected` = nrow(anomalies),
          `Corrections submitted` = nrow(corrections),
          `Fixes implemented` = nrow(fixes),
          `Additional ad-hoc fixes implemented` = nrow(fixes_ad_hoc)
        )
      })
    
    # Anomalies by country table
    anomalies_by_country <- anomalies %>%
      group_by(country) %>%
      tally
    output$table_anomalies_by_country <- 
      DT::renderDataTable({
        anomalies_by_country  %>% bohemia::prettify(download_options = TRUE)
      })
    output$plot_anomalies_by_country <- 
      renderPlot({
        ggplot(data = anomalies_by_country,
               aes(x = country,
                   y = n)) +
          geom_bar(stat = 'identity',
                   aes(fill = country)) +
          scale_fill_manual(name = '', values = country_cols) +
          theme_bohemia() +
          theme(legend.position = 'none') 
      })
    
    # Anomalies by country/date chart
    anomalies_by_country_date <- 
      anomalies %>%
      group_by(country, date) %>%
      tally %>% 
      arrange(date) %>%
      group_by(country) %>%
      mutate(cumulative = cumsum(n)) 
    output$table_anomalies_by_country_date <- 
      DT::renderDataTable({
        anomalies_by_country_date %>%
          bohemia::prettify(download_options = TRUE)
      })
    output$plot_anomalies_by_country_date <- renderPlot({
      ggplot(data = anomalies_by_country_date,
             aes(x = date,
                 y = cumulative,
                 color = country)) +
        geom_line() +
        scale_color_manual(name = 'Country', values = country_cols) +
        theme_bohemia() +
        theme(legend.position = 'bottom')
    })
    
    # Anomalies by correction status
    anomalies_by_correction_status <- 
      anomalies %>%
      left_join(corrections %>% dplyr::select(-instance_id)) %>%
      group_by(corrected = ifelse(!is.na(resolution_method), 'Corrected',
                                  'Not yet corrected')) %>%
      tally
    output$table_anomalies_by_correction_status <- DT::renderDataTable({
      anomalies_by_correction_status %>% bohemia::prettify(download_options = TRUE)
    })
    output$plot_anomalies_by_correction_status <- renderPlot({
      ggplot(data = anomalies_by_correction_status,
             aes(x = corrected,
                 y = n)) +
        geom_bar(stat = 'identity',
                 aes(fill = corrected)) +
        scale_fill_manual(name = '',
                          values = c('darkgreen', 'red')) +
        theme_bohemia()  +
        theme(legend.position = 'none') +
        labs(x = '',
             y = 'Anomalies')
    })
    
    # Anomalies by correction status and country
    anomalies_by_correction_status_and_country <- 
      anomalies %>%
      left_join(corrections %>% dplyr::select(-instance_id)) %>%
      group_by(country,
               corrected = ifelse(!is.na(resolution_method), 'Corrected',
                                  'Not yet corrected')) %>%
      tally
    output$table_anomalies_by_correction_status_and_country <- DT::renderDataTable({
      anomalies_by_correction_status_and_country %>% bohemia::prettify(download_options = TRUE)
    })
    output$plot_anomalies_by_correction_status_and_country <- renderPlot({
      ggplot(data = anomalies_by_correction_status_and_country,
             aes(x = country,
                 y = n,
                 group = corrected)) +
        geom_bar(stat = 'identity',
                 position = position_stack(),
                 aes(fill = corrected)) +
        scale_fill_manual(name = '',
                          values = c('darkgreen', 'red')) +
        theme_bohemia()  +
        theme(legend.position = 'bottom') +
        labs(x = '',
             y = 'Anomalies')
    })
    
    # Number of minicensus forms by country
    minicensus_forms <-minicensus_main %>%
      group_by(hh_country) %>%
      tally
    output$table_minicensus_forms <- DT::renderDataTable({
      minicensus_forms %>% bohemia::prettify(download_options = TRUE)
    })
    output$plot_minicensus_forms <- renderPlot({
      ggplot(data = minicensus_forms,
             aes(x = hh_country,
                 y = n)) +
        geom_bar(stat = 'identity',
                 aes(fill = hh_country)) +
        scale_fill_manual(name = '', values = country_cols) +
        theme_bohemia() +
        theme(legend.position = 'none') +
        labs(x = '',
             y= 'Forms')
    })
    
    # Number of minicensus forms by country / date
    minicensus_forms_by_date <-minicensus_main %>%
      group_by(country = hh_country, date = todays_date) %>%
      tally %>%
      ungroup %>%
      arrange(date) %>%
      group_by(country) %>%
      mutate(cumulative = cumsum(n))
    output$table_minicensus_forms_by_date <- DT::renderDataTable({
      minicensus_forms_by_date %>% bohemia::prettify(download_options = TRUE)
    })
    output$plot_minicensus_forms_by_date <- renderPlot({
      ggplot(data = minicensus_forms_by_date,
             aes(x = date,
                 y = cumulative)) +
        geom_line(aes(color = country)) +
        scale_color_manual(name = '',
                           values = country_cols) +
        theme_bohemia() +
        theme(legend.position = 'bottom') +
        labs(x = 'Date',
             y= 'Forms')
    })
    
    
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              
              fluidPage(
                fluidRow(DT::dataTableOutput('fixes_table')),
                fluidRow(
                  column(6,
                         h3('Anomalies by country'),
                         plotOutput('plot_anomalies_by_country'),
                         DT::dataTableOutput('table_anomalies_by_country')),
                  column(6,
                         h3('Anomalies by country/date'),
                         plotOutput('plot_anomalies_by_country_date'),
                         DT::dataTableOutput('table_anomalies_by_country_date'))
                ),
                fluidRow(
                  column(6,
                         h3('Anomalies by correction status'),
                         plotOutput('plot_anomalies_by_correction_status'),
                         DT::dataTableOutput('table_anomalies_by_correction_status')),
                  column(6,
                         h3('Anomalies by correction status and country'),
                         plotOutput('plot_anomalies_by_correction_status_and_country'),
                         DT::dataTableOutput('table_anomalies_by_correction_status_and_country'))
                ),
                fluidRow(
                  column(6,
                         h3('Minicensus forms by country'),
                         plotOutput('plot_minicensus_forms'),
                         DT::dataTableOutput('table_minicensus_forms')),
                  column(6,
                         h3('Minicensus forms by country and date'),
                         plotOutput('plot_minicensus_forms_by_date'),
                         DT::dataTableOutput('table_minicensus_forms_by_date'))
                )
              )
              
            })
  })
  
  # Server status UI  ################################################
  output$ui_server_status_date <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'server_status' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              # pd <- odk_data$data
              # pd <- pd$minicensus_main
              fluidPage(
                column(6,
                       sliderInput(inputId = 'server_status_date', 
                                   label = 'Select dates', 
                                   min= as.Date('2020-09-01'), #  min(pd$todays_date), 
                                   max= Sys.Date(), #max(pd$todays_date), 
                                   value = c(as.Date('2020-09-01'), Sys.Date())))
              )
              
            })
  })
  output$ui_server_status <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'server_status' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              pd <- odk_data$data
              an <- session_data$anomalies
              corrections <- pd$corrections
              the_country <- country()
              date_range <- input$server_status_date
              if(is.null(date_range)){
                # date_range <- c(min(pd$todays_date), max(pd$todays_date))
                date_range <- c(as.Date('2020-09-01'),
                                Sys.Date())
              }
              # save(pd, an, date_range, file='ui_temp.rda')
              # save(pd, file = '/tmp/tmp.RData')
              op <- input$only_pending
              if(is.null(op)){
                op <- FALSE
              }
              # save(an, op, corrections, file = '/tmp/an.RData')
              
              if(op){
                anx <- an %>%
                  left_join(corrections %>% dplyr::select(id, submitted_at)) %>%
                  filter(is.na(submitted_at))
              } else {
                anx <- an
              }
              by_fw_type <-anx %>%
                mutate(date = as.Date(date)) %>%
                filter(date >=date_range[1], 
                       date <= date_range[2]) %>%
                group_by(`FW ID` = wid, Description = description) %>%
                summarise(Occurrences = n()) %>%
                arrange(`FW ID`, Description)
              
              by_type <- anx %>%
                mutate(date = as.Date(date))
              by_type$date[is.na(by_type$date)] <- Sys.Date()
              by_type <- by_type %>%
                filter(date >=date_range[1], 
                       date <= date_range[2]) %>%
                group_by(description) %>%
                summarise(Occurrences = n()) %>%
                arrange(desc(Occurrences)) %>%
                mutate(`%` = round(Occurrences / sum(Occurrences) *100, digits = 2))
              
              
              # for every date that has mulitple dates seperated by comma, split and create new row.
              an <- an %>% 
                mutate(date = strsplit(as.character(date), ",")) %>% 
                unnest(date) %>%
                group_by(date) %>% 
                summarise(`# of anomalies` = sum(type == 'error', na.rm=TRUE),
                          `# of errors` = sum(type == 'anomaly', na.rm=TRUE)) %>%
                gather(key=key, value=value, -date) %>%
                mutate(date = as.Date(date)) %>%
                filter(date >=date_range[1], 
                       date <= date_range[2]) %>%
                group_by(key) %>%
                mutate(`cumulative_value` = cumsum(value))
              a_and_e_plot <- ggplot(an, aes(date, value, fill=key)) +
                geom_bar(stat = 'identity') +
                scale_fill_manual(name = '', 
                                  values = c('black', 'grey')) +
                labs(x='Date', y='') +
                theme_bohemia()
              output$e_and_a_per_day <- renderPlot({a_and_e_plot})
              
              # plot for cumulative errors and anomalies
              a_and_e_plot_cumulative <- ggplot(an, aes(date, cumulative_value, color=key)) +
                geom_line(size = 1.5,alpha=0.7) +
                geom_point(size = 2) +
                scale_color_manual(name = '', 
                                   values = c('black', 'grey')) +
                labs(x='Date', y='') +
                theme_bohemia()
              output$e_and_a_cumulative <- renderPlot({a_and_e_plot_cumulative})
              
              # create data to visualize number of active FW per day. active being submitted a form.
              pd_fw <- pd$minicensus_main %>% group_by(todays_date,wid) %>% summarise(counts=n()) %>%
                group_by(todays_date) %>% summarise(counts = n()) %>%
                filter(todays_date >=date_range[1], 
                       todays_date <= date_range[2])
              fw_active_daily_plot<-  ggplot(pd_fw, aes(todays_date, counts)) + 
                geom_bar(stat = 'identity') +
                geom_label(aes(label=counts)) +
                labs(x = 'Date', 
                     y = 'Number of active FW') +
                theme_bohemia()
              
              output$active_fw_per_day <- renderPlot({fw_active_daily_plot})
              
              end_times <- pd$minicensus_main %>%
                group_by(date_time = lubridate::round_date(end_time, unit = 'hour'),
                         country = hh_country) %>%
                tally
              left <- expand.grid(date_time = seq(
                from=as.POSIXct(min(end_times$date_time)),
                to=as.POSIXct(max(end_times$date_time)),
                by="hour"
              ),
              country = c('Mozambique', 'Tanzania'))
              end_times <- left_join(left, end_times) %>% mutate(n = ifelse(is.na(n), 0, n))
              end_times <- end_times %>% filter(as.Date(date_time) >= date_range[1], date_time <=date_range[2])
              end_times_plot <- ggplot(data = end_times %>% filter(country == the_country),
                                       aes(x = date_time,
                                           y = n)) +
                facet_wrap(~country) +
                geom_area(alpha = 0.3, 
                          fill = 'darkred',
                          color = 'black') +
                theme_bohemia()
              output$end_times_plot <- renderPlot({end_times_plot})
              
              
              
              out_mc <- pd$minicensus_main %>%
                group_by(country = hh_country,
                         date = todays_date) %>%
                summarise(Forms = n())
              out_people <- pd$minicensus_people %>%
                left_join(pd$minicensus_main %>% dplyr::select(country = hh_country,
                                                               date = todays_date,
                                                               instance_id),
                          by = 'instance_id') %>%
                group_by(country,
                         date) %>%
                summarise(People = n())
              out_enumerations <- pd$enumerations %>%
                group_by(country, date = todays_date) %>%
                summarise(`Enumerations` = n())
              out_va <- pd$va %>%
                group_by(country = the_country,
                         date = todays_date) %>%
                summarise(`Va forms` = n())
              out_va$country <- the_country
              out_refusals <- pd$refusals %>%
                mutate(reason_no_participate = ifelse(reason_no_participate %in% 
                                                        c('SEM COMENTARIO',
                                                          'He didnt want to do it',
                                                          'Dont know', 'refused'),
                                                      'refused',
                                                      'not_present')) %>%
                group_by(country,
                         date = todays_date) %>%
                summarise(Absences = length(which(reason_no_participate == 'not_present')),
                          Refusals = length(which(reason_no_participate == 'refused')))
              # Join all
              all_dates <- c(out_mc$date, out_people$date, out_enumerations$date, out_va$date, out_refusals$date)
              left <- expand.grid(date = seq(min(all_dates,na.rm = TRUE),
                                             max(all_dates, na.rm = TRUE),
                                             by = 1),
                                  country = c('Mozambique', 'Tanzania'))
              joined <-
                left %>% left_join(out_mc, by = c('country', 'date')) %>%
                left_join(out_people, by = c('country', 'date')) %>%
                left_join(out_enumerations, by = c('country', 'date')) %>%
                left_join(out_va, by = c('country', 'date')) %>%
                left_join(out_refusals, by = c('country', 'date')) %>%
                mutate(Forms = ifelse(is.na(Forms), 0, Forms),
                       People = ifelse(is.na(People), 0, People),
                       Enumerations = ifelse(is.na(Enumerations), 0, Enumerations),
                       `Va forms` = ifelse(is.na(`Va forms`), 0, `Va forms`),
                       Absences = ifelse(is.na(Absences), 0, Absences),
                       Refusals = ifelse(is.na(Refusals), 0, Refusals))
              joined <- joined %>% filter(country == the_country) %>%  filter(date >=date_range[1], 
                                                                              date <= date_range[2])
              
              
              fluidPage(
                fluidRow(
                  h3('Plot of form end times (minicensus)'),
                  plotOutput('end_times_plot'),
                ),
                fluidRow(
                  h3('Active FWs per day'),
                  plotOutput('active_fw_per_day')
                ),
                fluidRow(
                  h3('Errors and anomalies per day'),
                  plotOutput('e_and_a_per_day')
                ),
                fluidRow(
                  h3('Errors and anomalies (cumulative)'),
                  plotOutput('e_and_a_cumulative')
                ),
                fluidRow(
                  h3('Errors and Anomalies resolutions'),
                  column(6,
                         plotOutput('ea1')),
                  column(6,
                         plotOutput('ea2'))
                  
                ),
                fluidRow(
                  column(12,
                         h3('Summary table'),
                         bohemia::prettify(joined,
                                           nrows = nrow(joined),
                                           download_options = TRUE))
                  
                ),
                fluidRow(
                  column(12,
                         h3('Anomalies by fieldworker and type'),
                         bohemia::prettify(by_fw_type,
                                           nrows = nrow(by_type),
                                           download_options = TRUE)
                  )
                ),
                fluidRow(
                  column(12,
                         h3('Anomalies by type'),
                         bohemia::prettify(by_type,
                                           nrows = nrow(by_type),
                                           download_options = TRUE))
                )
              ) 
            }
            
    )
  })
  
  # Consent verification list UI ##########################################
  output$ui_consent_verification_list_a <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'consent_verification_list' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              fluidPage(
                fluidRow(
                  column(4,
                         dateRangeInput('verification_date_filter',
                                        'Filter by date',
                                        start = as.Date('2020-09-20'),
                                        end = Sys.Date())),
                  column(4,
                         checkboxInput('verification_all',
                                       'All fieldworkers?',
                                       value = TRUE)),
                  column(4,
                         checkboxInput('workers_on_separate_pages',
                                       'Print workers on separate pages?',
                                       value = TRUE))
                )
              )
              
            })
  })
  
  
  output$ui_verification_text_filter <- renderUI({
    pd <- odk_data$data
    pd <- pd$minicensus_main
    verification_all <- input$verification_all
    if(is.null(verification_all)){
      out <- NULL
    } else {
      if(!verification_all){
        # Get the country
        co <- input$geo
        co <- ifelse(co == 'Rufiji', 'Tanzania', 'Mozambique')
        pd <- pd %>% dplyr::filter(hh_country == co)
        wid <- sort(unique(pd$wid))
        wid <- wid[!is.na(wid)]
        out <- selectInput('verification_text_filter',
                           'Filter by FW code (all, by default)',
                           choices = wid,
                           selected = wid,
                           multiple = TRUE)
      } else {
        out <- NULL
      }
    }
    return(out)
    
  })
  
  consent_verification_list_reactive <- reactiveValues(data = NULL)
  
  quality_control_list_reactive <- reactiveValues(data = NULL)
  output$ui_consent_verification_list <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'consent_verification_list' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              
              # Get the odk data
              pd <- odk_data$data
              people <- pd$minicensus_people
              subs <- pd$minicensus_repeat_hh_sub
              pd <- pd$minicensus_main
              # Get the country
              co <- input$geo
              co <- ifelse(co == 'Rufiji', 'Tanzania', 'Mozambique')
              pd <- pd %>% dplyr::filter(hh_country == co)
              # save(pd, people, subs, co, people, file = '/tmp/joe.RData')
              # Get hh head
              out <- pd %>%
                dplyr::mutate(num = as.character(hh_head_id)) %>%
                # get info on heads
                left_join(people %>% 
                            mutate(num = as.character(num)) %>%
                            mutate(person_name = initials),
                          by = c('instance_id', 'num')) %>%
                # get info on fieldworkers
                left_join(fids %>% 
                            mutate(wid_name = paste0(first_name, ' ', last_name)) %>%
                            dplyr::select(wid = bohemia_id, wid_name)) %>%
                # get info on hh head subs
                left_join(subs %>% filter(!is.na(hh_sub_id)) %>%  
                            mutate(hh_sub_id = as.character(hh_sub_id)) %>%
                            dplyr::distinct(instance_id, .keep_all = TRUE) %>%
                            dplyr::select(instance_id, hh_sub_id)) %>%
                left_join(people %>% 
                            mutate(hh_sub_id = as.character(num)) %>%
                            mutate(sub_id = permid) %>%
                            mutate(sub_name = initials) %>%
                            dplyr::select(instance_id, hh_sub_id, sub_name, sub_id),
                          by = c('instance_id', 'hh_sub_id')) %>%
                dplyr::select(
                  todays_date,
                  wid,
                  wid_name,
                  hh_hamlet,
                  hh_hamlet_code,
                  hh_id,
                  permid,
                  person_name,
                  sub_id,
                  sub_name) %>%
                mutate(icf_exists = 'Sim__ Não__',
                       reason_no_exists = '',
                       icf_correct = 'Sim__ Não__',
                       reason_no_correct = '',
                       error_resolved = 'Sim__ Não__',
                       verified_by = '') 
              
              verification_all <- input$verification_all
              if(!is.null(verification_all)){
                if(!verification_all){
                  text_filter <- input$verification_text_filter
                  if(!is.null(text_filter)){
                    out <- out %>%
                      dplyr::filter(wid %in% text_filter)
                  }
                }
              }
              
              
              date_filter <- input$verification_date_filter
              if(!is.null(date_filter)){
                out <- out %>%
                  dplyr::filter(
                    todays_date <= date_filter[2],
                    todays_date >= date_filter[1]
                  )
              }
              
              # Location code filtering
              lc <- location_code()
              if(length(lc) > 0){
                out <- out %>%
                  filter(hh_hamlet_code %in% lc)
              }
              
              # Split by workers or by date
              workers_on_separate_pages <- input$workers_on_separate_pages
              if(is.null(workers_on_separate_pages)){
                workers_on_separate_pages <- TRUE
              }
              if(workers_on_separate_pages){
                out <- out %>% arrange(wid)
              } else {
                out <- out %>% arrange(todays_date)
              }
              
              # Render QC stuf
              qc <- out %>%  
                dplyr::select(`Hamlet code` = hh_hamlet_code,
                              `Worker code` = wid,
                              `Household ID` = hh_id, 
                              `HH Head ID` = permid,
                              Date = todays_date)
              quality_control_list_reactive$data <- qc
              
              # get inputs for slider to control sample size
              min_value <- 1
              max_value <- nrow(qc)
              
              selected_value <- max_value#sample(min_value:max_value, 1)
              if(co == 'Mozambique'){
                # here
                names(out) <- c(
                  'Data de recolha de dados',
                  'ID inquiridor',
                  'Nome do inquiridor',
                  'Bairro',
                  'ID_Bairro',
                  'Agregado',
                  'ID_Chefe de agregado',
                  'Nome do Chefe de Agregado',
                  'ID_Chefe de Agregado Substituto',
                  'Nome do Chefe de Agregado Substituto',
                  'O Consentimento Informado(CI) existe?',
                  'Se não existe, verifique com o supervisor e escreva a data caso encontre o CI',
                  'CI preenchido correctamente?',
                  'Se não estiver preenchido correctramente, indicar o erro (ver tabela tipos de erros)',
                  'O erro foi resolvido?',
                  'Verificado por (iniciais da arquivista e data)')
                
              } else {
                out <- out %>%
                  mutate(icf_exists = 'Y__ N__',
                         icf_correct = 'Y__ N__',
                         error_resolved = 'Y__ N__')
                names(out) <- 
                  c(
                    'Data collection date',
                    'FW ID',
                    'FW name',
                    'Hamlet',
                    'Hamlet ID',
                    'HH',
                    'HH head ID',
                    'HH head name',
                    'HH head sub ID',
                    'HH head sub name',
                    'ICF exists?',
                    'If not, verify with supervisor and write date of retrieval',
                    'ICF correctly filled?',
                    'If not, indicate error',
                    'Error resolved?',
                    'Verified by (initials of archivist and date)')
                
                
              }
              message('---Created visit control sheet')
              
              
              consent_verification_list_reactive$data <- out
              fluidPage(
                fluidRow(
                  downloadButton('render_consent_verification_list',
                                 'Print consent verification list')
                ),
                br(),
                br(),
                fluidRow(
                  h3('Quality control check'),
                  p('Select the number of forms to sample (below) and filter by date (above). Then, click "Quality control check" to generate a random table of forms to check on.'),
                  sliderInput('sample_num', 'Number of forms to sample',
                              min = min_value,
                              max = max_value,
                              value = selected_value, step = 1),
                  actionButton('quality_control_check',
                               'Quality control check'),
                  DT::dataTableOutput('quality_control_table')
                )
              )
            }
    )
  }
  )
  
  
  # Demography UI  ###################################################
  output$ui_demography <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'demography' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = 
              fluidPage(
                # h3('LOGGED IN. This page is under construction..')
              )
    )
  })
  
  # Socioeconomics UI  ###############################################
  output$ui_socioeconomics <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'socioeconomics' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = 
              fluidPage(
                # h3('LOGGED IN. This page is under construction..')
              )
    )
  })
  
  # Veterinary UI  ###################################################
  output$ui_veterinary <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'veterinary' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = 
              fluidPage(
                # h3('LOGGED IN. This page is under construction..')
              )
    )
  })
  
  # Environment UI  ##################################################
  output$ui_environment <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'environment' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = 
              fluidPage(
                # h3('LOGGED IN. This page is under construction..')
              )
    )
  })
  
  # Health UI  #######################################################
  output$ui_health <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'health' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = 
              fluidPage(
                # h3('LOGGED IN. This page is under construction..')
              )
    )
  })
  
  # Malaria UI  ######################################################
  output$ui_malaria <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'malaria' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = 
              fluidPage(
                # h3('LOGGED IN. This page is under construction..')
              )
    )
  })
  
  ###########################################################################
  # UI ELEMENTS
  ###########################################################################
  
  # Meta UI elements (ie, those that go across tabs) ####################
  
  # Log in / out button in the upper right
  output$log_ui <- renderUI({
    li <- session_info$logged_in
    make_log_in_button(li)
  })
  
  anomalies_dt <- reactive({
    action <- session_data$anomalies
    # save(action, '/tmp/action_table.RData')
    # Get supervisor
    action <- action %>%
      left_join(fids %>% dplyr::mutate(fw_name = paste0(first_name, ' ', last_name))  %>%
                  mutate(wid = as.character(bohemia_id)) %>%
                  dplyr::select(wid, supervisor))
    action <- action %>% dplyr::rename(FW = wid)
    # Join with the already existing fixes and remove those for which a fix has already been submitted
    corrections <- odk_data$data$corrections
    fixes <- odk_data$data$fixes
    # odk_data <- odk_data$data
    # save(action, corrections, fixes, odk_data, file = '/tmp/this.RData')
    
    # Join tables together
    joined <- left_join(action,
                        corrections %>% dplyr::select(-instance_id))
    joined <- left_join(joined, fixes)
    
    joined$technical_date <- unlist(lapply(strsplit(as.character(joined$date), ','), function(x){x[length(x)]}))
    joined$days_ago <- Sys.Date() - as.Date(joined$technical_date)
    joined$technical_date <- NULL
    joined$resolution_submitted <- !is.na(joined$submitted_at)
    joined$fix_implemented <- !is.na(joined$done_by)
    # Reorder a bit
    joined <- joined %>% 
      dplyr::select(type, days_ago, country, FW, date, id, description, incident, instance_id, supervisor,
                    hamlet_code,
                    response_details,
                    resolved_by,
                    resolution_method,
                    submitted_by,
                    submitted_at,
                    done_by,
                    done_at,
                    resolution_submitted,
                    fix_implemented)
    hide_submitted <- input$hide_submitted
    if(!is.null(hide_submitted)){
      if(hide_submitted){
        joined <- joined %>% filter(!resolution_submitted) 
      }
    }
    anomalies_show <- input$anomalies_show
    message('anomalies show is:')
    # if(!is.null(anomalies_show)){
    joined <- joined %>% filter(type %in% anomalies_show)
    # }
    # save(joined, file = '/tmp/anomalies_dt.RData')
    return(joined)
  })
  
  # Action table
  output$anomalies_table <- DT::renderDataTable({
    joined <- anomalies_dt()
    bohemia::prettify(joined, 
                      download_options = TRUE,
                      nrows = nrow(joined)) %>%
      DT::formatStyle(
        'Type',
        backgroundColor = styleEqual(c('Error', 'Anomaly'),
                                     c('red', 'orange'))
      ) %>%
      DT::formatStyle(
        'Resolution submitted',
        backgroundColor = styleEqual(c(TRUE),
                                     c('#FFFACD')),
        target = 'row'
      ) %>%
      DT::formatStyle(
        'Fix implemented',
        backgroundColor = styleEqual(c(TRUE),
                                     c('#98FB98')),
        target = 'row'
      )
  },
  options = list(scrollX = TRUE))
  
  
  # Map of all forms filled out
  output$field_monitoring_map_forms <- 
    renderLeaflet({
      fake_map(tile = 'Esri.WorldImagery',
               with_points = 1500,
               with_polys = FALSE)
    })
  
  # Map of estimated coverage
  output$field_monitoring_map_coverage <- 
    renderLeaflet({
      fake_map(with_points = 0,
               with_polys = TRUE)
    })
  
  output$individual_details <- 
    renderTable({
      NULL
    })
  
  # observe event for action button quality_control_check
  quality_control_table_data <- reactiveValues(data = NULL)
  observeEvent(input$quality_control_check, {
    # Get the data
    sample_num = input$sample_num
    qcx <- quality_control_list_reactive$data
    if(is.null(qcx)|is.null(sample_num)){
      NULL
    } else {
      # sample half data (replace with input)
      if(nrow(qcx)>1){
        if(sample_num > nrow(qcx)){
          sample_index <- 1:nrow(qcx)
        } else {
          sample_index <- sample(1:nrow(qcx), sample_num, replace = FALSE)
        }
        qcx <- qcx[sample_index,]
      }
      quality_control_table_data$data <- qcx
    }
    
  })
  
  output$quality_control_table <- DT::renderDataTable({
    qct <- quality_control_table_data$data
    if(is.null(qct)){
      NULL
    } else {
      bohemia::prettify(qct,
                        download_options = TRUE,
                        nrows = nrow(qct))
    }
  })
  
  # Errors and anomalies charts
  output$ea1 <- renderPlot({
    anomalies <- session_data$anomalies
    # # Get supervisor
    # anomalies <- anomalies %>%
    #   left_join(fids %>% dplyr::mutate(fw_name = paste0(first_name, ' ', last_name))  %>%
    #               mutate(wid = as.character(bohemia_id)) %>%
    #               dplyr::select(wid, supervisor))
    # anomalies <- anomalies %>% dplyr::rename(FW = wid)
    # Join with the already existing fixes and remove those for which a fix has already been submitted
    corrections <- odk_data$data$corrections
    fixes <- odk_data$data$fixes
    # save(anomalies, corrections, fixes, file = '/tmp/this.RData')
    
    joined <- left_join(anomalies,
                        corrections %>% dplyr::select(-instance_id))
    joined <- left_join(joined, fixes)
    
    pd <- joined %>%
      mutate(status = ifelse(!is.na(done_by), 'Done',
                             ifelse(!is.na(response_details), 'Response submitted',
                                    'Needs response'))) %>%
      group_by(category = status) %>%
      tally %>%
      ungroup %>%
      mutate(fraction = n/ sum(n)) %>%
      mutate(p = paste0(round(fraction * 100, digits = 1), '%')) %>%
      mutate(ymax = cumsum(fraction)) %>%
      mutate(ymin = c(0, head(ymax, n = -1))) %>%
      mutate(label_position = (ymax + ymin) / 2) %>%
      mutate(label = paste0(category, '\nN: ', n, ' (', p, ')'))
    
    cols <- c('darkgreen', grey(0.5),'darkorange')
    ggplot(data = pd, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2, fill=category)) +
      geom_rect(alpha = 0.6) +
      ggrepel::geom_text_repel( x=5, aes(y=label_position, label=label, color = category), size=4) +
      coord_polar(theta="y") +
      xlim(c(-1, 4)) +
      theme_void() +
      # scale_fill_brewer(palette=4) +
      scale_fill_manual(name = '',
                        values = cols) +
      scale_color_manual(name = '',
                         values = cols) +
      theme(legend.position = "none") +
      labs(x = '',
           y = '',
           title = 'Anomalies/errors status')
  })
  output$ea2 <- renderPlot({
    anomalies <- session_data$anomalies
    # # Get supervisor
    # anomalies <- anomalies %>%
    #   left_join(fids %>% dplyr::mutate(fw_name = paste0(first_name, ' ', last_name))  %>%
    #               mutate(wid = as.character(bohemia_id)) %>%
    #               dplyr::select(wid, supervisor))
    # anomalies <- anomalies %>% dplyr::rename(FW = wid)
    # Join with the already existing fixes and remove those for which a fix has already been submitted
    corrections <- odk_data$data$corrections
    fixes <- odk_data$data$fixes
    
    joined <- left_join(anomalies,
                        corrections %>% dplyr::select(-instance_id))
    joined <- left_join(joined, fixes)
    
    # save(joined, file='temp_joined.rda')
    pd <- joined %>%
      mutate(status = ifelse(!is.na(done_by), 'Done',
                             ifelse(!is.na(response_details), 'Response submitted',
                                    'Needs response'))) %>%
      mutate(week = lubridate::week(date)) %>%
      group_by(week) %>%
      mutate(week_label = paste0(min(date))) %>%
      group_by(category = status, date = week_label) %>%
      tally %>%
      ungroup 
    cols <- c(grey(0.5),'darkorange', 'darkgreen')
    pd$category <- factor(pd$category,
                          levels = c('Needs response',
                                     'Response submitted',
                                     'Done'))
    # save(pd, file = 'temp_pd.rda')
    ggplot(data = pd,
           aes(x = date,
               y = n,
               group = category,
               fill = category)) +
      geom_bar(stat = 'identity') +
      scale_fill_manual(name = '',
                        values = cols,
                        guide = guide_legend(reverse = TRUE)) +
      labs(x = 'Week of anomaly occurrence',
           y = 'Number of anomalies',
           title = 'Anomalies/errors and status over time') +
      theme_bohemia() +
      theme(axis.text.x = element_text(angle=45, hjust=1, size = 14))
  })
  
  # Alert ui
  uploaded_data <- reactiveValues(data_text ='No data uploaded')
  
  observeEvent(input$upload_data, {
    if (is.null(input$upload_data)){
      NULL
    } else {
      ok <- TRUE
      inFile <- input$upload_data
      dat <- read.csv(inFile$datapath, check.names = FALSE)
      previous_corrections <- odk_data$data$corrections
      # save(dat, previous_corrections, file='/tmp/temp_upload.rda')
      
      # create valid names 
      log_in_user <- input$log_in_user
      
      valid_names <- c("Instance id", "Id", "Response details", "Resolved by", "Resolution method")  
      
      # Check column names
      if(all(valid_names %in% names(dat))){
        ok_names <- TRUE
      } else {
        ok_names <- FALSE
        ok <- FALSE
        out_text <- paste0('ERROR! The following required columns are missing: ', paste0(valid_names[which(!valid_names %in% names(dat))], collapse = '; '), collapse = ' ')
      }
      
      # Check duplicates
      if(ok){
        any_duplicates <- any(duplicated(dat$Id))
        if(any_duplicates){
          ok <- FALSE
          out_text <- 'ERROR! Multiple entries per anomaly. No duplicated "Ids" allowed.'
        }
      }
      
      # Check to see if everything is complete
      if(ok){
        sub_dat <- dat[,valid_names]
        any_nas <- as.numeric(which(apply(sub_dat, 2, function(x){any(is.na(x))})))
        any_empties <- as.numeric(which(apply(sub_dat, 2, function(x){any(x == '')})))
        any_nas <- sort(unique(c(any_nas, any_empties)))
        if(length(any_nas) > 0){
          ok <- FALSE
          out_text <- paste0('ERROR! Empty values are not permitted. There are empty values in the following ', length(any_nas), ' columns. Fill out all cells in these columns and then re-upload: ', paste0(paste0(names(sub_dat)[any_nas], collapse = '; ')))
        }
      }
      
      # Check to see if anything has already been resolved
      if(ok){
        sub_dat <- dat[,valid_names]
        sub_dat$Id <- tolower(sub_dat$Id)
        already_ids <- previous_corrections$id
        already <- which(sub_dat$Id %in% already_ids)
        if(length(already) > 0){
          ok <- FALSE
          out_text <- paste0('ERROR! Column names are correct, but there are entries for anomalies which have ALREADY been correted. Please remove rows with the following', length(already),  ' IDs and then re-upload: ', paste0(paste0(sub_dat$Id[already], collapse = ', ')))
        }
      }
      if(ok){
        out_text <- 'Successfully uploaded corrections requests.'
        
        fix <- dat %>%
          mutate(id = tolower(Id),
                 instance_id = tolower(`Instance id`),
                 response_details = `Response details`,
                 resolved_by = `Resolved by`,
                 resolution_method = `Resolution method`,
                 resolution_date = as.character(Sys.Date()),
                 submitted_by = log_in_user,
                 submitted_at = Sys.time()) %>%
          dplyr::select(id, instance_id, response_details, resolved_by,
                        resolution_method, resolution_date, submitted_by, submitted_at)
        # con <- get_db_connection(local = is_local)
        dbAppendTable(conn = con,
                      name = 'corrections',
                      value = fix)
        message('Done. now disconnecting from database')
        # dbDisconnect(con)
        # AND THEN MAKE SURE TO UPDATE THE IN-SESSION STUFF
        message('Now uploading the in-session data')
        old_corrections <- odk_data$data$corrections 
        new_correction <- fix
        new_corrections <- bind_rows(old_corrections, new_correction)
        odk_data$data$corrections  <- new_corrections
      } 
      status_text <- ifelse(ok, 'SUCCESS!', 'ERROR!')
      uploaded_data$data_text <- out_text
      showModal(modalDialog(
        title = status_text,
        paste0(uploaded_data$data_text),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  output$upload_data_message <- renderTable({
    this_text <- uploaded_data$data_text
    data_frame(' '=this_text)
  })
  # sh
  output$anomalies_ui_a <- renderUI({
    
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'malaria' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              # fluidPage(
              #   h1('Temporarily down for maintenance'),
              #   add_busy_gif(src = "https://jeroen.github.io/images/banana.gif", 
              #                position = 'bottom-right',
              #                height = '100px',
              #                width = '100px',
              #                margins = c(350, 350))
              # )
              fluidPage(
                fluidRow(column(12, align = 'center',
                                'Select one row and then click "Submit response"',
                                actionButton('submit_fix',
                                             'Submit response',
                                             style='padding:=8px; font-size:120%'))),
                fluidRow(
                  column(3),
                  column(3, align = 'center',
                         checkboxInput('hide_submitted',
                                       'Hide rows for which a resolution has already been submitted?',
                                       value = FALSE)),
                  column(3,align = 'center',
                         checkboxGroupInput('anomalies_show',
                                            '',
                                            choices = c('Anomalies' = 'anomaly',
                                                        'Errors' = 'error'),
                                            selected = c('anomaly', 'error'),
                                            inline = TRUE
                                            # choiceNames = list(
                                            #   icon('exclamation-circle'),
                                            #   icon('question-circle')
                                            # ),
                                            # choiceValues = c('anomaly', 'error')
                         )),
                  column(3)
                  
                  
                ))
            }
    )
  })
  
  
  output$anomalies_ui <- renderUI({
    
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'malaria' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              # fluidPage(
              #   h1('Temporarily down for maintenance'),
              #   add_busy_gif(src = "https://jeroen.github.io/images/banana.gif", 
              #                position = 'bottom-right',
              #                height = '100px',
              #                width = '100px',
              #                margins = c(350, 350))
              # )
              fluidPage(
                fluidRow(
                  box(width = 12,
                      # icon = icon('table'),
                      color = 'orange',
                      div(DT::dataTableOutput('anomalies_table'), style = "font-size:60%"))
                  
                ))
            }
    )
  })
  
  output$download_ui <- renderUI({
    # See if the user is logged in and has access
    si <- session_info
    li <- si$logged_in
    ac <- 'malaria' %in% si$access
    # Generate the ui
    make_ui(li = li,
            ac = ac,
            ok = {
              fluidPage(
                fluidRow(
                  column(6,
                         selectInput('which_download',
                                     'Pick a dataset',
                                     choices = c('Minicensus main',
                                                 'Minicensus enumerations',
                                                 'Minicensus refusals',
                                                 'Minicensus people roster',
                                                 'Minicensus death registry',
                                                 'Minicensus HH subs',
                                                 'Minicensus mosquito nets',
                                                 'Minicensus water',
                                                 'VA',
                                                 'VA (full)',
                                                 'Modifications'
                                     ))
                  ),
                  column(6,
                         downloadButton("download_dataset", "Download dataset")
                  )
                )
              )
            })
  })
  
  
  
  output$download_dataset <- downloadHandler(
    filename = function(){
      paste0(input$which_download, ".csv", sep = "")
    },
    content = function(file){
      
      which_download <- input$which_download
      the_country <- country()
      if(the_country == 'Mozambique'){
        the_server <- 'https://sap.manhica.net:4442/ODKAggregate'
      } else {
        the_server <- 'https://bohemia.ihi.or.tz'
      }
      
      
      if(which_download == 'Minicensus main'){
        df <- odk_data$data$minicensus_main
      } else if(which_download == 'Minicensus enumerations'){
        df <- odk_data$data$enumerations
      } else if(which_download == 'Minicensus refusals'){
        df <- odk_data$data$refusals
      } else if(which_download == 'Minicensus people roster'){
        df <- odk_data$data$minicensus_people
      } else if(which_download == 'Minicensus death registry'){
        df <- odk_data$data$minicensus_repeat_death_info
      } else if(which_download == 'Minicensus HH subs'){
        df <- odk_data$data$minicensus_repeat_hh_sub
      } else if(which_download == 'Minicensus mosquito nets'){
        df <- odk_data$data$minicensus_repeat_mosquito_net
      } else if(which_download == 'Minicensus water'){
        df <- odk_data$data$minicensus_repeat_water
      } else if(which_download == 'VA'){
        df <- odk_data$data$va
      } else if(which_download == 'VA (full)'){
        df <- dbGetQuery(conn = con,
                         statement = paste0("SELECT * FROM va WHERE server = '", the_server, "'"))
      } else if(which_download == 'Modifications'){
        df <- odk_data$data$fixes
      } 
      write.csv(df,
                file,
                row.names = FALSE)
    }
  )
  
  output$render_control_sheet <-
    downloadHandler(filename = "sheet.pdf",
                    content = function(file){
                      # Get whether logged in
                      si <- session_info
                      li <- si$logged_in
                      xdata <- session_data$va_table
                      co <- country()
                      cs <-
                        ifelse(co == 'Tanzania',
                               "column_spec(1:ncol(xdata),width = '1.5cm')",
                               "column_spec(1:ncol(xdata),width = '1.5cm')")
                      
                      out_file <- paste0(getwd(), '/control_sheet.pdf')
                      rmarkdown::render(input = 
                                          paste0(system.file('rmd', package = 'bohemia'), '/control_sheet.Rmd'),
                                        # '../inst/rmd/control_sheet.Rmd',
                                        output_file = out_file,
                                        params = list(xdata = xdata,
                                                      li = li, font_size = 6,
                                                      # column_spec = "column_spec(1:6, background = '#ffcccb') %>% column_spec(7:10, background = '#fed8b1') %>% column_spec(11:(ncol(xdata)-1), background = '#ADD8E6') %>% column_spec(1:ncol(xdata),width = '1.1cm')"))
                                                      column_spec = cs))
                      
                      # copy html to 'file'
                      file.copy(out_file, file)
                      
                      # # delete folder with plots
                      # unlink("figure", recursive = TRUE)
                    },
                    contentType = "application/pdf"
    )
  
  output$download_visit_control_data <- downloadHandler(
    filename = function(){
      paste0('visit_control_sheet', ".csv", sep = "")
    },
    content = function(file){
      si <- session_info
      
      li <- si$logged_in
      #Processing of visit control data
      lc <- location_code()
      # Get other details
      enumeration_or_minicensus <- input$enumeration_or_minicensus
      enum <- enumeration_or_minicensus == 'Enumeration visit'
      use_previous <- enumeration_or_minicensus == 'Data collection visit'
      xdata <- data.frame(n_hh = as.numeric(as.character(input$enumeration_n_hh)),
                          n_teams = as.numeric(as.character(input$enumeration_n_teams)),
                          id_limit_lwr = as.numeric(as.character(input$id_limit[1])),
                          id_limit_upr = as.numeric(as.character(input$id_limit[2])))
      enumerations_data = odk_data$data$enumerations
      minicensus_main_data <- odk_data$data$minicensus_main
      refusals_data = odk_data$data$refusals
      loc_id = lc
      enumeration = enum
      
      # save(li, lc, enumeration_or_minicensus, enum, use_previous, xdata, enumerations_data,
      #      minicensus_main_data, refusals_data,
      #      loc_id, enumeration, file = '/tmp/spreadsheet.RData')
      
      
      x <- bohemia::locations
      x <- x %>% filter(code %in% loc_id)
      if(nrow(x) > 0){
        loc_name <- paste0(x$Ward, ', ', x$Village, ', ', x$Hamlet)
      } else {
        loc_name <- ' '
      }
      include_name <- FALSE
      
      
      lc <- loc_id
      n_hh <- as.numeric(xdata$n_hh)
      if(is.na(n_hh)){
        n_hh <- 1000
      }
      n_teams <- as.numeric(xdata$n_teams)
      id_limit_lwr <- as.numeric(xdata$id_limit_lwr)
      id_limit_upr <- as.numeric(xdata$id_limit_upr)
      
      # Get country
      country <- 'Mozambique'
      if(lc[1] %in% locations$code[locations$Country == 'Tanzania']){
        country <- 'Tanzania'
      }
      
      id_vals <- 1:n_hh
      id_vals <- id_vals[id_vals %in% id_limit_lwr:id_limit_upr]
      n_hh <- length(id_vals)
      
      team_numbers <- rep(1:n_teams, each = round(n_hh / n_teams))
      while(length(team_numbers) < n_hh){
        team_numbers <- c(team_numbers, team_numbers[length(team_numbers)])
      }
      
      while(length(team_numbers) > n_hh){
        team_numbers <- team_numbers[1:n_hh]
      }
      
      
      if(country == 'Tanzania'){
        out_list <- list()
        for(lcx in 1:length(lc)){
          this_out <- tibble(`HHID` = paste0(lc[lcx], '-', bohemia::add_zero(id_vals, n = 3)),
                             team = team_numbers)
          out_list[[lcx]] <- this_out
        }
        out <-bind_rows(out_list)
        
        left <- locations %>% filter(code %in% lc) %>% dplyr::select(code, District, Ward, Village, Hamlet)
        out$code <- substr(out$`HHID`, 1, 3)
        out = left_join(out, left, by = 'code') %>% dplyr::select(-code)
      } else {
        out_list <- list()
        for(lcx in 1:length(lc)){
          this_out <- tibble(`Código do agregado` = paste0(lc[lcx], '-', bohemia::add_zero(id_vals, n = 3)),
                             team = team_numbers) %>%
            arrange(`Código do agregado`)
          out_list[[lcx]] <- this_out
        }
        out <-bind_rows(out_list)
      }
      
      if(li){
        if(country == 'Mozambique'){
          if(enumeration){
            # ENUMERATION
            df <- out %>% dplyr::mutate(`Nome de chefe de agregado` = ' ', `Localização do Numero de Agregado` = ' ', `Data de enumeração` = ' ')
          } else {
            # NON ENUMERATION
            # We use the previously enumerated data then and have to remove absences / etc
            refusals <- refusals_data %>%
              mutate(reason_no_participate = ifelse(reason_no_participate %in% c('SEM COMENTARIO',
                                                                                 'He didnt want to do it',
                                                                                 'Dont know'),
                                                    'refused',
                                                    'not_present')) %>%
              group_by(hh_id, reason_no_participate) %>%
              tally
            # Define those who should not be visited again
            remove_these <- refusals %>% filter(reason_no_participate == 'refused' | length(which(reason_no_participate == 'not_present')) >= 3) %>%
              .$hh_id
            # Define those which have already been mini-censed (and can therefore be removed)
            remove_these_mc <- minicensus_main_data$hh_id
            remove_these <- unique(c(remove_these, remove_these_mc))
            # Define those with previous absences
            previous_absences <- refusals %>%
              group_by(agregado = hh_id) %>%
              filter(length(which(reason_no_participate == 'refused')) == 0) %>%
              filter(reason_no_participate == 'not_present') %>%
              summarise(n = n())
            this_df <- enumerations_data %>% dplyr::filter(hamlet_code %in% lc) %>% dplyr::select(
              agregado, village, ward, hamlet, hamlet_code, localizacao_agregado, todays_date, chefe_name, wid, hamlet_code) %>%
              filter(!is.na(agregado)) %>%
              dplyr::distinct(agregado, .keep_all = TRUE) %>%
              left_join(previous_absences) %>%
              mutate(`Ausências anteriores` = ifelse(is.na(n), 0, n)) %>%
              dplyr::select(-n) %>%
              filter(!agregado %in% remove_these) %>%
              arrange(agregado)
            
            if(!is.null(df)){
              if(!include_name & nrow(this_df) > 0){
                this_df$chefe_name <- ' '
              }
            }
            
            # Add team numbers
            nr = nrow(this_df)
            if(nr > 0){
              team_vals <- sort(((1:nr) %% n_teams) + 1)
              this_df$team <- team_vals
              df <- this_df %>%
                dplyr::select(-team) %>% dplyr::select(`Data de enumeração` = todays_date, `ID agregado` = agregado, `Posto administrativo e Localidade` = ward, `Povoado` = village, `Bairro` = hamlet, `ID Bairro` = hamlet_code, `Nome de chefe de agregado` = chefe_name, `Ausências anteriores`) %>% dplyr::mutate(`Data da visita` = ' ') %>%  dplyr::mutate(`O chefe de agregado ou Chefe de agregado sustituto assino o consentimento informado?` = 'Sim__Não__', `Foi realizado o formulario?` = 'Sim__Não__',  `Se Não foi visitado ou entrevistado, explique o porque?` = ' ')
            } else{
              df <- data.frame(a = paste0('No previous enumerated households for ', lc))
            }
          }
        } else {
          # TANZANIA
          df <- out  %>% dplyr::mutate(`Status` = ' ', `Comments` = ' ')
        }
      } else {
        # Not logged in
        df <- data.frame(a = 'Log in first.')
      }
      write.csv(df,
                file,
                row.names = FALSE)
    }
  )
  
  output$render_visit_control_sheet <-
    downloadHandler(filename = "visit_control_sheet.pdf",
                    content = function(file){
                      # Get whether logged in
                      si <- session_info
                      li <- si$logged_in
                      # Get the location code
                      lc <- location_code()
                      # Get other details
                      enumeration_or_minicensus <- input$enumeration_or_minicensus
                      enum <- enumeration_or_minicensus == 'Enumeration visit'
                      use_previous <- enumeration_or_minicensus == 'Data collection visit'
                      xdata <- data.frame(n_hh = as.numeric(as.character(input$enumeration_n_hh)),
                                          n_teams = as.numeric(as.character(input$enumeration_n_teams)),
                                          id_limit_lwr = as.numeric(as.character(input$id_limit[1])),
                                          id_limit_upr = as.numeric(as.character(input$id_limit[2])))
                      enumerations_data = odk_data$data$enumerations
                      minicensus_main_data <- odk_data$data$minicensus_main
                      refusals_data = odk_data$data$refusals
                      
                      
                      # save(refusals_data, minicensus_main_data, enumerations_data,
                      #      xdata, use_previous, enum, enumeration_or_minicensus, lc, li, si, 
                      #      file = '/tmp/tmp.RData')
                      out_file <- paste0(getwd(), '/visit_control_sheet.pdf')
                      rmarkdown::render(input = 
                                          paste0(system.file('rmd', package = 'bohemia'), '/visit_control_sheet.Rmd'),
                                        # '../inst/rmd/visit_control_sheet.Rmd',
                                        output_file = out_file,
                                        params = list(xdata = xdata,
                                                      loc_id = lc,
                                                      enumeration = enum,
                                                      use_previous = use_previous,
                                                      enumerations_data = enumerations_data,
                                                      refusals_data = refusals_data,
                                                      minicensus_main_data = minicensus_main_data,
                                                      li = li))
                      
                      # copy html to 'file'
                      file.copy(out_file, file)
                      
                      # # delete folder with plots
                      # unlink("figure", recursive = TRUE)
                    },
                    contentType = "application/pdf"
    )
  output$render_file_index_list <-
    downloadHandler(filename = "file_list.pdf",
                    content = function(file){
                      
                      # Get the location code
                      lc <- location_code()
                      # Get data
                      pd <- odk_data$data
                      pd <- pd$minicensus_main
                      pd <- pd %>% filter(hh_hamlet_code %in% lc)
                      
                      # Get country
                      # Get country
                      country <- 'Mozambique'
                      if(lc %in% locations$code[locations$Country == 'Tanzania']){
                        country <- 'Tanzania'
                      }
                      # save(pd, file = '/tmp/pd.RData')
                      out <- pd %>%
                        dplyr::select(
                          Ward = hh_ward,
                          Village = hh_village,
                          Hamlet = hh_hamlet,
                          `Hamlet code` = hh_hamlet_code,
                          `HH ID` = hh_id
                        ) %>%
                        mutate(Observations = '  ',
                               `Location of file` =  '   ') %>%
                        dplyr::arrange(`HH ID`)
                      
                      
                      if(country == 'Mozambique'){
                        names(out) <- c("Posto administrativo", "Localidade", "Bairro", "Código Bairro",
                                        "Número do Agregado Familiar",
                                        "Observações",
                                        "Localização da pasta de arquivo")
                      }
                      out_file <- paste0(getwd(), '/file_list.pdf')
                      rmarkdown::render(
                        input = paste0(system.file('rmd', package = 'bohemia'), '/file_list.Rmd'),
                        # input = '../inst/rmd/file_list.Rmd',
                        output_file = out_file,
                        params = list(xdata = out))
                      
                      # copy html to 'file'
                      file.copy(out_file, file)
                      
                    },
                    contentType = "application/pdf"
    )
  
  output$render_consent_verification_list <-
    downloadHandler(filename = "consent_verification_list.pdf",
                    content = function(file){
                      
                      # Get the data
                      pdx <- consent_verification_list_reactive$data
                      # Get page numbering system
                      
                      workers_on_separate_pages <- input$workers_on_separate_pages
                      # save(pdx, file = '/tmp/data.RData')
                      
                      # save(pdx, file = 'pdx_tab.rda')
                      out_file <- paste0(getwd(), '/consent_verification_list.pdf')
                      rmarkdown::render(
                        # input = '../inst/rmd/consent_verification_list.Rmd',
                        input = paste0(system.file('rmd', package = 'bohemia'), '/consent_verification_list.Rmd'),
                        output_file = out_file,
                        params = list(data = pdx,
                                      workers_on_separate_pages = workers_on_separate_pages))
                      
                      # copy html to 'file'
                      file.copy(out_file, file)
                      
                      # # delete folder with plots
                      # unlink("figure", recursive = TRUE)
                    },
                    contentType = "application/pdf"
    )
  
  # Data management UI elements #########################################
  
  # Server status UI elements ###########################################
  
  # Demography UI elements ##############################################
  
  # Socioeconomics UI elements ##########################################
  
  # Veterinary UI elements ##############################################
  
  # Environment UI elements #############################################
  
  # Health UI elements ##################################################
  
  # Malaria UI elements #################################################
  
  session$onSessionEnded(function(){
    cat(paste0('Session ended.'))
    if(exists('con')){
      dbDisconnect(con)
      cat(paste0('Disconnected from database.'))
    }
  })
  
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}

