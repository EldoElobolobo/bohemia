
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
#' @import aws.s3
#' @import readr
#' @import sp

suppressPackageStartupMessages(
  library(shinyjs)
)
library(dplyr)
library(aws.s3)

# Define function for retrieving data
get_data_from_aws <- function(country = 'Mozambique',
                              credentials_dir = 'credentials'){
  # Define the country
  if(country == 'Mozambique'){
    iso <- 'MOZ'
  } else {
    iso <- 'TZA'
  }
  
  # Read in credentials for S3 bucket
  s3creds <- read_csv(file.path(credentials_dir, 'bohemiacensuss3credentials.csv'))
  
  # Read in credentials for ODK Aggregate server
  odk_collect_creds <- yaml::yaml.load_file(file.path(credentials_dir, 'credentials.yaml'))
  
  # Set environment variables for AWS s3
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = s3creds$`Access key ID`,
    "AWS_SECRET_ACCESS_KEY" = s3creds$`Secret access key`,
    "AWS_DEFAULT_REGION" = "eu-west-3"
  )
  
  buck <- get_bucket(bucket = 'bohemiacensus',
                     prefix = 'incidents')
  
  # Retrieve objects from s3
  buck_names <- buck_times <-  c()
  for(i in 1:length(buck)){
    buck_names[i] <- buck[i]$Contents$Key
    buck_times[i] <- buck[i]$Contents$LastModified
  }
  buck_df <- tibble(file = buck_names,
                    date_time = buck_times) %>%
    filter(grepl('incidents/', file))
  buck_df_keep <- buck_df %>%
    arrange(desc(date_time)) %>%
    filter(grepl(country, file)) %>%
    filter(date_time == dplyr::first(date_time))
  
  # Retrieve and save locally
  if(nrow(buck_df_keep) > 0){
    for(i in 1:nrow(buck_df_keep)){
      this_file <- buck_df_keep$file[i]
      this_object_name <- unlist(strsplit(this_file, '_'))[1]
      local_file <- paste0(this_object_name, '.RData')
      save_object(
        object = this_file,
        bucket = 'bohemiacensus',
        file = local_file)
      load(local_file, envir = .GlobalEnv) # load to main namespace
      file.remove(local_file)
    }
  }
  incidents <- incidents %>%
    dplyr::distinct(`meta-instanceID`,
                    .keep_all = TRUE) 
  incidents <- incidents %>%
    mutate(type = `group_reason-kind_of_incident`) %>%
    mutate(lng = `group_location-location-Longitude`,
           lat = `group_location-location-Latitude`) %>%
    mutate(type_other = `group_reason-other_kind_of_incident`) %>%
    mutate(type = ifelse(!is.na(type_other), type_other, type)) %>%
    mutate(x = lng,
           y = lat) %>%
    mutate(details = `group_reason-details`) %>%
    mutate(wid = `group_intro-wid`)
  return(incidents)
}




app_ui <- function(request) {
  options(scipen = '999')
  
  tagList(
    mobile_golem_add_external_resources(),
    
    dashboardPage(
      dashboardHeader (title = "Incidents dashboard"),
      dashboardSidebar(
        sidebarMenu(
          menuItem(
            text="Incidents",
            tabName="incidents"),
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
            tabName="incidents",
            fluidPage(
              # shinyjs::useShinyjs(),
              fluidRow(
                column(6,
                       selectInput('country', 'Country',
                                   choices = c('Mozambique', 
                                               'Tanzania'))),
                column(6,
                       selectInput('type', 'Filter type of incident',
                                   choices = c('Accessibility issue' = 'accessibility_issue',
                                              'Social cultural issue' =   'social_cultural_issue',
                                              'Safety issue' =   'safety_issue',
                                              'Community resistance' =  'community_resistance',
                                               'Crime' = 'crime',
                                              'Other' = 'Other'),
                                   selected = c('Accessibility issue' = 'accessibility_issue',
                                                'Social cultural issue' =   'social_cultural_issue',
                                                'Safety issue' =   'safety_issue',
                                                'Community resistance' =  'community_resistance',
                                                'Crime' = 'crime',
                                                'Other' = 'Other'),
                                   multiple = TRUE)
                
                )
              
              ),
              fluidRow(
                leafletOutput('map')
              ),
              fluidRow(
                DT::dataTableOutput('table')
              )
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
  
  logged_in <- reactiveVal(value = FALSE)
  observeEvent(input$log_in,{
    logged_in(TRUE)
    removeModal()
  })
  
  output$top_button <- renderUI({
    li <- logged_in()
    if(li){
      actionButton("log_out", "Log out")
    } else {
      actionButton("show", "Log in")
    }
  })
  
  observeEvent(input$show, {
    # logged_in(TRUE)
    showModal(modalDialog(
      title = "Log in",
      fluidPage(
        fluidRow(
          column(6,
                 textInput('email', 'Email')),
          column(6,
                 passwordInput('password', 'Password'))
        ),
        fluidRow(
          actionButton('log_in', 'Log in')
        )
      )
    ))
  })
  
  dfr <- reactive({
    country <- input$country
    # type <- input$type
    out <- get_data_from_aws(country = country)
    return(out)
  })
  shpr <- reactive({
    country <- input$country
    if(country == 'Mozambique'){
      shp <- bohemia::mop2
    } else {
      shp <- bohemia::ruf2
    }
    
  })
  
  output$map <- renderLeaflet({
    shp <- shpr()
    the_type <- input$type
    pts <- dfr()
    # save(pts, file = '/tmp/pts.RData')
    pts <- pts %>% filter(type %in% the_type)
    if(!is.null(pts)){
      if(nrow(pts) > 0){
        coordinates(pts) <- ~x+y
        factpal <- colorFactor(topo.colors(length(unique(pts@data$type))), pts@data$type)
        
        
        l <-
          leaflet(data = pts) %>%
          addProviderTiles(providers$OpenStreetMap)
        
        l <- l %>%
          addPolylines(data = shp,
                       color = 'red',
                       weight = 3.5) %>% 
          addCircleMarkers(data = pts,
                           color = ~factpal(pts@data$type),
                           popup = paste0(pts@data$SubmissionDate, '. ', pts@data$details) ) %>%
          addLegend(pal = factpal, values = pts@data$type, opacity = 1)
        l
      }
    }
  })
  output$table <- DT::renderDataTable({
    the_type <- input$type
    pts <- dfr()
    if(!is.null(pts)){
      if(nrow(pts) > 0){
        pts <- pts %>% filter(type %in% the_type)
        pts %>%
          dplyr::select(wid, lnt, lat, description, type)
      }
    }
  })
  
  observeEvent(input$log_out, {
    logged_in(FALSE)
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
  #   'www', system.file('app/www', package = 'incidents')
  # )
  
  
  share <- list(
    title = "Bohemia incidents tool",
    url = "https://bohemia.team/incidents/",
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