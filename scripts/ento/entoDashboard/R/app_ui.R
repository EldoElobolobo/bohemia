#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    #options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
    shinydashboard::dashboardPage(
      skin = "blue",
      header = shinydashboard::dashboardHeader(
        title = "Ento Data Management"
      ),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          shinydashboard::menuItem("Tables", tabName = "tables", icon = icon("table"))
        )
      ),
      shinydashboard::dashboardBody(
        shinyalert::useShinyalert(),
        dashboardthemes::shinyDashboardThemes(theme = "grey_light"),
        shinymanager::auth_ui(id = "auth"),
        shinymanager::fab_button(
          shiny::actionButton(
            inputId = "logout",
            label = NULL,
            tooltip = "Logout",
            icon = icon("sign-out")
          )
        ),
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "tables",
                                  
                                  shinydashboard::tabBox(
                                    title = "Ento Data Management Tables",
                                    width = 12,
                                    id = "tabs",
                                    tabPanel("A4",
                                             h4("PARITY DISSECTION AND WING LENGTH", style = "color:#2E86C1; text-align: center; padding: 5px;"),
                                             div(style = 'overflow-x: scroll',shinycssloaders::withSpinner(shiny::uiOutput('A4'),type = 1, size = 1))
                                                      ),
                                    tabPanel("B4", "B4 table here"),
                                    tabPanel("C4", 
                                             h4("GONOTROPICAL CYCLE LENGTH AND SURVIVAL", style = "color:#2E86C1; text-align: center; padding: 5px;"),
                                             div(style = 'overflow-x: scroll', shinycssloaders::withSpinner(DT::DTOutput('dt'),type = 1, size = 1)))
                                  )
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
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'entoDashboard',
      name = "CellEdit",
      version = "1.0.19",
      all_files = TRUE
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyalert::useShinyalert(),
    tags$style(HTML("
                    .skin-blue .main-sidebar {background-color: #E0E0E0}
                    .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {#707070}
                    "))
  )
}

