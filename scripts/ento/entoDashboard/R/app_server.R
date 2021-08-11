#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  
  # authentication module
  auth <- callModule(
    module = shinymanager::auth_server,
    id = "auth",
    check_credentials = shinymanager::check_credentials(credentials)
  )
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(auth)
  })
  
  observeEvent(session$input$logout,{
    session$reload()
  })
  
  
  
  # List the first level callModules here
  
  ento <- getEntoData()
  
  #A4 reactive values
  rv <- shiny::reactiveValues(df = ento)
  
  observeEvent(shiny::reactiveTimer(400000)(), {
    # trigger every 8 minutes
    rv$df <- isolate({
      
      rbind(rv$df, getEntoData())
    })
  })
  
  
  
  # C4 reactive values
  c4rv <- reactiveValues(df = data.frame())
  
  observeEvent(reactiveTimer(450000)(), {
    # Trigger every 6 minutes
    c4rv$df <- isolate({
      # get and bind the new data
      rbind(c4rv$df, getEntoDataC4())
      #get_entoc4_data()
    })
  })
  
  
  
  winglength.update.callback <- function(data, olddata, row) {
    
    if (!dplyr::between(data[row, "Wing-length (micrometers)"],2000,3500)) {
     stop("Please verify the micrometers entered for wing length")
      #shinyalert::shinyalert("Check Value!!", "Please verify the micrometers entered for wing length", type = "error")
    }
    
    return(data)
  }
  
  
  #A4
  b4_datalist <-DTedit::dtedit(
    input,output,
    name = "A4",
    thedata = rv$df,
    input.types = list(
      Parity_Status = 'selectInput'
    ),
    input.choices = list(
      Parity_Status = c("NP","P")
    ),
    show.delete = FALSE,
    show.insert = FALSE,
    #show.update = TRUE,
    show.copy = FALSE,
    edit.cols = c("Parity_Status","Wing-length (micrometers)","Sample tube ID and link to QR code"),
    #icon.delete = shiny::icon("trash"), 
    icon.edit = shiny::icon("edit"), 
    #icon.add = shiny::icon("plus"), 
    icon.copy = shiny::icon("copy"),
    useairDatepicker = TRUE,
    numeric.width = "100%",
    datatable.call = function(...)
    {DT::datatable(..., extensions = 'Buttons',
                   caption = htmltools::tags$caption(
                     style = 'text-align: center;',
                     'Note: ', htmltools::em('Click any row and click the edit button to enter data.')
                   ))},
    datatable.options = list(
      dom = 'Bfrtip',
      buttons = c('csv')
    ),
    callback.update = winglength.update.callback
  )
  
  
  # C4
  output$dt = DT::renderDT({
    dtable <- DT::datatable(isolate(c4rv$df),
                        selection = "none",
                        caption = htmltools::tags$caption(
                          style = 'text-align: center;',
                          'Note: ', htmltools::em('Click any table cell from column 4 (Days) onwards to edit and press SAVE')
                        ),
                        container = sketch,
                        rownames = FALSE,
                        callback = htmlwidgets::JS(callback))%>% 
      DT::formatStyle(c(10), `border-right` = "solid 1px")
    
    path <- "./inst/app/www" 
    dep <- htmltools::htmlDependency(
      "CellEdit", "1.0.19", path, 
      script = "dataTables.cellEdit.js",
      stylesheet = "custom.css",
      all_files = FALSE)
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  },server = FALSE)
  
  
  

}
