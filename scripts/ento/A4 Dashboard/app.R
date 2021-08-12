library(shiny)
library(shinydashboard)
library(shinyalert)
library(DT)
library(readr)
library(dplyr)


 
get_entoa3_data = function(){
  require(dplyr)
  require(readr)
  require(odkr)
  
  briefcase_dir <- "."
  jar_file_briefcase <- 'ODK-Briefcase-v1.18.0'
  
  # Get ODK server credentials
  odk_server = Sys.getenv("databrew_odk_server")
  odk_user = Sys.getenv("databrew_odk_user")
  odk_password = Sys.getenv("databrew_odk_pass")
  
  
  # Make a temporary directory
  temp_dir <- tempdir()
  
  #removing downloaded old ento files
  old_file = paste0(temp_dir, "/entoa3.csv")
  if(file.exists(old_file)){
    file.remove(old_file)
    cat("[INFO]...Removing old ento file...")
  }else{
    cat("[INFO]..Previous file not available...")
  }
  
  # Retrieve the data
  pull_remote(target = paste0(briefcase_dir),
              briefcase = jar_file_briefcase,
              id = "entoa3",
              to = temp_dir,
              from = odk_server,
              username = odk_user,
              password = odk_password)
  
  # Export the data as csv
  export_data(target = briefcase_dir,
              briefcase = jar_file_briefcase,
              id = "entoa3",
              from = temp_dir,
              to = temp_dir,
              filename = "entoa3.csv")
  
  entoa3_files <- dir(temp_dir)
  entoa3_files <- entoa3_files[grepl('entoa3', entoa3_files)]
  
  entoa3_list = lapply(entoa3_files, function(x) {
    read_csv(file.path(temp_dir, x))
  })
  
  names(entoa3_list) <- gsub('.csv', '', entoa3_files, fixed = TRUE)
  entoa3 <- entoa3_list
  
  # # Read in the csv
  entoa3_df <- suppressWarnings(read_csv(file.path(temp_dir, 'entoa3.csv'))) 
  
  # Create cluster variable
  df <- entoa3_df %>%
    mutate(Cluster = ifelse(site == "Household",hh_id,if_else(site == "Livestock_Enclosure",le_id,site)))
  
  d = data.frame(Date = df$todays_date,
                 Cluster = df$Cluster,
                 "Collection Location" = df$site)
  emp = data.frame(matrix(ncol = 4, nrow = nrow(df)))
  A4df = cbind(d,emp)
  names(A4df)[-c(1:3)] = c("Species_Complex","Parity_status","Wing-Length (micrometers)","Sample tube ID and link to QR code")
  
  return(A4df)
}

# for dropdown
Species_Complex = as.factor(c("gambaie","Other"))

# # To populate
Parity_Status = as.factor(c("NP","P"))


callback <- c(
  "var id = $(table.table().node()).closest('.datatables').attr('id');",
  "$.contextMenu({",
  "  selector: '#' + id + ' td.factor input[type=text]',",
  "  trigger: 'hover',",
  "  build: function($trigger, e){",
  "    var levels = $trigger.parent().data('levels');",
  "    if(levels === undefined){",
  "      var colindex = table.cell($trigger.parent()[0]).index().column;",
  "      levels = table.column(colindex).data().unique();",
  "    }",
  "    var options = levels.reduce(function(result, item, index, array){",
  "      result[index] = item;",
  "      return result;",
  "    }, {});",
  "    return {",
  "      autoHide: true,",
  "      items: {",
  "        dropdown: {",
  "          name: 'Edit',",
  "          type: 'select',",
  "          options: options,",
  "          selected: 0",
  "        }",
  "      },",
  "      events: {",
  "        show: function(opts){",
  "          opts.$trigger.off('blur');",
  "        },",
  "        hide: function(opts){",
  "          var $this = this;",
  "          var data = $.contextMenu.getInputValues(opts, $this.data());",
  "          var $input = opts.$trigger;",
  "          $input.val(options[data.dropdown]);",
  "          $input.trigger('change');",
  "        }",
  "      }",
  "    };",
  "  }",
  "});"
)

createdCell <- function(levels){
  if(missing(levels)){
    return("function(td, cellData, rowData, rowIndex, colIndex){}")
  }
  quotedLevels <- toString(sprintf("\"%s\"", levels))
  c(
    "function(td, cellData, rowData, rowIndex, colIndex){",
    sprintf("  $(td).attr('data-levels', '[%s]');", quotedLevels),
    "}"
  )
}

ui <- dashboardPage(

  dashboardHeader(
    title = "Ento A4 Dashboard"
  ),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidPage(
      useShinyalert(),
      tags$head(
        tags$link(
          rel = "stylesheet",
          href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.css"
        ),
        tags$script(
          src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js"
        )
      ),
      box(status = "primary",
          width = 12,
          div(style = 'overflow-x: scroll', DTOutput('dtable'))
      ),
      tags$script(HTML(
        'function get_id(clicked_id) {
     Shiny.setInputValue("current_id", clicked_id, {priority: "event"});
}'
      ))
    )

  )
)


server <- function(input, output){
  
  # it is going to store all upcoming new data
  values <- reactiveValues(df = data.frame())
  
  observeEvent(reactiveTimer(400000)(), {
    # Trigger every 2 seconds
    values$df <- isolate({
      # get and bind the new data
      rbind(values$df, get_entoa3_data())%>%
        distinct()
    })
  })
  
  
  output[["dtable"]] <- renderDT({
    datatable(
      values$df, editable = list(target = 'cell',disable = list(columns = c(0,1,2))),
      caption = htmltools::tags$caption(
        style = 'text-align: center;',
        'Note: ', htmltools::em('Double click cells in columns 4 to 7 to edit and press ENTER to save')
      ),
      rownames = FALSE,
      callback = JS(callback),
      options = list(
        columnDefs = list(
          list(
            targets = 4,
            className = "factor",
            createdCell = JS(createdCell(c(levels(Parity_Status))))
          ),
          list(
            targets = 3,
            className = "factor",
            createdCell = JS(createdCell(c(levels(Species_Complex))))
          )
        ),
        dom = "Bfrtip",
        buttons = list('csv')
      ),
      extensions = "Buttons",
      
      
    )
  }, server = FALSE)
  
  
  
  proxy <- DT::dataTableProxy("dtable")

  shiny::observeEvent(input$dtable_cell_edit,{

    #isolate(rv$df)
    info = input$dtable_cell_edit

    str(info)
    i = info$row
    j = info$col
    v = info$value

    req(v)
    num_cols <- c("5","7","9","11","13","15","17","19","21")
    val = suppressWarnings(as.integer(v))
    if(j == 5 & !dplyr::between(val,2000,3500)){
      shinyalert("Check Value!!", "Please verify the micrometers entered for wing length", type = "error")
    }
    #clearSorting(proxy = dataTableProxy(outputId = "dtable"))
    #A4 <- editData(A4,info,proxy,resetPaging = FALSE, rownames = FALSE)
    #DT::replaceData(proxy, rv$df, resetPaging = FALSE, rownames = FALSE)
  })
}

shinyApp(ui, server)


