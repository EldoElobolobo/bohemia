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
  
  
  
  
  
  # A4
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
  
  
  # B4 reativevalues
  # B4
  entob4_df <- suppressWarnings(getEntoDataB4())
  
  if(nrow(entob4_df) >= 1){
    x <- create_btns(1:nrow(entob4_df))
    entob4_df$ScanCode = x
  }

  
  
  b4rv <- shiny::reactiveValues(
    df = entob4_df,
    dt_row = NULL,
    add_or_edit = NULL,
    edit_button = NULL,
    keep_track_id = nrow(entob4_df) + 1
  )
  
  # observeEvent(reactiveTimer(480000)(), {
  #   # Trigger every 10 minutes
  #   b4rv$df <- isolate({
  #     # get and bind the new data
  #     rbind(b4rv$df, getEntoDataB4())
  #     #get_entob4_data()
  #   })
  # })
  
  
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
    d_table <- DT::datatable(isolate(c4rv$df),
                        selection = "none",
                        caption = htmltools::tags$caption(
                          style = 'text-align: center;',
                          'Note: ', htmltools::em('Click any table cell from column 4 (Days) onwards to edit and press SAVE')
                        ),
                        container = sketch,
                        rownames = FALSE,
                        callback = htmlwidgets::JS(c(
                          "var tbl = $(table.table().node());",
                          "var id = tbl.closest('.datatables').attr('id');",
                          "function onUpdate(updatedCell, updatedRow, oldValue) {",
                          "  var cellinfo = [{",
                          "    row: updatedCell.index().row + 1,",
                          "    col: updatedCell.index().column + 1,",
                          "    value: updatedCell.data()",
                          "  }];",
                          "  Shiny.setInputValue(id + '_cell_edit:DT.cellInfo', cellinfo);",
                          "}",
                          "table.MakeCellsEditable({",
                          "  onUpdate: onUpdate,",
                          "  inputCss: 'my-input-class',",
                          "  columns: [3,4,5,6,7,8,9,10,11,12,13,14,15,16],",
                          "  confirmationButton: {",
                          "    confirmCss: 'my-confirm-class',",
                          "    cancelCss: 'my-cancel-class'",
                          "  },",
                          "  inputTypes: [",
                          "    {",
                          "      column: 3,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 4,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 5,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 6,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 7,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 8,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 9,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 10,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 11,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 12,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 13,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 14,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 15,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    },",
                          "    {",
                          "      column: 16,",
                          "      type: 'list',",
                          "      options: [",
                          "        {value: 'None', display: 'None'},",
                          "        {value: 'Yes', display: 'Y'},",
                          "        {value: 'No', display: 'N'}",
                          "      ]",
                          "    }",
                          "  ]",
                          "});"
                        )))%>% 
      DT::formatStyle(c(10), `border-right` = "solid 1px")
    
    path <- path.expand("inst/app/www") 
    dep <- htmltools::htmlDependency(
      "CellEdit", "1.0.19", path, 
      script = "dataTables.cellEdit.js",
      stylesheet = "custom.css",
      all_files = FALSE)
    d_table$dependencies <- c(d_table$dependencies, list(dep))
    d_table
  },server = FALSE)
  
  
  
  #B3 datatable
  output$dt_table <- DT::renderDT({
    
    #shiny::isolate(rv$df)
    
    dtable <- DT::datatable(shiny::isolate(b4rv$df),
                        escape = F,
                        rownames = FALSE,
                        editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4))),
                        caption = htmltools::tags$caption(
                          style = 'text-align: center;',
                          'Note: ', htmltools::em('N represents Number' , br(),'QR represents QRCODE, .', br(),
                                                  'DM represents Dead Mosquitoes',br(), 'HINT: IF no mosquitoes, ENTER 0.')
                        ),
                        #selection = "none",
                        container = sketchb4,
                        options = list(
                          dom='t',
                          ordering = F,
                          paging = FALSE,
                          rowsGroup = list(0,1,2,3), # merge cells of column 1
                          processing = FALSE,
                          columnDefs = list(list(targets = c(5,7,9,11,13,15,17,19,21), className = "withPlaceholder"),
                                            list(targets = c(6,8,10,12,14,16,18,20), className = "withPlaceholder2"))
                        ))
    path <- path.expand("inst/app/www") # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0",
      path, script = "dataTables.rowsGroup.js")
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
    
  })
  
  proxy <- DT::dataTableProxy("dt_table")
  
  shiny::observeEvent(input$dt_table_cell_edit,{
    
    shiny::isolate(b4rv$df)
    info = input$dt_table_cell_edit
    
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    req(v)
    num_cols <- c("5","7","9","11","13","15","17","19","21")
    if(as.integer(v) > 10 & as.character(j) %in% num_cols | suppressWarnings(is.na(as.integer(v)))){
      shinyalert::shinyalert("Check Value!!", "Please verify the number of mosquitoes entered", type = "error")
    }
    clearSorting(proxy = DT::dataTableProxy(outputId = "dt_table"))
    b4rv$df <- DT::editData(b4rv$df,info,proxy,resetPaging = FALSE, rownames = FALSE)
    #DT::replaceData(proxy, b3rv$df, resetPaging = FALSE, rownames = FALSE)
  })
  
  
  
  # when edit button is clicked, modal dialog shows current editable row filled out
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit"))
    b3rv$dt_row <- which(stringr::str_detect(b3rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
    df <- b3rv$df[b3rv$dt_row, ]
    shiny::modal_dialog(
      D1_dead_mosquitoes_N = df$D1_dead_mosquitoes_N,
      D1_dead_mosquitoes_QR = df$D1_dead_mosquitoes_QR,
      D2_dead_mosquitoes_N = df$D2_dead_mosquitoes_N,
      D2_dead_mosquitoes_QR = df$D2_dead_mosquitoes_QR,
      D3_dead_mosquitoes_N = df$D3_dead_mosquitoes_N,
      D3_dead_mosquitoes_QR = df$D3_dead_mosquitoes_QR,
      D4_dead_mosquitoes_N = df$D4_dead_mosquitoes_N,
      D4_dead_mosquitoes_QR = df$D4_dead_mosquitoes_QR,
      D5_dead_mosquitoes_N = df$D5_dead_mosquitoes_N,
      D5_dead_mosquitoes_QR = df$D5_dead_mosquitoes_QR,
      D6_dead_mosquitoes_N = df$D6_dead_mosquitoes_N,
      D6_dead_mosquitoes_QR = df$D6_dead_mosquitoes_QR,
      D7_dead_mosquitoes_N = df$D7_dead_mosquitoes_N,
      D7_dead_mosquitoes_QR = df$D7_dead_mosquitoes_QR,
      alive_at_D7_mosquitoes_N = df$alive_at_D7_mosquitoes_N,
      alive_at_D7_mosquitoes_QR = df$live_at_D7_mosquitoes_QR,
      edit = TRUE
    )
    b3rv$add_or_edit <- NULL
  })
  
  
  # when final edit button is clicked, table will be changed
  shiny::observeEvent(input$final_edit, {
    shiny::req(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit") & is.null(b3rv$add_or_edit))
    b3rv$dt_row <- which(stringr::str_detect(b3rv$df$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))
    b3rv$edited_row <- dplyr::tibble(
      Date = b3rv$df$Date[b3rv$dt_row],
      HH.ID = b3rv$df$HH.ID[b3rv$dt_row],
      Indoors.or.Outdoors = b3rv$df$Indoors.or.Outdoors[b3rv$dt_row],
      Time.period = b3rv$df$Time.period[b3rv$dt_row],
      Species = b3rv$df$Species[b3rv$dt_row],
      D1_dead_mosquitoes_N = b3rv$df$D1_dead_mosquitoes_N[b3rv$dt_row],
      D1_dead_mosquitoes_QR = input$d1_qrdm,
      D2_dead_mosquitoes_N = b3rv$df$D2_dead_mosquitoes_N[b3rv$dt_row],
      D2_dead_mosquitoes_QR = input$d2_qrdm,
      D3_dead_mosquitoes_N = b3rv $df$D3_dead_mosquitoes_N[b3rv$dt_row],
      D3_dead_mosquitoes_QR = input$d3_qrdm,
      D4_dead_mosquitoes_N = b3rv$df$D4_dead_mosquitoes_N[rv$dt_row],
      D4_dead_mosquitoes_QR = input$d4_qrdm,
      D5_dead_mosquitoes_N = rv$df$D5_dead_mosquitoes_N[rv$dt_row],
      D5_dead_mosquitoes_QR = input$d5_qrdm,
      D6_dead_mosquitoes_N = rv$df$D6_dead_mosquitoes_N[rv$dt_row],
      D6_dead_mosquitoes_QR = input$d6_qrdm,
      D7_dead_mosquitoes_N = rv$df$D7_dead_mosquitoes_N[rv$dt_row],
      D7_dead_mosquitoes_QR = input$d7_qrdm,
      alive_at_D7_mosquitoes_N = rv$df$alive_at_D7_mosquitoes_N[rv$dt_row],
      alive_at_D7_mosquitoes_QR = input$d7_qral,
      Buttons = rv$df$Buttons[rv$dt_row]
    )
    rv$df[rv$dt_row, ] <- rv$edited_row
    rv$dt_row
  })
  
  shiny::observeEvent(input$dismiss_modal, {
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$final_edit, {
    shiny::removeModal()
  })
  
  

}
