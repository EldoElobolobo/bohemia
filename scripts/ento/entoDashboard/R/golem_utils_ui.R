#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#' 
#' @return an HTML list
#' @noRd
#' 
#' @examples
#' list_to_li(c("a","b"))
#'
#' @importFrom htmltools tags tagAppendAttributes tagList
list_to_li <- function(list, class = NULL){
  if (is.null(class)){
    tagList(
      lapply(
        list, 
        tags$li
      )
    )
  } else {
    res <- lapply(
      list, 
      tags$li
    )
    res <- lapply(
      res, 
      function(x) {
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
  
}

#' @importFrom htmltools tags tagAppendAttributes tagList
list_to_p <- function(list, class = NULL){
  if (is.null(class)){
    tagList(
      lapply(
        list, 
        tags$p
      )
    )
  } else {
    res <- lapply(
      list, 
      tags$p
    )
    res <- lapply(
      res, 
      function(x) { 
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
  
}

#' @importFrom htmltools tags tagAppendAttributes tagList
named_to_li <- function(list, class = NULL){
  if(is.null(class)){
    res <- mapply(
      function(x, y){
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list, 
      names(list), 
      SIMPLIFY = FALSE
    )
    tagList(res)
  } else {
    res <- mapply(
      function(x, y){
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list), 
      SIMPLIFY = FALSE
    )
    res <- lapply(
      res, 
      function(x) {
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' Remove a tag attribute
#'
#' @param tag the tag
#' @param ... the attributes to remove
#'
#' @return a new tag
#' @noRd
#' 
#' @examples
#' a <- shiny::tags$p(src = "plop", "pouet")
#' tagRemoveAttributes(a, "src")
tagRemoveAttributes <- function(tag, ...) {
  attrs <- as.character(list(...))
  for (i in seq_along(attrs)) {
    tag$attribs[[ attrs[i] ]] <- NULL
  }
  tag
}

#' Hide or display a tag
#' 
#' @param tag the tag
#' 
#' @return a tag
#' @noRd
#' 
#' @examples
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
#' 
#' @importFrom htmltools tagList
undisplay <- function(tag) {
  # if not already hidden
  if (
    !is.null(tag$attribs$style) && 
    !grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- paste(
      "display: none;", 
      tag$attribs$style
    )
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

#' @importFrom htmltools tagList
display <- function(tag) {
  if (
    !is.null(tag$attribs$style) && 
    grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- gsub(
      "(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*", 
      "", 
      tag$attribs$style
    )
  }
  tag
}

#' Hide an elements by calling jquery hide on it
#' 
#' @param id the id of the element to hide
#' 
#' @noRd
#' 
#' @importFrom htmltools tags
jq_hide <- function(id) {
  tags$script(sprintf("$('#%s').hide()", id))
}

#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#' @noRd
#' 
#' @examples
#' with_red_star("Enter your name here")
#' 
#' @importFrom htmltools tags HTML
with_red_star <- function(text) {
  htmltools::tags$span(
    HTML(
      paste0(
        text,
        htmltools::tags$span(
          style = "color:red", "*"
        )
      )
    )
  )
}



#' Repeat tags$br
#'
#' @param times the number of br to return
#'
#' @return the number of br specified in times
#' @noRd
#' 
#' @examples
#' rep_br(5)
#' 
#' @importFrom htmltools HTML
rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}

#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @noRd
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' 
#' @importFrom htmltools tags
enurl <- function(url, text){
  tags$a(href = url, text)
}

#' Columns wrappers
#' 
#' These are convenient wrappers around 
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#' 
#' @noRd
#' 
#' @importFrom shiny column
col_12 <- function(...){
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...){
  column(10, ...)
}

#' @importFrom shiny column
col_8 <- function(...){
  column(8, ...)
}

#' @importFrom shiny column
col_6 <- function(...){
  column(6, ...)
}


#' @importFrom shiny column
col_4 <- function(...){
  column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...){
  column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...){
  column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...){
  column(1, ...)
}

# UNCOMMENT AND USE 
# 
# usethis::use_package("markdown")
# usethis::use_package("rmarkdown")
#   
# To use this part of the UI
#   
#' #' Include Content From a File
#' #' 
#' #' Load rendered RMarkdown from a file and turn into HTML.
#' #' 
#' #' @rdname includeRMarkdown
#' #' @export
#' #' 
#' #' @importFrom rmarkdown render
#' #' @importFrom markdown markdownToHTML
#' #' @importFrom htmltools HTML
#' includeRMarkdown <- function(path){
#'   
#'   md <- tempfile(fileext = '.md')
#'   
#'   on.exit(unlink(md),add = TRUE)
#'   
#'   rmarkdown::render(
#'     path,
#'     output_format = 'md_document',
#'     output_dir = tempdir(),
#'     output_file = md,quiet = TRUE
#'     )
#'   
#'   html <- markdown::markdownToHTML(md, fragment.only = TRUE)
#'   
#'   Encoding(html) <- "UTF-8"
#'   
#'   return(HTML(html))
#' }



CSS <- "
table td.withPlaceholder:empty:before {
  content: 'integer';
  color: gray;
  font-style: italic;
}

table td.withPlaceholder2:empty:before {
  content: 'qrcode';
  color: gray;
  font-style: italic;
}

"

callback <- c(
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
)















# for creating buttons in button column on B4 table
create_btns <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="edit_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-qrcode" aria-hidden="true"></i></i></button>
                   </div>'
                     ))
}

#get ento b3 data


# define some credentials
credentials <- data.frame(
  user = c("admin", "databrewuser2"), # mandatory
  password = c("admin", "12345"), # mandatory
  admin = c(TRUE, FALSE),
  comment = "Simple and secure authentification mechanism 
  for single 'Shiny' applications.",
  stringsAsFactors = FALSE
)

# Change language
shinymanager::set_labels(
  language = "en",
  "Please authenticate" = "LOGIN"
)

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)


# html table sketch
sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = "3",'Date'),
      th(rowspan = "3",'HH ID'),
      th(rowspan = "3",'Collection Location'),
      th(colspan = "14", style="text-align:center",'Life History'),
      th(rowspan = "3",'Species Complex'),
      th(rowspan = "3",'Sample Tube ID and Link to QR Code')
    ),
    tr(
      th(colspan ="7", style="text-align:center", 'Oviposition'),
      th(colspan ="7", style="text-align:center", 'Death')
    ),
    tr(
      lapply(rep(c('D1','D2','D3','D4','D5','D6','D7'),2),th)
    )
  )
)
)


sketchb4 = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = "2",'Date'),
      th(rowspan = "2",'HH ID'),
      th(rowspan = "2",'Indoors or Outdoors'),
      th(rowspan = "2",'Time period'),
      th(rowspan = "2",'Species'),
      th(colspan = "2",'D1 - DM'),
      th(colspan = "2",'D2 - DM'),
      th(colspan = "2",'D3 - DM'),
      th(colspan = "2",'D4 - DM'),
      th(colspan = "2",'D5 - DM'),
      th(colspan = "2",'D6 - DM'),
      th(colspan = "2",'D7 - DM'),
      th(colspan = "2",'Alive at D7'),
      th(rowspan = "2",'QR Scanner')
    ),
    tr(
      lapply(rep(c('N','QR'),8),th)
    )
  )
)
)