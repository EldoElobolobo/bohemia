#' Inverted versions of in, is.null and is.na
#' 
#' @noRd
#' 
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#' 
#' @noRd
#' 
#' @example 
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x){
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NULL`
#' 
#' @noRd
#' 
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#' 
#' @param x,y Two elements to test, one potentially `NA`
#' 
#' @noRd
#' 
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#' 
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#' 
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList





# C4 callback function
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
