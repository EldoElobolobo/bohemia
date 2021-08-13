$(document).ready(function () {
    var tbl = $(table.table().node());
    var id = tbl.closest('.datatables').attr('id');
    tbl.MakeCellsEditable({
        "onUpdate": onUpdate,
        "inputCss":'my-input-class',
        "columns": [3,4,5,6,7,8,9,10,11,12,13,14,15,16],
        "confirmationButton": { // could also be true
            "confirmCss": 'my-confirm-class',
            "cancelCss": 'my-cancel-class'
        },
        "inputTypes": [
            {
                "column":3, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":4, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":5, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":6, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":7, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":8, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":9, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":10, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":11, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":12, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":13, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":14, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":15, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            {
                "column":16, 
                "type": "list",
                "options":[
                    { "value": "None", "display": "None" },
                    { "value": "Yes", "display": "Y" },
                    { "value": "No", "display": "N" }
                ]
            },
            
        ]
    });

});

function onUpdate (updatedCell, updatedRow, oldValue) {
  var cellinfo = [{
    row: updatedCell.index().row + 1,
    col: updatedCell.index().column + 1,
    value: updatedCell.data()
    
  }];
  Shiny.setInputValue(id + '_cell_edit:DT.cellInfo', cellinfo);
}

