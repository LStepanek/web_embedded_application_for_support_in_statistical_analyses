// Function to enable or disable the 'statistical-methods' dropdown item in the navbar
function setNavbarItemsState(enabled) {
  var itemsSelectors = [
    '.navbar-nav .dropdown [data-value="statistical-methods"]',
    '.navbar-nav [data-value="tab_ai_insight"]'
  ];

  var items = $(itemsSelectors.join(', '));

  if (enabled) {
    items.removeClass('disabled')
         .css('pointer-events', 'auto')
         .css('opacity', '1');
  } else {
    items.addClass('disabled')
         .css('pointer-events', 'none')
         .css('opacity', '0.5');
  }
}


// Function to highlight table cells that contain the value "N/A" (missings)
function highlightMissingCells(row, data, index) {
  for (var i = 0; i < data.length; i++) {
    if (data[i] === "N/A") {
      $('td:eq(' + i + ')', row).addClass('missing-cell');
    }
  }
}


// Function to build a string of data type abbreviations based on the first character of each selected value
// Considers all rows, including those on hidden pages (due to pagination)
function getSelectedDataTypes() {
  var dataTypes = "";
  var table = $('#upload_summary_table table').DataTable();

  table.rows().every(function(rowIdx, tableLoop, rowLoop) {
    var cellNode = table.cell(rowIdx, 1).node();  // Get the DOM element of the 2nd column cell
    var $select = $('select', cellNode);          // Try to find a <select> element inside the cell

    // Determine value based on presence of <select>
    var rawValue = $select.length ? $select.val() : this.data()[1];

    // Process the value: take the first character and convert to uppercase
    if (rawValue && rawValue.charAt) {
      var firstLetter = rawValue.charAt(0).toUpperCase();
      if (/^[INLCDP]$/.test(firstLetter)) {
        dataTypes += firstLetter;
      }
    }
  });

  return dataTypes;
}

// Triggered when a select box value is changed
function onDataTypeSelectChange(columnName, newType) {
  // Get all data types from the table (including hidden rows)
  var dataTypes = getSelectedDataTypes();

  // Display a toast notification
  //toastr.info('Data type for column "' + columnName + '" was changed to: ' + newType + '.');

  // Update input value for Shiny app
  document.getElementById("col_types").value = dataTypes;

  // Send data types to Shiny app
  Shiny.setInputValue("col_types", dataTypes, {priority: "event"});
}

// Renders a <select> dropdown for choosing the data type of a table column.
function renderDataTypeSelector(data, type, row, options) {
  if (type === 'display') {
    var select = '<select onchange="onDataTypeSelectChange(\'' + row[0] + '\', this.value)">';
    for (var i = 0; i < options.length; i++) {
      var value = options[i].trim();
      var selected = value === String(data).trim() ? ' selected' : '';
      select += '<option value="' + value + '"' + selected + '>' + value + '</option>';
    }
    select += '</select>';
    return select;
  }

  return data;
}


$(document).ready(function() {
  toastr.options = {
      "closeButton": true,
      "debug": false,
      "newestOnTop": false,
      "progressBar": true,
      "preventDuplicates": true,
      "positionClass": "toast-custom-position",
      "onclick": null,
      "showDuration": "300",
      "hideDuration": "1000",
      "timeOut": "3000",
      "extendedTimeOut": "1000",
      "showEasing": "swing",
      "hideEasing": "linear",
      "showMethod": "fadeIn",
      "hideMethod": "fadeOut"
  };


  // column data types input is read-only
  document.getElementById("col_types").readOnly = true;


  // allow Shiny to call this function
  Shiny.addCustomMessageHandler("setNavbarItemsState", function(enabled) {
    setNavbarItemsState(enabled);
  });

});
