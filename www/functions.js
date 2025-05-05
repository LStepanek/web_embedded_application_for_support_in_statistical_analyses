// Function to enable or disable the 'statistic-methods' dropdown item in the navbar
function setNavbarStatisticMethodsState(enabled) {
  var item = $('.navbar-nav .dropdown [data-value="statistic-methods"]');

  if (enabled) {
    item.removeClass('disabled')
        .css('pointer-events', 'auto')
        .css('opacity', '1');
  } else {
    item.addClass('disabled')
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


// Function to update the abbreviation string using the first character of each selected value
function getSelectedDataTypes() {
  var dataTypes = '';

  // Iterate over all <select> elements within the DataTable
  $('#upload_summary_table').find('select').each(function() {
    var value = $(this).val().charAt(0).toUpperCase();

    // Only include characters that match allowed data type initials
    if (value && /^[INLSCD]$/.test(value)) {
      dataTypes += value;
    }
  });

  return dataTypes;
}

// Triggered when a select box value is changed
function onDataTypeSelectChange(columnName, newType) {
  var dataTypes = getSelectedDataTypes();

  // Display a toast notification
  //toastr.info('Data type for column "' + columnName + '" was changed to: ' + newType + '.');

  // Update the visible input field so the user can see the change
  document.getElementById("col_types").value = dataTypes;
  // Notify Shiny about the new value
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
  Shiny.addCustomMessageHandler("setNavbarStatisticMethodsState", function(enabled) {
    setNavbarStatisticMethodsState(enabled);
  });

});
