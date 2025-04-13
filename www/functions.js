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


// Function to highlight table cells that contain the value "N/A"
function highlightMissingCells(row, data, index) {
  for (var i = 0; i < data.length; i++) {
    if (data[i] === "N/A") {
      $('td:eq(' + i + ')', row).addClass('missing-cell');
    }
  }
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

  // allow Shiny to call this function
  Shiny.addCustomMessageHandler("setNavbarStatisticMethodsState", function(enabled) {
    setNavbarStatisticMethodsState(enabled);
  });
});

