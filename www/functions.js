$(document).ready(function() {

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


    // statistic methods are disabled before data is loaded
    setNavbarStatisticMethodsState(false);

    // allow Shiny to call this function
    Shiny.addCustomMessageHandler("setNavbarStatisticMethodsState", function(enabled) {
      setNavbarStatisticMethodsState(enabled);
    });

});
