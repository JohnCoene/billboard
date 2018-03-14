HTMLWidgets.widget({

  name: 'billboard',

  type: 'output',

  factory: function(el, width, height) {
    
    var chart;

    return {

      renderValue: function(x) {
        
        var options = x.options;
        options.bindto = "#" + el.id;
        chart = bb.generate(options);

      },

      getChart: function(){
        return chart;
      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});

function get_b_boardChart(id){

  // Get the HTMLWidgets object
  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  // Use the getChart method we created to get the underlying C3 chart
  var bboard;

  if (typeof htmlWidgetsObj != 'undefined') {
    bboard = htmlWidgetsObj.getChart();
  }

  return(bboard);
}

if (HTMLWidgets.shinyMode) {

  // data = load
  Shiny.addCustomMessageHandler('b_zoom_p',
    function(data) {
      var chart = get_b_boardChart(data.id);
      if (typeof chart != 'undefined') {
        // chart.unload();
        chart.zoom(data.domain);
      }
  });

  Shiny.addCustomMessageHandler('b_focus_p',
    function(data) {
      var chart = get_b_boardChart(data.id);
      if (typeof chart != 'undefined') {
        // chart.unload();
        chart.focus(data.series);
      }
  });

  Shiny.addCustomMessageHandler('b_transform_p',
    function(data) {
      var chart = get_b_boardChart(data.id);
      if (typeof chart != 'undefined') {
        // chart.unload();
        chart.transform(data.params.to, data.params.serie);
      }
  });

  Shiny.addCustomMessageHandler('b_stack_p',
    function(data) {
      var chart = get_b_boardChart(data.id);
      if (typeof chart != 'undefined') {
        // chart.unload();
        chart.groups(data.serie);
      }
  });

  Shiny.addCustomMessageHandler('b_region_p',
    function(data) {
      var chart = get_b_boardChart(data.id);
      if (typeof chart != 'undefined') {
        // chart.unload();
        chart.regions(data.opts);
      }
  });

  Shiny.addCustomMessageHandler('b_add_region_p',
    function(data) {
      var chart = get_b_boardChart(data.id);
      if (typeof chart != 'undefined') {
        // chart.unload();
        chart.regions.add(data.opts);
      }
  });

  Shiny.addCustomMessageHandler('b_defocus_p',
    function(data) {
      var chart = get_b_boardChart(data.id);
      if (typeof chart != 'undefined') {
        // chart.unload();
        chart.defocus(data.series);
      }
  });

  Shiny.addCustomMessageHandler('b_flow_p',
    function(data) {
      var chart = get_b_boardChart(data.id);
      if (typeof chart != 'undefined') {
        // chart.unload();
        chart.flow(data.opts);
      }
  });

  Shiny.addCustomMessageHandler('b_load_p',
    function(data) {
      var chart = get_b_boardChart(data.id);
      if (typeof chart != 'undefined') {
        // chart.unload();
        chart.load(data.opts);
      }
  });
  
  Shiny.addCustomMessageHandler('b_export_p',
    function(data) {
      var chart = get_b_boardChart(data.id);
      if (typeof chart != 'undefined') {
        // chart.unload();
        chart.export();
      }
  });
}
