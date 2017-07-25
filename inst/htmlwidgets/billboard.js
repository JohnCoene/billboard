HTMLWidgets.widget({

  name: 'billboard',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        // TODO: code to render the widget, e.g.
        var options = x.options;
        options.bindto = "#" + el.id;
        var chart = bb.generate(options);

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
