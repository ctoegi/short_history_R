PlotModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("radar_groups")),
    plotOutput(ns("radar_overall"))
  )
}

PlotModule <- function(input, output, session, sidebar_args, data_source) {
  
  # plots error plot if no json uploaded
  no_data_error <- function() {
    plot.new()
    text(x = 0.5, y = 0.5, labels = 'No data provided', col = 'red', cex = 2)
  }
  
  output$radar_groups <- renderPlot({
    if (data_source == 'sliders') {
      ratings <- sidebar_args$ratings()
    } else {
      ratings <- sidebar_args$ratings_uploaded_json()
    }
    if (is.null(ratings)) {
      no_data_error()
    } else {
      par(mfrow = c(2, 3))
      for (group in ratings) {
        radarchart(
          group$df,
          pfcol = group$color,
          title = group$title
        )
      }
    }
  })
  
  output$radar_overall <- renderPlot({
    if (data_source == 'sliders') {
      overall <- sidebar_args$overall()
    } else {
      overall <- sidebar_args$overall_uploaded_json()
    }
    if (is.null(overall)) {
      no_data_error()
    } else {
      radarchart(
        overall$df,
        pfcol = overall$color,
        title = overall$title
      )
    }
  })
}
