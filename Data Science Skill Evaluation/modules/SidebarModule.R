SidebarModuleUI <- function(id) {
  ns <- NS(id)
  sliders <- list()
  sliders[[1]] <- fileInput(ns("json_file"), "Upload Json ratings")
  i <- 2
  for (group in groups) {
    sliders[[i]] <- shiny::tags$h3(group$title)
    i <- i + 1
    for (entry in group$col_names) {
      slider <- sliderInput(ns(entry), entry, MIN_VAL, MAX_VAL, DEFAULT_VAL, step = 1)
      sliders[[i]] <- slider
      i <- i + 1
    }
  }
  sliders[[i]] <- downloadButton(ns("json_export"), "Download ratings as JSON")
  do.call('tagList', sliders)
}

SidebarModule <- function(input, output, session) {
  
  # this is taken from current sliders for EXPORT
  json_ratings <- reactive({
    raw_values <- list()
    for (group in groups) {
      for (entry in group$col_names) {
        raw_values[entry] <- input[[entry]]
      }
    }
    raw_values
  })
  output$json_export <- downloadHandler(
    filename = 'ratings_export.json',
    content = function(file) {
      cat(jsonlite::toJSON(json_ratings()), file = file)
    }
  )
  
  # Helpers for getting ratings from input (sliders) / json file
  get_ratings <- function(input) {
    ratings <- list()
    i <- 1
    for (group in groups) {
      cols <- list()
      for (entry in group$col_names) {
        cols[[entry]] <- c(MAX_VAL, MIN_VAL, input[[entry]])
      }
      ratings[[i]] <- list(
        df = cbind.data.frame(cols),
        color = group$color,
        title = group$title
      )
      i <- i + 1
    }
    return(ratings)
  }
  
  get_overall <- function(ratings) {
    if (is.null(ratings)) {
      overall <- NULL
    } else {
      cols <- list()
      for (entry in ratings) {
        group_avg <- mean(as.numeric(entry$df[3,]))
        cols[[entry$title]] <- c(MAX_VAL, MIN_VAL, group_avg)
      }
      overall <- list(
        df = cbind.data.frame(cols),
        color = rgb(0,0,1,0.5),
        title = 'Overall'
      )
    }
    return(overall)
  }
  
  ratings <- reactive({
    get_ratings(input)
  })
  
  overall <- reactive({
    get_overall(ratings())
  })
  
  ratings_uploaded_json <- reactive({
    uploaded_data <- try(
      jsonlite::fromJSON(input$json_file$datapath),
      silent = TRUE
    )
    if (class(uploaded_data) == 'try-error') {
      ratings <- NULL
    } else {
      ratings <- get_ratings(uploaded_data)
    }
    ratings
  })
  
  overall_uploaded_json <- reactive({
    get_overall(ratings_uploaded_json())
  })
  
  return(list(
    ratings = ratings,
    overall = overall,
    ratings_uploaded_json = ratings_uploaded_json,
    overall_uploaded_json = overall_uploaded_json
  ))
}