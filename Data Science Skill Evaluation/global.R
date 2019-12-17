# import necessary libraries
library(jsonlite)
library(shiny)
library(fmsb)

# source shiny module files
sapply(list.files(pattern = 'Module\\.R', recursive = TRUE), source)

# globals
MIN_VAL <- 0
MAX_VAL <- 20
DEFAULT_VAL <- (MIN_VAL+MAX_VAL)/2

groups <- list(
  list(
    title = 'Machine Learning',
    color = rgb(1,0,1,0.5),
    col_names = c(
      "Trees",
      "SVM",
      "Regression models",
      "Deep learning",
      "Association analysis",
      "Clustering",
      "Splines",
      "Ensemble methods"
    )
  ),
  list(
    title = 'Visualization',
    color = rgb(1,1,0,0.5),
    col_names = c(
      "Tableau",
      "ggplot",
      "Plotly",
      "MatPlotLib",
      "PowerPoint",
      "Presentation skills"
    )
  ),
  list(
    title = 'IT',
    color = rgb(0,0,1,0.5),
    col_names = c(
      "Github",
      "Software development",
      "Distributed computing",
      "Hacking skills",
      "Unix shell"
    )
  ),
  list(
    title = 'Big data',
    color = rgb(0,1,0,0.5),
    col_names = c(
      "Hadoop Hive Pig",
      "Spark MLib",
      "Cloudera",
      "Elasticsearch",
      "Kibana"
    )
  ),
  list(
    title = 'Programming',
    color = rgb(0.5,0.5,0.5,0.5),
    col_names = c(
      "C++",
      "Java",
      "Scala",
      "SQL",
      "R",
      "Python",
      "Tensorflow",
      "Javascript"
    )
  ),
  list(
    title = 'Math Stat',
    color = rgb(1,0,0,0.5),
    col_names = c(
      "Resampling",
      "Linear Algebra",
      "Probability and information theory",
      "Numerical computation",
      "Hypothesis testing",
      "Multivariate analysis"
    )
  )
)
