ui <- fluidPage(
  
  titlePanel("Team Organization"),
  
  sidebarLayout(
    
    sidebarPanel(
      SidebarModuleUI("sidebar")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", PlotModuleUI("plot")),
        tabPanel("Plot (from json)", PlotModuleUI("plot_from_json"))
      )
    )
    
  )
)