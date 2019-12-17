server <- function(input, output) {
  SideBar <- callModule(SidebarModule, "sidebar")
  callModule(PlotModule, "plot", sidebar_args = SideBar, data_source = 'sliders')
  callModule(PlotModule, "plot_from_json", sidebar_args = SideBar, data_source = 'uploaded_json')
}