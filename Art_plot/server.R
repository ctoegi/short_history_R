library(shiny)

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  load(file="art_plot.RData")
  
  # Fill in the spot we created for a plot
  output$artPlot <- renderPlot({
  
  input$goButton  
  
  art_plot(no.row=input$row,no.col=input$col,
           prob.red=input$red,prob.blue=input$blue,prob.yellow=input$yellow, random_lines=input$randomlines)
     
  })
})
