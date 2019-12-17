library(shiny)

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Include Bootstrap (NOT IMPLEMENTED)
    includeCSS("bootstrap.min.css"),
    
    # Give the page a title
    titlePanel("Mondrian Randomized" ,windowTitle= "Anti Productive Ideas I"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar 
      sidebarPanel(
        strong("Remake Piet Mondrian's Composition C (No III) with Red, Yellow and Blue"),
          br(),  
        a("View Original on Wikiart...",target="_blank",  href="http://www.wikiart.org/en/piet-mondrian/composition-c-no-iii-with-red-yellow-and-blue-1935"),        
          hr(),
        em("Probability of Field Colors"),
          sliderInput("red", "Red:", 
                     min = 0, max = 0.34, value = 0.08, step= 0.01),
         sliderInput("yellow", "Yellow:", 
                     min = 0, max = 0.34, value = 0.08, step= 0.01),
         sliderInput("blue", "Blue:", 
                     min = 0, max = 0.34, value = 0.08, step= 0.01),
         hr(),
        em("Line strength"),
          checkboxInput("randomlines", "Random", TRUE),
          hr(),
        em("Number of Plots"),
         sliderInput("col", "Vertical:", 
                     min=1, max=10, value=2),
         sliderInput("row", "Horizontal:", 
                       min=1, max=10, value=2),
          actionButton("goButton", "Refresh!"),
          hr()
      ),
      # Create a spot for the plot
      mainPanel(
        plotOutput("artPlot",height = "750px")  
      )
      
    )
  )
)
