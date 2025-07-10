#This program takes in percentage of grain type in a sediment and calculates sediment type
#Classifying sediment types based on grain size is a well-scoped, achievable project
#The project has to be broken down into small steps
#Sediment classification type

library(shiny)
#library(bslib)

setwd("~/Documents/grainDay")


# Define UI for grainDay app

ui <- fluidPage( 

  #App Title
  titlePanel("GrainDay"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("type_button", "Select type of sediment classification"),
      
      ),
    
    mainPanel(
      textOutput("Style")
      
    )
  )
)




# Define server logic required to draw a histogram

server <- function(input, output) {
  
  
  
  
  
  
  
  
  
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)
