#This program takes in percentage of grain type in a sediment and calculates sediment type
#The project has to be broken down into small steps
#Sediment classification type

library(shiny)
setwd("~/Documents/grainDay")


# Define UI for grainDay app

ui <- fluidPage( #fluid page so things are in rows ?

  #App Title
  titlePanel(GrainDay),
  
  sidebarLayout(
    inputPanel(
      actionButton("type_button", "Select type of sediment classification"),
      
      ),
    
    mainPanel(
      textOutput("Style")
      
    )
  )
)




# Define server logic required to draw a histogram

server <- function(input, output, session) {
  
  
  
}




# Run the application 
