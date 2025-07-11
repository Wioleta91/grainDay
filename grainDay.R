#This program takes in percentage of grain type in a sediment and calculates sediment type
#Classifying sediment types based on grain size is a well-scoped, achievable project
#The project has to be broken down into small steps
#Sediment classification type

library(shiny)
#library(bslib)

setwd("~/Documents/grainDay")


# Define UI for grainDay app

ui <- fluidPage( 
  
  #here are details on what buttons are displayed in the sidebar

  #App Title
  titlePanel("GrainDay"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #Later different types of classification might be added
      actionButton("type_button", "Select type of sediment classification"),
      
      #For easy visualization, this textInput field asks for the sample ID
      #The text will reactively appear on the main field
      textInput(inputId = "sampleID",
                label = "Please insert sample ID:",
                value = "Type sampleID"),
      
      
      #Next, ask the User to type Mass of their sample. This will be used to 
      #calculate percentage of each grain Fraction
      
      numericInput(inputId = "Total_mass",
                   label = "Please insert Total Mass",
                   value = "0"
                   )
      
      
      
      
      
      
      
      
      ),
    
    #here is the connecting part between output and server logic
    mainPanel(
      textOutput("Style")
      
    )
  )
)




# Define server logic required to draw a histogram

server <- function(input, output) {

  #Logic for each of the button option
  
  #Reactive sampleID value
  #instruction for that is in runExample("03_reactivity") 
  
  
  
  
  
  
  
  
  
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)
