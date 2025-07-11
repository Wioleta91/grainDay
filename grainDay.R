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
      #actionButton("type_button", "Select type of sediment classification"),
      
      #For easy visualization, this textInput field asks for the sample ID
      #The text will reactively appear on the main field
      textInput(inputId = "sampleID",
                label = "Please insert sample ID:",
                value = "Type sampleID"),
      
      
      #Next, ask the User to type Mass of each fraction in the sample. This will be used to 
      #calculate total mass and percentage of each fraction
      
      actionButton("totalMass_button", "Pick the Love Interest"),
      

      numericInput(inputId = "Gravel_mass",
                   label = "Please insert Gravel Mass",
                   value = NA
      ),
      
      numericInput(inputId = "Sand_mass",
                   label = "Please insert Sand Mass",
                   value = NA
      ),
      
      
     numericInput(inputId = "Silt_mass",
                   label = "Please insert Silt Mass",
                   value = NA
      ),
      
     numericInput(inputId = "Clay_mass",
                   label = "Please insert Clay Mass",
                   value = NA
      )
      
      
      
      
      
      
      ),
    
    #here is the connecting part between output and server logic
    mainPanel(
      h3("Sample ID"),
      textOutput("sampleID", container = span),
      h5("Total mass"),
      #print total mass of the sediment sample - after pressing a button
      textOutput("totalMass", container = span)
      
      
    )
  )
)




# Define server logic required to draw a histogram

server <- function(input, output) {

  #Logic for each of the button option
  
  #Reactive sampleID value
  #instruction for that is in runExample("03_reactivity") 
  
  output$sampleID <- renderText({
    input$sampleID})
  
  
  
  
  totalMass <- reactiveVal("")

  
  #calculating the mass value instruction
  #totalMass <- print("Gravel_mass"+"Sand_mass"+"Silt_mass"+"Clay_mass")
  # When the button is clicked, calculate total mass
  observeEvent(input$totalMass_button, {
    totalMass(sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))
  })
  

    ##Add total mass of the sediment result
    
  

    
    
    
  
  
  
  
  
  
  
  
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)
