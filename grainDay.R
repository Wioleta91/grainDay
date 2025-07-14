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
      
      textInput(inputId = "sampleLocation",
                label = "Please insert sample Location:",
                value = "Type sample Location"),
      
      
      #Next, ask the User to type Mass of each fraction in the sample. This will be used to 
      #calculate total mass and percentage of each fraction
      
      

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
      ),
     
     #this button calculates mass of sample
     # the click will also store the data for table/pie chart 
     actionButton("update_button", #update instead of totalMass_button
                  "Calculate Parameters"),
     
     
      
      ),
    
    #here is the connecting part between output and server logic
    mainPanel(
      h5("Sample ID:"),
      textOutput("sampleID", container = span),
      h5("Total mass"),
      #print total mass of the sediment sample - after pressing a button
      textOutput("totalMass_output", container = span),
      
      #consider output format 
      #this code has separate tabs for the plot, summary and table
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Table", tableOutput("table"))
      )
      
      
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
  #output$sampleLocation <- renderText({
  # input$sampleLocation})
  
  output$update_button <- renderTable({
    input$update_button})
  
  
  
  # Calculating the total mass value instruction
  # When the button is pushed, calculate total mass
  # UPDATE: this button will calculate the % of each fraction and append them to the Table
  
  #
  totalMass <- reactiveVal("")
  gravels <- reactiveVal("")
  sands <- reactiveVal("")
  silts <- reactiveVal("")
  clays <- reactiveVal("")
  
  # this will be the correct way to use the button
  
  

  
  #

  observeEvent(input$update_button, {
    
    totalMass(sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))
    gravels(100*input$Gravel_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))
    sands(100*input$Sand_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))
    silts(100*input$Silt_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))
    clays(100*input$Clay_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))
    #should i add the other conditions here? this is not working yet

  })
  
  
  #just output of total mass in the main field below sample ID (not in tabs)
  output$totalMass_output <- renderText({
    totalMass()
    
    })
  
  #output$gravels_output <- renderText({
  #  gravels()
  #})
  
  
  
  
  output$table <- renderTable({
    data.frame(
      ID = c(input$sampleID),
      Sediment_mass = c(totalMass()),
      Gravel_perc = c(gravels()),#c((100*input$Gravel_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))),
      Sand_perc = c(sands()),#c((100*input$Sand_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))),
      Silt_perc = c(silts()),#c((100*input$Silt_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))),
      Clay_perc = c(clays()),#c((100*input$Clay_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))),
      Location = c(input$sampleLocation)
      
      
    )
    
  })
  
  #other outputs being saved?

  # saving inserted values into a data frame
  
  #datasetInput <- reactive("")
  
#  output$view <- renderTable({
#    head(datasetInput(), n = input$obs)
# })
  

 
}



# Run the application 
shinyApp(ui = ui, server = server)
