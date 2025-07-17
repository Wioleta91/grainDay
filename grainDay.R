#This program takes in percentage of grain type in a sediment and calculates sediment type
#Classifying sediment types based on grain size is a well-scoped, achievable project
#The project has to be broken down into small steps
#Sediment classification type

library(shiny)
#library(bslib)

setwd("~/Documents/grainDay")


#data frame for appending the new inputs
df <- data.frame(sampleID = character(),
                 Init_mass = numeric(), #weight of the sample before sieve
                 Sediment_mass = numeric(),
                 Loss_weight = numeric(), #% of sample lost
                 Gravel_perc = numeric(),
                 Sand_perc = numeric(),
                 Silt_perc = numeric(),
                 Clay_perc = numeric(),
                 Location = character(),
                 Shepherd_Class = character(), #type of sediment based on classification which?
                 stringsAsFactors = FALSE)


# Define UI for grainDay app

ui <- fluidPage( 
  
  #here are details on what buttons are displayed in the sidebar

  #App Title
  titlePanel("GrainDay"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Future ideas: different types of classification might be added
      # actionButton("type_button", "Select type of sediment classification"),
      
      # For easy visualization, this textInput field asks for the sample ID
      # The text will reactively appear on the main field
      # To resolve: The active sample can be used to calculate ternary diagram
      textInput(inputId = "sampleID",
                label = "Please insert sample ID:",
                value = "Type sampleID"),
      
      textInput(inputId = "sampleLocation",
                label = "Please insert sample Location:",
                value = "Type sample Location"),
      
  
      
      # Next, ask the User to type Mass of each fraction in the sample. 
      # This will be used to 
      # calculate total mass and percentage of each fraction
      
      numericInput(inputId = "Initial_mass",
                   label = "Please insert sample mass before sieving",
                   value = NA
      ),
      

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
     
     # This action button calculates total mass of sediments
     # It also stores the data input to the Table with calculated percentages of each fraction
     # Additionally, stores the sample Location (collection) data
     actionButton("update_button", #update instead of totalMass_button
                  "Calculate Parameters"),
     
     
      
      ),
    
    # Here is the connecting part between output and server logic
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
        tabPanel("Table", tableOutput("result")),
        tabPanel("Summary", verbatimTextOutput("summary"))
        
      )
      
      
    )
  )
)




# Define server logic required to draw a histogram
# First - the Update button does percentage calculations

server <- function(input, output) {
  

  # Reactive sampleID value
  # instruction for that is in runExample("03_reactivity") 
  
  output$sampleID <- renderText({
    input$sampleID})
  output$sampleLocation <- renderText({
    input$sampleLocation})
  
  
  # Calculating the total mass value of sediment fractions instruction
  # When the button is pushed, calculate total mass
  # And calculate the % of each fraction and append them to the Table
  
  # Storing each parameter as a reactive Value
  totalMass <- reactiveVal("")
  loss <- reactiveVal("")
  gravels <- reactiveVal("")
  sands <- reactiveVal("")
  silts <- reactiveVal("")
  clays <- reactiveVal("")
  
  #just output of total mass in the main field below sample ID (not in Table)
  output$totalMass_output <- renderText({
    totalMass()
    
  })
  
  
  
  # starting a reactive data frame
  df_server <- reactiveVal(df)
  
  

  
  #

  observeEvent(input$update_button, {
    
    totalMass(sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))
    loss(((input$Initial_mass-totalMass())/input$Initial_mass)*100)
    gravels(100*input$Gravel_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))
    sands(100*input$Sand_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))
    silts(100*input$Silt_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))
    clays(100*input$Clay_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))
    # Initial sample mass and % loss has to be added here
    
    # Render the data frame
    temp_df <- rbind(df_server(),data())
    df_server(temp_df)

  })
  
  
  
  ## Apply calculated values to the DF
  
  data <- reactive({
    req(input$update_button)
    data.frame(sampleID = input$sampleID,
               Init_mass = input$Initial_mass,
               Loss_weight = loss(),
               Sediment_mass = totalMass(),
               Gravel_perc = gravels(),
               Sand_perc = sands(),
               Silt_perc = silts(),
               Clay_perc = clays(),
               Location = input$sampleLocation
               
               
               )
  })
  
  output$result <- renderTable(df_server())
  

}
  
  
  
  
  

# Run the application 
shinyApp(ui = ui, server = server)
