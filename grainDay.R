#This program takes in percentage of grain type in a sediment and calculates sediment type
#Classifying sediment types based on grain size is a well-scoped, achievable project
#Sediment classification type

library(shiny)
library(bslib)

setwd("~/Documents/grainDay")
source("grainDay/helper_fractionType.R")
source("grainDay/helper_ternaryPlot.R")


#data frame for appending the new inputs
df <- data.frame(sampleID = character(),
                 Init_mass = numeric(), # weight of the sample before sieve
                 Sediment_mass = numeric(),
                 Loss_weight = numeric(), # % of sample lost
                 # Gravel_perc = numeric(), # might be removed
                 Sand = numeric(),
                 Clay = numeric(),
                 Silt = numeric(),
                 Location = character(),
                 Shepard_Class = character(), # type of sediment based on classification which?
                 stringsAsFactors = FALSE)



# Define UI for grainDay app

ui <- fluidPage( 
  
  #here are details on what buttons are displayed in the sidebar

  # App Title
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
      

      #numericInput(inputId = "Gravel_mass",
      #             label = "Please insert Gravel Mass",
      #             value = NA
      # ),
      
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
     # Additionally, stores the sample Location (collection site) data
     actionButton("update_button",
                  "Calculate Parameters"),
     
     
      
      ),
    
    # Here is the connecting part between output and server logic
    mainPanel(
            h5("Current sample ID:"),
            textOutput("sampleID", container = span),
            h5("Total mass"),
            #print total mass of the sediment sample - after pressing a button
            textOutput("totalMass_output", container = span),
      
      #button to download Table
      downloadButton("downloadData", "Data Table"),
      
      #button to download Plot
      downloadButton("downloadPlot", "Chart"),
      
      
      
      #Button to calculate statistics - make it work only for >10 sample inputs
      actionButton("stats_Button", "Calculate Summary Statistics"),
      
      #If you want to plot a single sample from the data Table - next add a button to plot all
      selectInput("selected_sample_output", "Choose a sample to plot:", choices = NULL),
      
      #consider output format 
      #this code has separate tabs for the plot, summary and table
      tabsetPanel(
        tabPanel("Data Table", tableOutput("result")),
        tabPanel("Ternary Plot", plotOutput("plot")),
        tabPanel("Summary", verbatimTextOutput("summary")) #generate summary from Data Table
        
      )
      
      
    )
  )
)




# Define server logic required to draw a histogram
# First - the Update button does percentage calculations

server <- function(input, output, session) {
  

  # Reactive sampleID value
  # instruction for that is in runExample("03_reactivity") 
  
  output$sampleID <- renderText({
    input$sampleID})
  output$sampleLocation <- renderText({
    input$sampleLocation})
  
  
  # When the update_button is pushed, calculate the total sample mass
  # Then calculate the % of each fraction and append them to the Table
  # Also calculates % of sample lost during measurement
  
  # Storing each parameter as a reactive Value
  totalMass <- reactiveVal("")
  loss <- reactiveVal("")
  sands <- reactiveVal("")
  silts <- reactiveVal("")
  clays <- reactiveVal("")
  
  # gravels <- reactiveVal("") - it might be added later for different sediment classifications

  
  #Shows sample ID and the total mass of the most recently added sample in the main field
  output$totalMass_output <- renderText({
    totalMass()
  })
  
  output$selected_sample_output <- renderText({
    sampleID()
    })
    
  
  
  # Starting a reactive data frame
  df_server <- reactiveVal(df) # takes the df initiated before the UI
  stats_calc <- reactiveValues()
  

  # Observe event when the buttons are clicked

  observeEvent(input$update_button, { #main calculations
    req(input$Initial_mass, input$Sand_mass, input$Silt_mass, input$Clay_mass)
    totalMass(sum(input$Sand_mass+input$Silt_mass+input$Clay_mass))
    loss(((input$Initial_mass-totalMass())/input$Initial_mass)*100)
    sands(100*input$Sand_mass/sum(input$Sand_mass+input$Silt_mass+input$Clay_mass))
    silts(100*input$Silt_mass/sum(input$Sand_mass+input$Silt_mass+input$Clay_mass))
    clays(100*input$Clay_mass/sum(input$Sand_mass+input$Silt_mass+input$Clay_mass))
    # gravels(100*input$Gravel_mass/sum(input$Gravel_mass+input$Sand_mass+input$Silt_mass+input$Clay_mass))
    

    
    # Render the data frame
    temp_df <- rbind(df_server(),data())
    df_server(temp_df)
    
    # This is applied to the drop down list
    updateSelectInput(session, "selected_sample_output",
                      choices = df_server()$sampleID)
    
    


  })
  
  # When stats button is pressed, calculate basic summary from the data table
  observeEvent(input$stats_Button, {
    req(input$Initial_mass, input$Sand_mass, input$Silt_mass, input$Clay_mass)
    stats_calc$result <- summary(
      df_server())
  })  
  
  
  ## Apply calculated mass and fraction % values to the DF
  
  data <- reactive({
    req(input$update_button)
    data.frame(sampleID = input$sampleID,
               Init_mass = input$Initial_mass,
               Loss_weight = loss(),
               Sediment_mass = totalMass(),
               # Gravel_perc = gravels(),
               Sand = sands(),
               Clay = clays(),
               Silt = silts(),
               Location = input$sampleLocation,
               Shepard_Class = clay_func(sands(), silts(), clays()) #this is not fully checked yet
               
               
               )
  })
  
  # Render all the outputs
  output$result <- renderTable(df_server())
  output$summary <- renderPrint({
    req(stats_calc$result)
    stats_calc$result
  })  
  
  
  #reactive plot
  
  plot_reactive <- reactive({
    req(input$update_button)
    plot_Tern +
      geom_point(
        data = df_server(),
        mapping = aes(
          Sand,
          Clay,
          Silt,
          color = Location
        ),
        size = 3,
        inherit.aes = FALSE
      )
  })
  
  output$plot <- renderPlot({
    plot_reactive()
  })

   
  # Download the data table and save as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", ".csv", sep=",")
    },
    content = function(file) {
      write.csv(df_server(), file, row.names = FALSE)
    }
  )  
  
  # Download the chart
  output$downloadPlot <- downloadHandler(
    filename = function() {
      "chart.png"
    },
    content = function(file) {
      ggsave(file, plot = plot_reactive(), width = 290, height = 265, units = "mm", device = "png")
    }
  )  
  
  
  

}
  
  
  
  
  

# Run the application 
shinyApp(ui = ui, server = server)
