#Author: Wioleta Rasmus (PhD)
#contact: wiolarasmus@gmail.com
#github: Wioleta91
#This program takes in entry weight of soil sample
#Calculates % of sand, silt and clay based on the user weight input
#!!Work in progress - the sediment/soil type is not yet functional!!
#The samples are then plotted on a USDA soil type chart available from the ggtern package
#The graph can be downloaded, as well as the data table
#Summary is also available and plain ternary chart with your samples
#I hope to further develop this app and add following features: uploading your own csv data, 
#more flexibility on which samples to plot, 
#and most importantly a few different sediment and soil classification graphs and methods
#Contact me for questions and possible collaborations! 
#
#This is my first Shiny app, be nice :)
#For more information: https://github.com/Wioleta91/grainDay

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
  
  theme = bs_theme(bootswatch = "cerulean", base_font = font_google("Open Sans")),
  tags$head(
    tags$style(HTML("
    .well {
      padding: 10px !important;
    }
    .form-group {
      margin-bottom: 10px;
    }
    input[type='text'], input[type='number'] {
      max-width: 100%;
      font-size: 14px;
    }
  "))
  ),
  
  #here are details on what buttons are displayed in the sidebar

  # App Title
  tags$div(
    style = "background-color:#2C3E50; color:white; padding:20px; margin-bottom:30px; border-radius:5px;",
    h2("GrainDay: Soil and Sediment Classification Tool"),
    p("Analyze grain fractions and visualize samples on ternary diagrams")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      # Future ideas: different types of classification might be added
      # actionButton("type_button", "Select type of sediment classification"),
      
      # For easy visualization, this textInput field asks for the sample ID
      # The text will reactively appear on the main field
      # To resolve: The active sample can be used to calculate ternary diagram
      h4("Define Sample"),
      textInput(inputId = "sampleID",
                label = "Please insert sample ID:",
                value = "Type sampleID"),
      
      textInput(inputId = "sampleLocation",
                label = "Please insert sample Location:",
                value = "Type sample Location"),
      
  
      
      # Next, ask the User to type Mass of each fraction in the sample. 
      # This will be used to 
      # calculate total mass and percentage of each fraction
      
      h4("Define mass of the sample and each fraction"),
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
      fluidRow(
      column(width = 6, 
            h5("Recently added sample - ID:"),
            textOutput("sampleID", container = span),
    
            h5("Recently added sample - Total mass"),
            #print total mass of the sediment sample - after pressing a button
            textOutput("totalMass_output"),
            #Button to calculate statistics - make it work only for >10 sample inputs
            actionButton("stats_Button", "Calculate Summary Statistics"),
            br(),
            #If you want to plot a single sample from the data Table - next add a button to plot all
            selectInput("selected_sample_output", "(In progress)Choose to plot:", choices = NULL),
      ),
      
      column(
      width = 6, 
      #button to download Table
      downloadButton("downloadData", "Download Data Table"),
      br(), br(),
      #button to download Plot
      downloadButton("downloadPlot", "Download USDA Ternary Chart"),
      br(), br(),
      #button to download Plot
      downloadButton("downloadPlot_Clean", "Download Plain Ternary Chart")
    
      )
      ),
      #consider output format 
      #this code has separate tabs for the plot, summary and table
      tabsetPanel(
        tabPanel("🧮 Data Table", tableOutput("result")),
        tabPanel("🌱 USDA Soil Plot", plotOutput("plot")),
        tabPanel("📈 Clean Ternary", plotOutput("plot_Clean")),
        tabPanel("📊 Summary", verbatimTextOutput("summary"))
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
  
  
  #reactive USDA soil plot
  
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
  
  
  #reactive clean ternary plot
  
  clean_plot_reactive <- reactive({
    req(input$update_button)
    ternPlot_clean +
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

  
  
  # attach the plots to correct Tabs
  output$plot <- renderPlot({
    plot_reactive()
  })
  
  output$plot_Clean <- renderPlot({
    clean_plot_reactive()
  })
  

   
  # Download the data table and save as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", ".csv", sep="")
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
  
  # Download the clean chart
  output$downloadPlot_Clean <- downloadHandler(
    filename = function() {
      "chart_clean.png"
    },
    content = function(file) {
      ggsave(file, plot = clean_plot_reactive(), width = 290, height = 265, units = "mm", device = "png")
    }
  )  
  
  
  

}
  
  
  
  
  

# Run the application 
shinyApp(ui = ui, server = server)
