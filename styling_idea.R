mainPanel(
  fluidRow(
    column(6,
           h5("Sample ID:"),
           textOutput("sampleID")
    ),
    column(6,
           h5("Total Mass:"),
           textOutput("totalMass_output")
    )
  ),
  
  fluidRow(
    column(4, actionButton("downloadTable_button", "Download Table")),
    column(4, actionButton("some_other_button", "Do Something"))
  ),
  
  tabsetPanel(
    tabPanel("Plot", plotOutput("plot")),
    tabPanel("Table", tableOutput("result")),
    tabPanel("Summary", verbatimTextOutput("summary"))
  )
)