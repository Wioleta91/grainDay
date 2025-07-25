library(shiny)
library(shinythemes)
library(leaflet)

# generate the data
dat <- data.frame(
  region = sample(c("A", "B", "C"), 100, replace = TRUE),
  x = rnorm(100, mean = 50, sd = 10),
  y = rnorm(100, mean = 40, sd = 10)
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("united"),
  # Application title
  titlePanel("Test"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "regionInput", 
        label = h3("Region"),
        choices = c("(All)", "A", "B", "C"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
)


# Define server logic to map the distribution
server <- shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({
    if (input$regionInput == "(All)") {
      idx <- rep(TRUE, nrow(dat))
    } else {
      idx <- dat$region == input$regionInput
    }
    leaflet(dat) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addCircleMarkers(color = "blue",
                       radius = 2, 
                       stroke = FALSE, 
                       fillOpacity = 0.5, 
                       lng = dat[idx, "x"], 
                       lat = dat[idx, "y"])
  })
})

# Run the application 
shinyApp(ui, server)
