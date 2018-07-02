# https://github.com/rstudio/shiny-examples/blob/master/086-bus-dashboard/ui.R
# put in shiny dashboard


library(shiny)
library(crosstalk)
library(sp)
library(leaflet)
library(d3scatter)
library(plotly)

#setwd("SDAIapp")
shapes_to_filter <- rgdal::readOGR("boundary/LA/LA_withattr.shp") # rgdal import to 'Spatial Object'

## create the shared data 
sd_map <- SharedData$new(shapes_to_filter)
sd_df <-  SharedData$new(as.data.frame(shapes_to_filter@data), group = sd_map$groupName())


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("The Recession and Mental Health"),
  
  # Sidebar 
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      filter_select("filterid",  "Council", sd_df, ~NAME),
      filter_select("filterid",  "Year", sd_df, ~Year)),
    
    # Show a plot of the generated distribution
    mainPanel(

   # tabs
  tabPanel(
    "2 columns",
    fluidRow(
      column(width = 5,
             h2("Scatter"),
             plotlyOutput("plotlygraph")),
      column(width = 5,
             h2("Map"),
             leafletOutput("LAmap")),
      column(width = 10,
             h2("Table"),
             DT::dataTableOutput("DTtable"))
    ))
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plotlygraph <- renderPlotly({
    plot_ly(sd_df, x=~Year, y=~Value, color=~NAME,
            showlegend = FALSE, type = 'scatter')
  })
  
  library(leaflet)
  output$LAmap<-renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolygons(data = sd_map,
                  popup="hello")
    
  })
  
  output$DTtable = DT::renderDataTable({sd_df}, server=F, pageLength = 5)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

