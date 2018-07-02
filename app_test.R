# https://github.com/rstudio/shiny-examples/blob/master/086-bus-dashboard/ui.R
# put in shiny dashboard


library(shiny)
library(crosstalk)
library(sp)
library(leaflet)
library(d3scatter)
library(plotly)

#setwd("SDAIapp")
shapes_to_filter <- rgdal::readOGR("boundary/LA/LA.shp") # rgdal import to 'Spatial Object'
shapes_to_filter <- spTransform(shapes_to_filter, CRS("+init=epsg:4326"))
councilnames<-readr::read_csv("boundary/councilnames.csv")
shapes_to_filter<-merge(shapes_to_filter, councilnames, by="NAME")

## add in the earnings
library(dplyr)
earnings<- readr::read_csv("macroeconomy/earnings.csv")


LAallearnings<-earnings %>%
  tidyr::spread(., DateCode, Value) %>%
  dplyr::filter(Gender=="All", `Population Group`=="Residence Based")%>%
  dplyr::rename(., Council = FeatureCode) %>%
  dplyr::select(-one_of(c("1998", "1999", "2000", "Gender", "Population Group"
                          , "Units", "Measurement"))) %>%
  tidyr::gather(., key=Year, value=Value,-Council) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  na.omit() 

shapes_to_filter<-merge(shapes_to_filter, LAallearnings, by="Council", duplicateGeoms = TRUE)

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
      filter_select("filterid",  "Council", sd_df, ~NAME)),
    
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
      addPolygons(data = sd_map)
    
  })
  
  output$DTtable = DT::renderDataTable({sd_df}, server=F)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

