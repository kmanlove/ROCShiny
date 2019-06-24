# ui.R builds the user interface for the shiny app. 
# the functionality behind most of the pages is contained in
# server.R.

require(leaflet)
require(maptools)
require(rgdal)
require(ggplot2)
require(rgeos)

navbarPage("Risk of Contact Tool",
           tabPanel("Overview", uiOutput("overviewPage")), #
           tabPanel("Load data", uiOutput("loadDataPage")),
           tabPanel("View data", uiOutput("viewDataPage")),
           tabPanel("Specify inputs", uiOutput("specifyInputsPage")),
           tabPanel("View results", uiOutput("viewResultsPage")),
#           tabPanel("Allotment boundaries", uiOutput("livestockPage")),
#           tabPanel("Season", uiOutput("seasonalityPage")),
           tabPanel("Download", uiOutput("downloadPage"))
)