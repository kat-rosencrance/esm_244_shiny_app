# Attach necessary packages
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(leaflet)
library(shinythemes)

### READ IN DATA ###

### map stuff
usaLat <- 36.5588659
usaLon <- -107.6660877
usaZoom <- 3

### Seal observations
seal_obs <- read_csv(here("data", "seal_observations.csv")) %>% 
  clean_names()

### Genealogy
# unable to read file probably because it's a gene tree in excel
# genes <- read_xls(here("data", "genealogy.xlsx"))

### Pup and mom data
pup_predict <- read_csv(here("data", "pup_predictions.csv")) %>% 
  clean_names()
pup_data <- read_csv(here("data", "pup_data.csv")) %>% 
  clean_names()



### CREATE THE USER INTERFACE: ###
ui <- fluidPage(
  navbarPage(theme = shinytheme("darkly"),
             title = "Hawaiian Monk Seal",
             
             
             
             ### FIRST TAB ###
           tabPanel("Locations", fluid = TRUE, icon = icon("globe-americas"),
                    
                    # sidebar layout
                    sidebarLayout(
                      sidebarPanel("WIDGETS",
                                   selectInput(inputId = "pick_beach",
                                                      label = "Choose a beach:",
                                                      choices = unique(seal_obs$beach_location_name_from_standardized_list)
                                   )
                      ),
                      mainPanel("Seal Locations!",
                                leafletOutput(outputId = "beach_map")
                      ) # end mainpanel
                    ) # end sidebar layout
           ), ### END FIRST TAB ###
           
           
           
           
           
           ### SECOND TAB ###
           tabPanel("Genealogy"),
           tabPanel("Seal Characteristics"),
           tabPanel("Moms and Pups")
) # end navbarpage
) # end ui


### Create the server function: ###
server <- function(input, output) {

 ### FIRST TAB### 
seal_reactive <- reactive({
  seal_obs %>%
    filter(beach_location_name_from_standardized_list %in% input$pick_beach)
})

output$beach_map <- renderLeaflet({
  leaflet(data = seal_reactive) %>%
    setView(lat = usaLat, lng = usaLon, zoom = usaZoom) %>%
    addTiles() %>%
    addMarkers(~x, ~y, popup = ~tag_number, label = ~tag_number)
})
}
# Combine them into an app:
shinyApp(ui = ui, server = server)
