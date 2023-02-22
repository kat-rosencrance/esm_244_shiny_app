# Attach necessary packages
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(leaflet)
library(shinythemes)
library(shinyWidgets)
library(plotly)

### READ IN DATA ###

# map stuff
usaLat <- 36.5588659
usaLon <- -107.6660877
usaZoom <- 3

# Seal observations
seal_obs <- read_csv(here("data", "seal_observations.csv")) %>% 
  clean_names()

# Genealogy
# unable to read file probably because it's a gene tree in excel
# genes <- read_xls(here("data", "genealogy.xlsx"))

# Pup and mom data
pup_predict <- read_csv(here("data", "pup_predictions.csv")) %>% 
  clean_names()
pup_data <- read_csv(here("data", "pup_data.csv")) %>% 
  clean_names()

  colnames(pup_data)[1] <- "mother_tag_name"

# view(pup_data)
# colnames(pup_data)


### CREATE THE USER INTERFACE: ###
ui <- fluidPage(
  navbarPage(theme = shinytheme("darkly"),
             title = "Hawaiian Monk Seal",
             
             
             ### FIRST TAB ###
           tabPanel("About the Hawaiian Monk Seal", fluid = TRUE, icon = icon("water"),
                    
                    # sidebar layout
                    sidebarLayout(
                      sidebarPanel("WIDGETS",
                                   actionButton("lifecycle", label = "Life Cycle"),
                                   actionButton("pups", label = "Pups"),
                                   actionButton("surveys", label = "Surveys"),
                                   actionButton("genealogy", label = "Genealogy")
                      ), # end sidebar panel
                      mainPanel("Information about the Monk Seal",
                                textOutput("value")
                      ) # end mainpanel
                    ) # end sidebar layout
           ), ### END FIRST TAB ###
           
           
           
           
           
           ### SECOND TAB ###
           tabPanel("Locations",fluid = TRUE, icon = icon("globe-americas"),
           
           # sidebar layout
           sidebarLayout(
             sidebarPanel("WIDGETS",
                          selectInput(inputId = "pick_beach",
                                      label = "Choose a beach:",
                                      choices = unique(seal_obs$beach_location_name_from_standardized_list)
                          )
             ), # end sidebar panel
             mainPanel("Seal Locations!",
                       leafletOutput(outputId = "beach_map")
             ) # end mainpanel
           ) # end sidebar layout
  ), ### END SECOND TAB ###
  
  
  
  
  
  #  ### THIRD TAB ###
           tabPanel("Seal Characteristics",
                    sidebarLayout(
                      sidebarPanel(
                        prettyCheckboxGroup(
                          "selectsex",
                          label = h4("Select gender"),
                          choices = unique(seal_obs$sex),
                          selected = unique(seal_obs$sex)),
                        prettyCheckboxGroup(
                          "selectlocation",
                          label = h4("Select location"),
                          choices = unique(seal_obs$beach_location_name_from_standardized_list),
                          selected = unique(seal_obs$beach_location_name_from_standardized_list)),
                        prettyCheckboxGroup(
                          "selectsize",
                          label = h4("Select size"),
                          choices = unique(seal_obs$size),
                          selected = unique(seal_obs$size))
                        ),
                      mainPanel(
                        plotlyOutput(outputId = "seal_obs_plot")
                      )# end sidebar panel
  ) # end sidebar layout
  ), # end tab panel
  
  
  
  ### FOURTH TAB ###
  tabPanel("Moms and Pups",
           
           # sidebar layout
           sidebarLayout(
             sidebarPanel("WIDGETS",
                          radioButtons(
                            inputId = "pick_mom",
                            label = "Select a Mom",
                            choices = unique(pup_data$mother_tag_name),
                            selected = c("None Selected" = ""),
                            inline = TRUE,
                            width = '400px')
                          ), # end sidebarpanel 
             mainPanel("Pup info!",
                       textOutput("pick_mom")
             ) #end mainpanel
           ) # end sidebar layout
           ) # end tabpanel
  
  ### END FOURTH TAB###
           ) # end navbarpage 

  
) # end ui

### Create the server function: ###
server <- function(input, output) {

  ### FIRST TAB####
  observeEvent(input$lifecycle, {
    output$value <- renderText({"Monk seals spend two-thirds of their life at sea. They’ll molt completely once a year which helps keep their coat clean and free of algae growth. They can live to over 30 years, though life expectancy is often shorter. They can hold their breath for up to 20 minutes and dive more than 1,800 feet! However, an average dive is much shorter and shallower. Though they don’t migrate, they can travel hundreds of miles throughout the Hawaiian archipelago. " })
  })
  observeEvent(input$pups, {
    output$value <- renderText({"Pup info"})
  })
  observeEvent(input$surveys, {
    output$value <- renderText({"Survey info"})
  })
  observeEvent(input$genealogy, {
    output$value <- renderText({"Genealogy info"})
  })
  
 ### SECOND TAB### 
seal_reactive <- reactive({
  seal_obs %>%
    filter(beach_location_name_from_standardized_list %in% input$pick_beach)
})

output$beach_map <- renderLeaflet({
  leaflet(data = seal_obs) %>%
    setView(lat = usaLat, lng = usaLon, zoom = usaZoom) %>%
    addTiles() %>%
    addMarkers(~x, ~y, popup = ~tag_number, label = ~tag_number)
})

### THIRD TAB ### - we are making a reactive plot
seal_obs_reactive <- reactive({
  message("i am in seal_obs_reactive and I seem to be working")
  seal_obs %>%
    dplyr::filter(sex %in% input$selectsex, # not sure what is happening here
                  location %in% input$selectlocation,
                  size %in% input$selectsize)})

output$seal_obs_plot <- renderPlotly({
  ggplotly(
    ggplot(data = seal_obs_reactive(), aes(x = location, fill = size)) +
      geom_bar(position_dodge2(preserve = "single"), width = 0.5) +
      scale_fill_manual(values = c('steelblue1', 'slategrey'), drop = FALSE) +
      labs(x = "Sex",
           y = "Counts") +
      theme_minimal(),
    tooltip = 'text')})


### FOURTH TAB ###
output$pick_mom <- renderText({
  paste("You chose", input$pick_mom)
  
})


} # end server

# Combine them into an app:
shinyApp(ui = ui, server = server)
