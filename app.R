# Attach necessary packages
library(shiny)
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(shinyWidgets)
library(plotly)

### READ IN DATA ###

# map stuff
usaLat <- 21.196031
usaLon <- -156.969455
usaZoom <- 15

# Seal observations
seal_obs <- read_csv(here("data", "seal_observations.csv")) %>% 
  clean_names() %>%
  mutate(case_when(is.na(sex) ~ "Sex not detected", 
                   TRUE ~ sex))

# Genealogy
# used the data from the left of the original genealogy sheet. May be able to use this one for the mom and pup widget
genes <- read_csv(here("data", "genealogy_fix_kr.csv"))

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
             title = "Hawaiian Monk Seals",
             

             
             
             
             
             
             
                          
             ###FIRST TAB - HOME PAGE & INTRODUCTION###
             tabPanel("Home", fluid = TRUE, icon = icon("home"),
                      mainPanel(h1("Welcome!"),
                                "Hawaiian Monk Seals is an app that lets you visualize Kalaupapa National Historic Park's monk seal survey data .

From the top navigation panel you'll be able to access graphs displaying seal observation data, observed locations, as well as mother and pup characteristics.

It has been made with shiny for the course ESM 244 Advanced Data Analysis at UCSB.",
                      HTML('<center><img src="kala_2.jpeg" alt="Mother and offspring monk seal on the beach" style="height: 580px; width:653px;"/></center>'),
                      )# end mainpanel
                      ), 
            ###END FIRST TAB###
             
                 




                    
                                     
                                     
                              
                                     
            ### SECOND TAB - ABOUT MONK SEALS (MAY BE REPLACED WITH GENEALOGY?) ###
           tabPanel("About Monk Seals", fluid = TRUE, icon = icon("water"),
                    # sidebar layout
                    sidebarLayout(
                      sidebarPanel(HTML('<center><img src="kala_park_pic.PNG" alt="Kalaupapa National Historic Park" style="height: 150px; width:200px;"/></center>'),
                                   hr(),
                                   actionButton("lifecycle", label = "Life Cycle"),
                                   hr(),
                                   actionButton("pups", label = "Pups"),
                                   hr(),
                                   actionButton("surveys", label = "Surveys"),
                                   hr(),
                                   actionButton("genealogy", label = "Genealogy")
                      ), # end sidebar panel

                      mainPanel(          
                      textOutput("value")
                      ) # end mainpanel
                    ) # end sidebar layout
           ), 
            ### END SECOND TAB ###
           
           
           


           
           
           ### THIRD TAB - LOCATIONS ###
           tabPanel("Locations",fluid = TRUE, icon = icon("globe-americas"),
           
           # sidebar layout
           sidebarLayout(
             sidebarPanel(h3("Beaches"),
                          selectInput(inputId = "pick_beach",
                                      label = "Choose a beach:",
                                      choices = unique(seal_obs$beach_location_name_from_standardized_list)
                          )
             ), # end sidebar panel
             mainPanel(h1("Seal Locations!"),
                       leafletOutput(outputId = "beach_map")
             ) # end mainpanel
           ) # end sidebar layout
  ),
        ### END THIRD TAB ###
  
  
  


  
  
        ### FOURTH TAB - SEAL CHARACTERISTICS ###
           tabPanel("Oberved Seal Characteristics",
                    HTML('<center><img src="kala_6.jpeg" alt="Mother and offspring monk seal on the beach" style="height: 580px; width:653px;"/></center>'),
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
  ), 
          ### END FOURTH TAB###
  



  
  
         ### FIFTH TAB - MOMS AND PUPS ###
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
  
      ### END FIFTH TAB###
           ) # end navbarpage 

  
) # end ui





### Create the server function: ###
server <- function(input, output) {

  ### ABOUT SEALS TAB####
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
  
 ### LOCATION TAB### 
seal_reactive <- reactive({
  seal_obs %>%
    filter(beach_location_name_from_standardized_list %in% input$pick_beach)
})

output$beach_map <- renderLeaflet({
  leaflet(data = seal_reactive()) %>%
    addProviderTiles(providers$Thunderforest.Outdoors) %>% 
    setView(lat = usaLat, lng = usaLon, zoom = usaZoom) %>%
    addTiles() %>%
    addMarkers(~x, ~y, popup = ~tag_number, label = ~tag_number)
})

### THIRD TAB ### - we are making a reactive plot
seal_obs_reactive <- reactive({
  message("i am in seal_obs_reactive and I seem to be working")
  seal_third_widget <- 
    seal_obs %>%
    filter(sex %in% input$selectsex,
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
