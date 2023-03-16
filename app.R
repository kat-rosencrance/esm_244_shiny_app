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
library(collapsibleTree)
library(DT)
library(knitr)

### READ IN DATA ###

# map stuff
usaLat <- 21.196031
usaLon <- -156.969455
usaZoom <- 15

# Seal observations
seal_obs <- read_csv(here("data", "seal_observations.csv")) %>% 
  clean_names() %>%
  select(beach_location_name_from_standardized_list, sex, size, x, y, tag_number) %>% #rename for sanity
  mutate(sex = case_when(is.na(sex) ~ "Sex not detected", 
                         TRUE ~ sex)) %>%
  mutate(beach_location_name_from_standardized_list = str_to_title(beach_location_name_from_standardized_list)) %>% 
  drop_na() %>%
  mutate(size = case_when(size == "A" ~ "Adult",
                   size == "U" ~ "Unknow",
                   size == "N" ~ "Nursing Pup",
                   size == "W" ~ "Weaned Pup",
                   size == "J" ~ "Juvenile",
                   size == "S" ~ "Subadult",
                   size == "U" ~ "Unknow",
                   size == "N" ~ "Nursing Pup",
                   size == "W" ~ "Weaned Pup",
                   size == "J" ~ "Juvenile",
                   size == "S" ~ "Subadult")) %>%
  mutate(beach_location_name_from_standardized_list = case_when(beach_location_name_from_standardized_list == "Kahikilo (Carpenters) Beach, Kalaupapa" ~ "Kahikilo Beach", TRUE ~ beach_location_name_from_standardized_list)) %>%
  mutate(sex = case_when(sex == "F" ~ "Female",
                         sex == "M" ~ "Male",
                         sex == "U" ~ "Unknown",
                         sex == "Sex not detected" ~ "Unknown"))


# Genealogy
# used the data from the left of the original genealogy sheet. May be able to use this one for the mom and pup widget
gene_df_new <- read_csv(here("data", "gene_updated_tidy_kat.csv")) 

moms_gene <- sort(as.character(unique(gene_df_new[,c("mom")])))
#moms_gene

# Pup and mom data
pup_predict <- read_csv(here("data", "pup_predictions.csv")) %>% 
  clean_names()
pup_data <- read_csv(here("data", "pup_data.csv")) %>% 
  clean_names()

colnames(pup_data)[1] <- "mother_tag_name"

# view(pup_data)
# colnames(pup_data)

pup_table_data <- pup_data %>%
  select(mother_tag_name, pup_tags, pup_first_observed, pup_weaned) %>%
  drop_na()




### CREATE THE USER INTERFACE: ###
ui <- fluidPage(
  
  navbarPage(theme = shinytheme("flatly"),
             header = HTML('<center><img src="park_horizontal.jpg" alt="Kalaupapa National Historic Park" style="display: block; margin-left: auto; margin-right: auto; height: 150px; width: 100% "/></center>'),
             hr(),
             title = "Hawaiian Monk Seals at Kalaupapa National Historical Park",
             
             
             
             
             
             
             ###FIRST TAB - HOME PAGE & INTRODUCTION###
             tabPanel("Home", fluid = TRUE, icon = icon("home"),
                      sidebarLayout(
                        sidebarPanel(width = 2,
                                     actionButton("aboutapp", label = "About the App"),
                                     br(),
                                     br(),
                                     actionButton("lifecycle", label = "Seal Life Cycle"),
                                     br(),
                                     br(),
                                     actionButton("surveys", label = "NPS Surveys"),
                                     
                                     
                        ), # end sidebar panel
                        mainPanel(h1("Welcome!"),
                                  br(),
                                  br(),
                                  HTML('<center><img src="kala_2.jpeg" alt="Mother and offspring monk seal on the beach" style="height: 200px; width:250px;"/></center>'),
                                  br(),
                                  htmlOutput("value")
                        )) # end mainpanel
             ), 
###END FIRST TAB###











### SECOND TAB - GENEALOGY ###
tabPanel("Genealogy Tree", fluid = TRUE, icon = icon("tree"),
         mainPanel(h1("Generations of Seals at Kalaupapa National Historical Park"),
           includeMarkdown("www/genealogy.Rmd"),
           ##trying gene stuff
           selectInput(inputId = "selectedmom",
                       label = "Select a Mom",
                       choices = unique(gene_df_new$mom)),
           collapsibleTreeOutput('tree', height='700px')
         ) # end mainpanel
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
           mainPanel(h1("Seal Locations"),
                     includeMarkdown("www/locations.Rmd"),
                     leafletOutput(outputId = "beach_map")
           ) # end mainpanel
         ) # end sidebar layout
),
### END THIRD TAB ###







### FOURTH TAB - SEAL CHARACTERISTICS ###
tabPanel("Oberved Seal Characteristics", fluid = TRUE, icon= icon("binoculars"),
         HTML('<center><img src="kala_6.jpeg" alt="Mother and offspring monk seal on the beach" style="height: 200px; width:250px;"/></center>'),
         sidebarLayout(
           sidebarPanel(
             prettyCheckboxGroup(
               "selectsex",
               label = h4("Select gender"),
               choices = unique(seal_obs$sex),
               selected = unique(seal_obs$sex)[1]),
             prettyCheckboxGroup(
               "selectlocation",
               label = h4("Select location"),
               choices = unique(seal_obs$beach_location_name_from_standardized_list),
               selected = unique(seal_obs$beach_location_name_from_standardized_list)[1]),
             prettyCheckboxGroup(
               "selectsize",
               label = h4("Select size"),
               choices = unique(seal_obs$size),
               selected = unique(seal_obs$size)[1])
           ),
           mainPanel(h1("Plot Seal Characteristics"),
                     includeMarkdown("characteristics.Rmd"),
             plotOutput(outputId = "seal_obs_plot")
           )# end sidebar panel
         ) # end sidebar layout
), 
### END FOURTH TAB###






### FIFTH TAB - MOMS AND PUPS ###
tabPanel("Moms and Pups", fluid = TRUE, icon = icon("heart"),
         
         # sidebar layout
         sidebarLayout(
           sidebarPanel("Select a mom to see data about her pups! This data was collected through surveys completed by the National Parks Service. You can read more about the survey data on the home page.",
                        prettyCheckboxGroup(
                          "pick_mom",
                          label = h4("Select a Mom"),
                          choices = unique(pup_table_data$mother_tag_name),
                          selected = unique(pup_table_data$mother_tag_name)[1])
           ), # end sidebarpanel 
           mainPanel("Pup info!",
                     DT::dataTableOutput("pick_mom")
           ) #end mainpanel
         ) # end sidebar layout
) # end tabpanel

### END FIFTH TAB###
  ) # end navbarpage 


) # end ui





### Create the server function: ###
server <- function(input, output) {
  
  ### ABOUT SEALS TAB####
  
 observeEvent(input$aboutapp, {
   output$value <- renderText({HTML("<em>Hawaiian Monk Seals at Kalaupapa National Historical Park</em> is an app that lets you visualize Kalaupapa National Historic Park's monk seal survey data.
     From the top navigation panel you'll be able to access a seal genealogy tree, observed seal locations, graphs displaying seal observation data, as well as mother and pup characteristics.
     Additionally, using the side panel to the left, you can dive into the life history of this special marine species. <br> <br>
     This app was created by Katherine Rosencrance, Annie Combs, and Alessandra Puig-Santana for the course ESM 244 - Advanced Data Analysis at the University of Santa Barbara.
     Data were obtained from Glauco Puig-Santana, a biological technician working at Kalaupapa National Historical Park.
     The data are not publicly available, but the original data files contained seal and pup observations dating back to 2021 and genealogy data beginning in 1997.
     The side panel tab 'NPS Surveys' details the data collection methods used for the data included in this app. Enjoy!")
   })
 })
  
  observeEvent(input$lifecycle, {
    output$value <- renderText({HTML("Monk seals spend two-thirds of their life at sea.
      They’ll molt completely once a year which helps keep their coat clean and free of algae growth.
      They can live to over 30 years, though life expectancy is often shorter. 
      They can hold their breath for up to 20 minutes and dive more than 1,800 feet!
      However, an average dive is much shorter and shallower.
      Though they don’t migrate, they can travel hundreds of miles throughout the Hawaiian archipelago.
      
      Monk seals average around 7 – 7.5 feet long and can weigh an average of 375-450 pounds.
      The Hawaiian monk seal is endangered, having been hunted almost to extinction in the 19th century, with only about 1,200 individuals at last count in 2016 in their native habitats in the Hawaiian Islands.")})})

  observeEvent(input$surveys, {
    output$value <- renderText({HTML("At Kalaupapa, we coordinate with the National Park Service to help conserve monk seals. A park marine biologist walks along beaches, spots the seals, takes photographs, and records the data observed.
      These surveys track when the new pups are born, how much they grow, and when they wean from their moms.
      Marine biologists look for identifying tags, bleach marks, and scars to tell monk seals apart.
      Monk seal surveys allow park biologists to respond to seal emergencies, such as injuries and hookings from fishing gear.
      Surveys also help biologists understand seal monk generations and family patterns.
      Surveys and photos of monk seals are always conducted under a NOAA Fisheries permit and in partnership with NOAA’s Pacific Islands Fisheries Science Center.")})
  })

  
  ### GENEALOGY TEST###
  # collapsible tree
  
  puptree <- reactive(gene_df_new[gene_df_new$mom==input$selectedmom,
                                  c("mom", "pup", "grandpup", "greatgrandpup")])
  
  output$tree <- renderCollapsibleTree(
    collapsibleTree(
      puptree(),
      root = input$selectedmom,
      hierarchy = c("mom","pup", "grandpup", "greatgrandpup"),
      fill = "cornflowerblue",
      zoomable = FALSE
    )
  )
  
  
  
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
    seal_third_widget <- 
      seal_obs %>%
      filter(sex %in% input$selectsex,
             beach_location_name_from_standardized_list %in% input$selectlocation,
             size %in% input$selectsize)
  })
  
  output$seal_obs_plot <- renderPlot({
    ggplot(data = seal_obs_reactive(), aes(x = beach_location_name_from_standardized_list, fill = size)) +
      geom_bar(width = 0.5) +
      scale_fill_manual(values = c("deepskyblue4","cornflowerblue","deepskyblue1",'darkslategray3',"cadetblue1","honeydew2"), drop = FALSE) +
      labs(x = "Beach",
           y = "Counts") +
      theme_minimal() #change to render plot #fix the ggplot your error is here
  })
  
  
  ### FOURTH TAB ###
  data_table_reactive <- reactive({
   message("I am in data_table_reactive and I seem to be working")
    seal_fifth_widget <- pup_table_data %>%
     filter(mother_tag_name %in% input$pick_mom)
 })
  
  #output$pick_mom <- renderTable(pup_table_data)
  output$pick_mom <- renderDataTable({
   datatable(data_table_reactive(), colnames = c('Mother Tag Name', 'Pup Tag Name(s)', 'Expected Pupping Date', 'Pup First Observed'),
             rownames = FALSE,
             options = list(
     initComplete = JS(
       "function(settings, json) {",
       "$(this.api().table().header()).css({'background-color': 'cornflowerblue', 'color': 'white'});",
       "}"))) %>%
      formatStyle(colnames(data_table_reactive()), color = "slategray")
      
 })
    
  
  
} # end server

# Combine them into an app:
shinyApp(ui = ui, server = server)