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
                                  "Explore our Hawaiian Monk Seal app starting with the panel to the left.",
                                  br(),
                                  br(),
                                  fluidRow(
                                    column(4,
                                           HTML("<center><img src='kala_2.jpeg' alt='Two seals on the beach' style='height: 200px; width:250px;'/></center>")     
                                    ),
                                    column(4,
                                           HTML("<center><img src='kala_9.jpeg' alt='Seal on a beach with rainbow' style='height: 200px; width:250px;'/></center>")     
                                    ),
                                    column(4,
                                           HTML("<center><img src='kala_3.jpeg' alt='Mom and pup yawning' style='height: 200px; width:250px;'/></center>")     
                                    )),
                                  #HTML('<center><img src="kala_2.jpeg" alt="Two seals on the beach" style="height: 200px; width:250px;"/></center>'),
                                  br(),
                                  htmlOutput("value"),
                                  br(),
                                  br(),
                                  br()
                        ) #end main panel
                        ) # end sidebar layout
                      ) # end tab 
             , 
###END FIRST TAB###











### SECOND TAB - GENEALOGY ###
tabPanel("Genealogy Tree", fluid = TRUE, icon = icon("tree"),
         mainPanel(h1("Generations of Seals at Kalaupapa National Historical Park"),
           includeMarkdown("www/genealogy.Rmd"),
           ##trying gene stuff
           selectInput(inputId = "selectedmom",
                       label = "Select a Mom",
                       choices = unique(gene_df_new$mom)),
           collapsibleTreeOutput('tree', height='700px'),
           br(),
           br(),
           br(),
           br()
         ) # end mainpanel
), # end tab
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
                     leafletOutput(outputId = "beach_map"),
                     br(),
                     br(),
                     br(),
                     br()
           ) # end mainpanel
         ) # end sidebar layout
), # end tab
### END THIRD TAB ###







### FOURTH TAB - SEAL CHARACTERISTICS ###
tabPanel("Observed Seal Characteristics", fluid = TRUE, icon= icon("binoculars"),
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
             plotOutput(outputId = "seal_obs_plot"),
             br(),
             br(),
             br(),
             br()
           )# end main panel panel
         ) # end sidebar layout
), # end tab 
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
           mainPanel(h1("Pup info!"),
                        includeMarkdown("pupinfo.Rmd"),
                     DT::dataTableOutput("pick_mom"),
                     br(),
                     br(),
                     br(),
                     br()
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
   output$value <- renderText({HTML("<center><img src='kala_6.jpeg' alt='Mother and offspring monk seal on the beach' style='height: 200px; width:250px'>
                                    <br>
                                    <br>
                                    <font size='+2'>About This App</font>
                                    <br>
                                    <br>
                                    <em>Hawaiian Monk Seals at Kalaupapa National Historical Park</em> is an app that lets you visualize <a href='https://www.nps.gov/kala/learn/nature/monkseal.htm'>Kalaupapa National Historic Park's</a> monk seal survey data.
     From the top navigation panel you'll be able to access a seal genealogy tree, observed seal locations, graphs displaying seal observation data, as well as mother and pup characteristics.
     Additionally, using the side panel to the left, you can dive into the life history of this special marine species.
     <br>
     <br>
     <font size='+2'>Creators and Data Source</font>
     <br>
     <br>
     This app was created by Katherine Rosencrance, Annie Combs, and Alessandra Puig-Santana for the course ESM 244 - Advanced Data Analysis at the University of Santa Barbara.
     Data were obtained from Glauco Puig-Santana, a biological technician working at Kalaupapa National Historical Park.
     The data are not publicly available, but the original data files contained seal and pup observations dating back to 2021 and genealogy data beginning in 1997.
     The side panel tab 'NPS Surveys' details the data collection methods used for the data included in this app. Enjoy!
     <br>
     <br>
     <br>
     <br>
     <br>
     <br>
     <br>
     <br>
     <b>Citations:</b> National Park Service. (2022, December 22). Hawaiian Monk Seal. https://www.nps.gov/kala/learn/nature/monkseal.htm <br>
       <br>
       NOAA Fisheries. (2022, September 15). Hawaiian Monk Seal. https://www.fisheries.noaa.gov/species/hawaiian-monk-seal")
   })
 })
  
  observeEvent(input$lifecycle, {
    output$value <- renderText({HTML("<center><img src='kala_1.jpeg' alt='Mother and offspring monk seal on the beach' style='height: 200px; width:250px'>
                                     <br>
                                     <br>
                                     <font size='+2'>Seal Life Cycle</font> 
                                     <br> 
                                     <br>
                                     Within five to seven weeks of birth, monk seal pups can rapidly gain weight, growing from 25 pounds to 200 pounds.
                                     During this period, they also shed their black fur and replace it with a grey coat which will eventually fade to a brown shade when they molt again the following year.
                                     Monk seals spend two-thirds of their lives at sea, and they are capable of holding their breath for up to 20 minutes and diving as deep as 1,800 feet; however, the average dive tends to be much shallower.
                                     They may travel far and wide throughout the Hawaiian archipelago without migrating.
                                     <br>
                                     <br>
                                     Female monk seals typically reach reproductive age between 5-10 years and often return to the same beach where they were born to give birth to a single pup every one to two years.
                                     The beaches of Kalaupapa are ideal for monk seal pupping, as they offer shallow, protected waters, minimal human disruption, and plenty of food in the nearby reef.
                                     Monk seals average around 7 – 7.5 feet long and can weigh an average of 375-450 pounds.
                                     The Hawaiian monk seal is endangered, having been hunted almost to extinction in the 19th century, with only about 1,200 individuals at last count in 2016 in their native habitats in the Hawaiian Islands.")})})

  observeEvent(input$surveys, {
    output$value <- renderText({HTML("<center><img src='kala_7.jpeg' alt='Seal with bleach marking on back' style='height: 200px; width:250px'>
                                     <br>
                                     <br>
                                     <font size='+2'>NPS Surveys</font>
                                     <br>
                                     <br>
                                     Kalaupapa National Historical Park employees help to conserve the Hawaiian Monk Seal population.
                                     Throughout the year, marine biologists that work for the National Park Service walk the beaches to observe, take pictures of, and monitor the behavior of the seals.
                                     Biologists observe seal pupping to track reproduction rates and grwoth among new members of the population.
                                     To keep track of the seals, the employees look for tags with numbers on their flippers, bleach marks, or identifying scars.
                                     Surveys also help conservationists understand seal monk generations and family patterns.
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