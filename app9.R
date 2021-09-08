#Load the required packages to run the app
rm(list = ls())
library(shinydashboard)
library(tidyverse)
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(htmltools)
library(rgdal)
library(leaflet)
library(plotrix)
library(treemapify)
library(plotly)

#Load the csv files
Test<- read_csv("Data/CSVs/Test.csv")
Test$Needs<-as.character(Test$Needs)
Sample<- read_csv("Data/CSVs/Sample.csv")

Sample<- Sample %>% mutate(Needs = recode(Needs, "1" = "Social Media", "2" = "Protesting", "3" = "Data Entry",
                                          "4" = "Sign a Petition", "5" = "Calls to Citizens", "6" = "General Assistance",
                                          "7" = "Meetings", "8" = "Contacting Decision Makers", "9" = "Tabling Events",
                                          "10" = "Canvassing", "11" = "Other")) %>% 
  mutate(Level = recode(Level, "1" = "Local", "2" = "State", "3" = "Regional", "4" = "National"))%>% 
  mutate(Setting = recode(Setting, "1" = "Virtual", "2" = "In-person",
                          "3" = "No Preference", "4" = "No Preference"))%>% 
  mutate(Affinity = recode(Affinity, "1" = "No Other Affiliations",
                           "2" = "Climate Justice", "3" = "Indigenous Justice",
                           "4" = "Racial Justice", "5" = "Students",
                           "6" = "Groups for Young People",
                           "7" = "Women", "8" = "Other"))%>% 
  mutate(Region = recode(Region, "1" = "Northeast", "2" = "Southeast",
                         "3" = "Midwest", "4" = "Northern Great Plains",
                         "5" = "Southern Great Plains", "6" = "Northwest",
                         "7" = "Southwest", "8" = "Alaska",
                         "9" = "Hawaii & Pacific Islands", "10" = "Caribbean"))

Sample<- Sample[c(1,2,3,4,5,6,8,9,10,11,12,13,15)]


ui <- fluidPage(
  
  # App title ----
  titlePanel("On the Level"),
  
  tabsetPanel(
    #Intro tab
    tabPanel("Introduction", fluid = TRUE,
             sidebarPanel(h5("Here is some introduction text on how to use the app."),
                          br(),
                          h5("You can have fancy pictures or text to fill in the space. We can outline
                            how to use the app and maybe have a picture of the team below or
                            state our mission statement or something. Totally up to our imagination.")),
             mainPanel(h4("It's time to get ON THE LEVEL!"),
                       img(src = "Logo.png", height = 400, width = 450),
                       h6("Created by the gang, 2020"),
                       
                       
             )
    ),
    
    
    #Map of Actions
    tabPanel("Mapping the Movement", fluid = TRUE,
             fluidRow(
               column(4,wellPanel(
                 selectInput("setting", 
                                    h3("Setting"), 
                                    choices = c("Virtual", 
                                                   "In person", 
                                                   "No Preference"),
                                    selected = "Virtual"))
               
               ),
              column(4,wellPanel( selectInput("state", h3("Choose your State"),
                                              choices = c("Any",
                                                          "Montana",
                                                          "New Hampshire",
                                                          "Missouri",
                                                          "California",
                                                          "Kansas"),
                                              selected = "Montana",
                                              multiple = TRUE)),
    ),
    
    column(4, wellPanel(
      selectInput("action", h3("Choose your Preferred Action"),
                  choices = c( "Social Media",
                               "Protesting",
                               "Data Entry",
                               "Sign a Petition",
                               "Calls to Citizens",
                               "General Assistance",
                               "Meetings",
                               "Contacting Decision Makers",
                               "Tabling Events",
                               "Canvassing",
                               "Other"),
                  selected = "Social Media",
                  multiple = TRUE))
    ),   
    
    column(4, wellPanel(
      selectInput("dependent", 
                         h3("Co-dependent Issue"), 
                         choices = c("No Other Affiliations", 
                                     "Climate Justice", 
                                     "Indigenous Justice",
                                     "Racial Justice",
                                     "Students",
                                     "Groups for Young People",
                                     "Women",
                                     "Other"),
                         selected = "No Other Affiliations",
                  multiple = TRUE))
    ),                              
                
               
                                          
               
               
    mainPanel(h5("Use the map below to explore the potential actions near you! Use this
                   text for addition explanations"),
      tabsetPanel(
        tabPanel("Map View", leafletOutput("actionsmap")),
        tabPanel("Table View", tableOutput("table"))
    
             )
    )
             )
    )
  )
)
    
  



server<- function(input, output){
  
                    
  output$actionsmap<-renderLeaflet({
    
    selectedData<-reactive({
  
      if(input$state == "Any")
        Sample %>% 
        dplyr::filter(Setting %in% input$setting & Needs %in% input$action
                      & Affinity %in% input$dependent)
      else
        Sample %>% 
        dplyr::filter(Setting %in% input$setting & State %in% input$state & Needs %in% input$action
                      & Affinity %in% input$dependent)
    })
    
    labels<-sprintf( "%s", 
                     selectedData()$Group) %>% lapply(htmltools::HTML)
    
    
    leaflet() %>% addTiles() %>%
      addMarkers(data = selectedData(),
                 label = labels,
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"))
    
  })
  
  output$table<-renderTable({
 
    if(input$state == "Any")
        dplyr::filter(Sample, Setting %in% input$setting, Needs %in% input$action,
                      Affinity %in% input$dependent)
      
      else
        dplyr::filter(Sample, Setting %in% input$setting, State %in% input$state & Needs %in% input$action
                      &  Affinity %in% input$dependent)
      
    
  })
  
}

shinyApp(ui = ui, server = server)
