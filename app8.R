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
                 selectInput("setting", h3("Choose your Setting"),
                             choices = c("Virtual",
                                         "In-person",
                                         "No Preference"),
                             selected = "No Preference"),
               )
               ),
               column(4, wellPanel(
                 # This outputs the dynamic UI component
                 uiOutput("ui")
               )),
                      
               column(4, wellPanel(
                 # This outputs the dynamic UI component
                 uiOutput("ui_next")
               )),   
                
               column(4, wellPanel(
                 # This outputs the dynamic UI component
                 uiOutput("ui_three")
               )),                               
              
               column(4, wellPanel(
                 # This outputs the dynamic UI component
                 uiOutput("ui_four")
               )),                             
                                  
               mainPanel(h5("Use the map below to explore the potential actions near you! Use this
                   text for addition explanations"),
                         leafletOutput("actionsmap"))
             )
    )
  )
)


server <- function(input, output){
  
    output$ui <- renderUI({
      if (is.null(input$setting))
        return()
     
      switch(input$setting,
        "Virtual" = selectInput("level", h3("Choose your Preferred Action"),
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
                                selected = "Social Media"),
        
      "In-person"=  selectInput("level", h3("Choose your Level"),
                    choices = c("Local",
                                "State",
                                "Regional",
                                "National"),
                    selected = "Local"),
      "No Preference" = selectInput("level", h3("Choose your Level"),
                                    choices = c("Local",
                                                "State",
                                                "Regional",
                                                "National"),
                                    selected = "Local"),
      )
  
  
    })
    
    output$ui_next <- renderUI({
      
      if (is.null(input$level))
        return()
      # Depending on input$input_type, we'll generate a different
      # UI component and send it to the client.
      switch(input$level,
              "Social Media" =  selectInput("affinity", h3("Choose a Co-dependent Issue"),
                                            choices = c("No Other Affiliations",
                                                        "Climate Justice",
                                                        "Indigenous Justice",
                                                        "Racial Justice",
                                                        "Students",
                                                        "Groups for Young People",
                                                        "Women",
                                                        "Other"),
                                            selected = "No Other Affiliations"),
        "Protesting" =  selectInput("affinity", h3("Choose a Co-dependent Issue"),
                                    choices = c("No Other Affiliations",
                                                "Climate Justice",
                                                "Indigenous Justice",
                                                "Racial Justice",
                                                "Students",
                                                "Groups for Young People",
                                                "Women",
                                                "Other"),
                                    selected = "No Other Affiliations"),
           "Data Entry" =  selectInput("affinity", h3("Choose a Co-dependent Issue"),
                                       choices = c("No Other Affiliations",
                                                   "Climate Justice",
                                                   "Indigenous Justice",
                                                   "Racial Justice",
                                                   "Students",
                                                   "Groups for Young People",
                                                   "Women",
                                                   "Other"),
                                       selected = "No Other Affiliations"),
                 "Sign a Petition" =  selectInput("affinity", h3("Choose a Co-dependent Issue"),
                                                  choices = c("No Other Affiliations",
                                                              "Climate Justice",
                                                              "Indigenous Justice",
                                                              "Racial Justice",
                                                              "Students",
                                                              "Groups for Young People",
                                                              "Women",
                                                              "Other"),
                                                  selected = "No Other Affiliations"),
               "Calls to Citizens" =  selectInput("affinity", h3("Choose a Co-dependent Issue"),
                                                  choices = c("No Other Affiliations",
                                                              "Climate Justice",
                                                              "Indigenous Justice",
                                                              "Racial Justice",
                                                              "Students",
                                                              "Groups for Young People",
                                                              "Women",
                                                              "Other"),
                                                  selected = "No Other Affiliations"),
               "General Assistance" =  selectInput("affinity", h3("Choose a Co-dependent Issue"),
                                                   choices = c("No Other Affiliations",
                                                               "Climate Justice",
                                                               "Indigenous Justice",
                                                               "Racial Justice",
                                                               "Students",
                                                               "Groups for Young People",
                                                               "Women",
                                                               "Other"),
                                                   selected = "No Other Affiliations"),
               "Meetings" =  selectInput("affinity", h3("Choose a Co-dependent Issue"),
                                         choices = c("No Other Affiliations",
                                                     "Climate Justice",
                                                     "Indigenous Justice",
                                                     "Racial Justice",
                                                     "Students",
                                                     "Groups for Young People",
                                                     "Women",
                                                     "Other"),
                                         selected = "No Other Affiliations"),
                "Contacting Decision Makers" =  selectInput("affinity", h3("Choose a Co-dependent Issue"),
                                                            choices = c("No Other Affiliations",
                                                                        "Climate Justice",
                                                                        "Indigenous Justice",
                                                                        "Racial Justice",
                                                                        "Students",
                                                                        "Groups for Young People",
                                                                        "Women",
                                                                        "Other"),
                                                            selected = "No Other Affiliations"),
                 "Tabling Events" =  selectInput("affinity", h3("Choose a Co-dependent Issue"),
                                                 choices = c("No Other Affiliations",
                                                             "Climate Justice",
                                                             "Indigenous Justice",
                                                             "Racial Justice",
                                                             "Students",
                                                             "Groups for Young People",
                                                             "Women",
                                                             "Other"),
                                                 selected = "No Other Affiliations"),
               "Canvassing" =  selectInput("affinity", h3("Choose a Co-dependent Issue"),
                                           choices = c("No Other Affiliations",
                                                       "Climate Justice",
                                                       "Indigenous Justice",
                                                       "Racial Justice",
                                                       "Students",
                                                       "Groups for Young People",
                                                       "Women",
                                                       "Other"),
                                           selected = "No Other Affiliations"),
                  "Other" =  selectInput("affinity", h3("Choose a Co-dependent Issue"),
                                         choices = c("No Other Affiliations",
                                                     "Climate Justice",
                                                     "Indigenous Justice",
                                                     "Racial Justice",
                                                     "Students",
                                                     "Groups for Young People",
                                                     "Women",
                                                     "Other"),
                                         selected = "No Other Affiliations"),
             
             "Local"=  selectInput("affinity", h3("Choose your State"),
                                   choices = c("Montana",
                                               "New Hampshire",
                                               "Missouri",
                                               "California"),
                                   selected = "Montana"),
        
        
        
             "State" = selectInput("affinity", h3("Choose your State"),
                                   choices = c("Montana",
                                               "New Hampshire",
                                               "Missouri",
                                               "California"),
                                   selected = "Montana"),
           "Regional" = selectInput("affinity", h3("Choose a Region"),
                                    choices = c("Northeast", 
                                                "Southeast",
                                                "Midwest", 
                                                "Northern Great Plains",
                                                "Southern Great Plains", 
                                                "Northwest",
                                                "Southwest",
                                                "Alaska",
                                                "Hawaii & Pacific Islands",
                                                "Caribbean"),
                                    selected = "Northeast"),
             "National" =  selectInput("affinity", h3("Choose your Preferred Action"),
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
                                       selected = "Social Media")
                                          
      )
      
      
    })
    
    output$ui_three <- renderUI({
      
      if (input$setting == "Virtual")
        return()
     
      switch(input$affinity,
            
             "Montana" = selectInput("action", h3("Choose your Preferred Action"),
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
                                     selected = "Social Media"),
                "New Hampshire" = selectInput("action", h3("Choose your Preferred Action"),
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
                                              selected = "Social Media"),
                  "Missouri" = selectInput("action", h3("Choose your Preferred Action"),
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
                                           selected = "Social Media"),
               "California" = selectInput("action", h3("Choose your Preferred Action"),
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
                                        selected = "Social Media"),
             
           "Northeast" = selectInput("action", h3("Choose your Preferred Action"),
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
                                     selected = "Social Media"), 
            "Southeast" = selectInput("action", h3("Choose your Preferred Action"),
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
                                      selected = "Social Media"),
            "Midwest" = selectInput("action", h3("Choose your Preferred Action"),
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
                                    selected = "Social Media"), 
            "Northern Great Plains" = selectInput("action", h3("Choose your Preferred Action"),
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
                                                  selected = "Social Media"),
         "Southern Great Plains" = selectInput("action", h3("Choose your Preferred Action"),
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
                                               selected = "Social Media"), 
        "Northwest" = selectInput("action", h3("Choose your Preferred Action"),
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
                                selected = "Social Media"),
          "Southwest" = selectInput("action", h3("Choose your Preferred Action"),
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
                                  selected = "Social Media"),
      "Alaska" = selectInput("action", h3("Choose your Preferred Action"),
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
                             selected = "Social Media"),
  "Hawaii & Pacific Islands" = selectInput("action", h3("Choose your Preferred Action"),
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
                                           selected = "Social Media"),
       "Caribbean" = selectInput("action", h3("Choose your Preferred Action"),
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
                                 selected = "Social Media"),
                                      
    "Social Media" =  selectInput("action", h3("Choose a Co-dependent Issue"),
                                  choices = c("No Other Affiliations",
                                              "Climate Justice",
                                              "Indigenous Justice",
                                              "Racial Justice",
                                              "Students",
                                              "Groups for Young People",
                                              "Women",
                                              "Other"),
                                  selected = "No Other Affiliations"),
    "Protesting" =  selectInput("action", h3("Choose a Co-dependent Issue"),
                                choices = c("No Other Affiliations",
                                            "Climate Justice",
                                            "Indigenous Justice",
                                            "Racial Justice",
                                            "Students",
                                            "Groups for Young People",
                                            "Women",
                                            "Other"),
                                selected = "No Other Affiliations"),
    "Data Entry" =  selectInput("action", h3("Choose a Co-dependent Issue"),
                                choices = c("No Other Affiliations",
                                            "Climate Justice",
                                            "Indigenous Justice",
                                            "Racial Justice",
                                            "Students",
                                            "Groups for Young People",
                                            "Women",
                                            "Other"),
                                selected = "No Other Affiliations"),
    "Sign a Petition" =  selectInput("action", h3("Choose a Co-dependent Issue"),
                                     choices = c("No Other Affiliations",
                                                 "Climate Justice",
                                                 "Indigenous Justice",
                                                 "Racial Justice",
                                                 "Students",
                                                 "Groups for Young People",
                                                 "Women",
                                                 "Other"),
                                     selected = "No Other Affiliations"),
    "Calls to Citizens" =  selectInput("action", h3("Choose a Co-dependent Issue"),
                                       choices = c("No Other Affiliations",
                                                   "Climate Justice",
                                                   "Indigenous Justice",
                                                   "Racial Justice",
                                                   "Students",
                                                   "Groups for Young People",
                                                   "Women",
                                                   "Other"),
                                       selected = "No Other Affiliations"),
    "General Assistance" =  selectInput("action", h3("Choose a Co-dependent Issue"),
                                        choices = c("No Other Affiliations",
                                                    "Climate Justice",
                                                    "Indigenous Justice",
                                                    "Racial Justice",
                                                    "Students",
                                                    "Groups for Young People",
                                                    "Women",
                                                    "Other"),
                                        selected = "No Other Affiliations"),
    "Meetings" =  selectInput("action", h3("Choose a Co-dependent Issue"),
                              choices = c("No Other Affiliations",
                                          "Climate Justice",
                                          "Indigenous Justice",
                                          "Racial Justice",
                                          "Students",
                                          "Groups for Young People",
                                          "Women",
                                          "Other"),
                              selected = "No Other Affiliations"),
    "Contacting Decision Makers" =  selectInput("affinity", h3("Choose a Co-dependent Issue"),
                                                choices = c("No Other Affiliations",
                                                            "Climate Justice",
                                                            "Indigenous Justice",
                                                            "Racial Justice",
                                                            "Students",
                                                            "Groups for Young People",
                                                            "Women",
                                                            "Other"),
                                                selected = "No Other Affiliations"),
    "Tabling Events" =  selectInput("action", h3("Choose a Co-dependent Issue"),
                                    choices = c("No Other Affiliations",
                                                "Climate Justice",
                                                "Indigenous Justice",
                                                "Racial Justice",
                                                "Students",
                                                "Groups for Young People",
                                                "Women",
                                                "Other"),
                                    selected = "No Other Affiliations"),
    "Canvassing" =  selectInput("action", h3("Choose a Co-dependent Issue"),
                                choices = c("No Other Affiliations",
                                            "Climate Justice",
                                            "Indigenous Justice",
                                            "Racial Justice",
                                            "Students",
                                            "Groups for Young People",
                                            "Women",
                                            "Other"),
                                selected = "No Other Affiliations"),
    "Other" =  selectInput("action", h3("Choose a Co-dependent Issue"),
                           choices = c("No Other Affiliations",
                                       "Climate Justice",
                                       "Indigenous Justice",
                                       "Racial Justice",
                                       "Students",
                                       "Groups for Young People",
                                       "Women",
                                       "Other"),
                           selected = "No Other Affiliations"),
             
      )
      
      
    })
    
    output$ui_four <- renderUI({
      
      if (input$setting == "Virtual")
        return()
     
      switch(input$action,
             
             "Social Media" =  selectInput("dependent", h3("Choose a Co-dependent Issue"),
                                           choices = c("No Other Affiliations",
                                                       "Climate Justice",
                                                       "Indigenous Justice",
                                                       "Racial Justice",
                                                       "Students",
                                                       "Groups for Young People",
                                                       "Women",
                                                       "Other"),
                                           selected = "No Other Affiliations"),
             "Protesting" =  selectInput("dependent", h3("Choose a Co-dependent Issue"),
                                         choices = c("No Other Affiliations",
                                                     "Climate Justice",
                                                     "Indigenous Justice",
                                                     "Racial Justice",
                                                     "Students",
                                                     "Groups for Young People",
                                                     "Women",
                                                     "Other"),
                                         selected = "No Other Affiliations"),
             "Data Entry" =  selectInput("dependent", h3("Choose a Co-dependent Issue"),
                                         choices = c("No Other Affiliations",
                                                     "Climate Justice",
                                                     "Indigenous Justice",
                                                     "Racial Justice",
                                                     "Students",
                                                     "Groups for Young People",
                                                     "Women",
                                                     "Other"),
                                         selected = "No Other Affiliations"),
             "Sign a Petition" =  selectInput("dependent", h3("Choose a Co-dependent Issue"),
                                              choices = c("No Other Affiliations",
                                                          "Climate Justice",
                                                          "Indigenous Justice",
                                                          "Racial Justice",
                                                          "Students",
                                                          "Groups for Young People",
                                                          "Women",
                                                          "Other"),
                                              selected = "No Other Affiliations"),
             "Calls to Citizens" =  selectInput("dependent", h3("Choose a Co-dependent Issue"),
                                                choices = c("No Other Affiliations",
                                                            "Climate Justice",
                                                            "Indigenous Justice",
                                                            "Racial Justice",
                                                            "Students",
                                                            "Groups for Young People",
                                                            "Women",
                                                            "Other"),
                                                selected = "No Other Affiliations"),
             "General Assistance" =  selectInput("dependent", h3("Choose a Co-dependent Issue"),
                                                 choices = c("No Other Affiliations",
                                                             "Climate Justice",
                                                             "Indigenous Justice",
                                                             "Racial Justice",
                                                             "Students",
                                                             "Groups for Young People",
                                                             "Women",
                                                             "Other"),
                                                 selected = "No Other Affiliations"),
             "Meetings" =  selectInput("dependent", h3("Choose a Co-dependent Issue"),
                                       choices = c("No Other Affiliations",
                                                   "Climate Justice",
                                                   "Indigenous Justice",
                                                   "Racial Justice",
                                                   "Students",
                                                   "Groups for Young People",
                                                   "Women",
                                                   "Other"),
                                       selected = "No Other Affiliations"),
             "Contacting Decision Makers" =  selectInput("dependent", h3("Choose a Co-dependent Issue"),
                                                         choices = c("No Other Affiliations",
                                                                     "Climate Justice",
                                                                     "Indigenous Justice",
                                                                     "Racial Justice",
                                                                     "Students",
                                                                     "Groups for Young People",
                                                                     "Women",
                                                                     "Other"),
                                                         selected = "No Other Affiliations"),
             "Tabling Events" =  selectInput("dependent", h3("Choose a Co-dependent Issue"),
                                             choices = c("No Other Affiliations",
                                                         "Climate Justice",
                                                         "Indigenous Justice",
                                                         "Racial Justice",
                                                         "Students",
                                                         "Groups for Young People",
                                                         "Women",
                                                         "Other"),
                                             selected = "No Other Affiliations"),
             "Canvassing" =  selectInput("dependent", h3("Choose a Co-dependent Issue"),
                                         choices = c("No Other Affiliations",
                                                     "Climate Justice",
                                                     "Indigenous Justice",
                                                     "Racial Justice",
                                                     "Students",
                                                     "Groups for Young People",
                                                     "Women",
                                                     "Other"),
                                         selected = "No Other Affiliations"),
             "Other" =  selectInput("dependent", h3("Choose a Co-dependent Issue"),
                                    choices = c("No Other Affiliations",
                                                "Climate Justice",
                                                "Indigenous Justice",
                                                "Racial Justice",
                                                "Students",
                                                "Groups for Young People",
                                                "Women",
                                                "Other"),
                                    selected = "No Other Affiliations"),
             
      )
      
      
    })
    
  
  
  output$actionsmap<-renderLeaflet({
    
    if(input$setting == "Virtual"){
      
      selectedData<-reactive({
       
        Sample %>% filter(Setting %in% input$setting & Needs %in% input$level & 
                          Affinity %in% input$affinity)
      })
        
      
    }
    
    else{

      selectedData<-reactive({
      
      Sample %>% filter(Setting %in% input$setting & Level %in% input$level &
                        State %in% input$affinity & Action %in% input$action &
                        Affinity %in% input$dependent)
      })

    }
      labels<-sprintf( "%s",selectedData()$Group) %>% lapply(htmltools::HTML)
     
       leaflet() %>% addTiles() %>%
        addMarkers(data = selectedData(),
                   label = labels,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"))
      observe({
        leafletProxy("actionsmap", data = selectedData())
        
    
      })
      
    

    
  })


 
    

  

}

shinyApp(ui = ui, server = server)
