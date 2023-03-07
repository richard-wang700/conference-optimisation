# Made by: Richard Wang
# LinkedIn: linkedin.com/in/richard-wang700
# Github: github.com/richard7w

# Import all the appropriate libraries
library(lutz)
library(shiny)
library(lubridate)
library(rvest)
library(tidyverse)
rm(list=ls())

# Import complete table and format it
complete_table <- read_csv("complete_table.csv") %>% 
  arrange(university)
complete_table$QOL_Index <- as.character(complete_table$QOL_Index)
complete_table$utc <- as.character(complete_table$utc)
default_table <- complete_table

# UI for add
ui <- fluidPage(
  
  # App title
  titlePanel("Conference Optimisation"),
  
  # Side bar of UI
  sidebarLayout(
    
    # Sidebar for inputs
    sidebarPanel(
      #COUNTRY------------------------------------------------------------------
      #Country choice
      selectInput(inputId = "countryCho",
                  label = "Choose a country",
                  choices = complete_table$country),
      #Multiplier for country
      sliderInput(inputId = "countryMulti",
                  label = "Multiplier for country",
                  min = 0,
                  max = 5,
                  value = 1,
                  step = 0.5),
      #-------------------------------------------------------------------------
      #QOL----------------------------------------------------------------------
      # QOL num choice
      numericInput(inputId = "qolNum",
                   label = "Quality of life less than",
                   value = 0),
      #Multiplier on QOL
      sliderInput(inputId = "qolMulti",
                  label = "Multiplier for quality of life",
                  min = 0,
                  max = 5,
                  value = 1,
                  step = 0.5),
      #-------------------------------------------------------------------------
      #UTC----------------------------------------------------------------------
      # UTC num choice
      numericInput(inputId = "utcNum",
                   label = "UTC time (-11 to 12)",
                   value = 0),
      #Multiplier on QOL
      sliderInput(inputId = "utcMulti",
                  label = "Multiplier for UTC",
                  min = 0,
                  max = 5,
                  value = 1,
                  step = 0.5),
      #-------------------------------------------------------------------------
      
      #Check box for showing only last row
      checkboxInput("lastrow", "Show summary", FALSE),
      #Check box for sorting the last row
      checkboxInput("rearrange", "Re-arrange table", FALSE),
      #Button to update table
      actionButton("update", "Update table"),
      #Button to freeze table
      actionButton("freeze", "Freeze table"),
      #Reset table
      actionButton("reset", "Reset table"),
      #Button to download table
      downloadButton("downloadData", "Download table")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: HTML table with requested number of observations
      tableOutput("view")
      
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # finished_table
  finished_table <- complete_table
  
  # Do this when update button is clicked
  observeEvent(input$update, {
    #-------------------------------------------------------------------------
    # Set multipliers for qol
    qol_value <- as.numeric(input$qolNum)
    qol_multi <- as.numeric(input$qolMulti)
    qol_temp <- complete_table %>% 
      filter(as.numeric(QOL_Index) <= qol_value) %>% 
      mutate_if(., is.numeric, ~ . * qol_multi)
    # Attached qol multiplier table to modified table
    qol_t <- complete_table %>% 
      filter(!(as.numeric(QOL_Index) <= qol_value)) %>% 
      rbind(qol_temp)
    #-------------------------------------------------------------------------
    #-------------------------------------------------------------------------
    # Set multipliers for country
    country_value <- toString(input$countryCho)
    country_multi <- as.numeric(input$countryMulti)
    country_temp <- qol_t %>% 
      filter(country == country_value) %>% 
      mutate_if(., is.numeric, ~ . * country_multi)
    # Attached country multiplier table to original table
    country_t <- qol_t %>% 
      filter(!(country == country_value)) %>% 
      rbind(country_temp)
    #-------------------------------------------------------------------------
    #-------------------------------------------------------------------------
    # Set multipliers for utc
    utc_value <- as.numeric(input$utcNum)
    utc_multi <- as.numeric(input$utcMulti)
    utc_temp <- country_t %>% 
      filter(as.numeric(utc) == utc_value) %>% 
      mutate_if(., is.numeric, ~ . * utc_multi)
    # Attached qol multiplier table to modified table
    multiplier_t <- country_t %>% 
      filter(!(as.numeric(utc) == utc_value)) %>% 
      rbind(utc_temp) %>% 
      bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else NA)) %>% 
      arrange(university)
    #-------------------------------------------------------------------------
    
    # multiplier_t as only last row is displayed
    if (input$lastrow == TRUE){
      mod_table <- tail(multiplier_t, 1) %>% 
        select(-university, -country, -QOL_Index, -utc)
    } else {
      mod_table <- multiplier_t
    }
    
    # multiplier_t as only last row is displayed and it is arranged
    if (input$lastrow == TRUE && input$rearrange == TRUE){
      mod_table <- mod_table[mod_table %>% slice(n()) %>% order(decreasing = TRUE)]
    }
    finished_table <<- mod_table
    # displaying the finished table
    output$view <- renderTable({
      finished_table
    })
  })
  
  # Freeze the table so multiple multipliers can be added
  observeEvent(input$freeze, {
    complete_table <<- finished_table
  })

  # Reset table
  observeEvent(input$reset, {
    # Resetting inputs
    updateSliderInput(session,'countryMulti',value = 1)
    updateSliderInput(session,'qolNum',value = 0)
    updateSliderInput(session,'qolMulti',value = 1)
    updateSliderInput(session,'utcNum',value = 0)
    updateSliderInput(session,'utcMulti',value = 1)
    updateCheckboxInput(session, 'lastrow',value=FALSE)
    updateCheckboxInput(session, 'rearrange',value=FALSE)
    # Reset view of table
    output$view <- renderTable({
      default_table
    })
    complete_table <<- default_table
  })
  
  # Download table called optimised timing table
  output$downloadData <- downloadHandler(
    filename = function() {
      "Optimised timing table.csv"
    },
    content = function(file) {
      write.csv(finished_table, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)