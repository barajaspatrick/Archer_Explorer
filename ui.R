#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(gsheet)

## function for reading a google sheet doc and saving it in the global enviroment.
read_sheet <- function(URL)
{
        gsheet2tbl(URL)
}

gURL <- "https://docs.google.com/spreadsheets/d/16yHM6ldzaxMbLmHTuWU0w0LQJo7S_BwqJ-qhWotjz9g/edit?usp=sharing"
scores <- read_sheet(gURL)
scores <- unique(scores$`ARCHER (FIRSTNAME_LASTNAME)`)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("UCI Archery Team"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
     
         ## sidebar panel for inputs ----     
        sidebarPanel(
        
        ## input: filter Dataset ----    
        selectInput("archer_name", "View Archer:",
                    choices = scores)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("scoreplot"),
       tableOutput("dataframe1"),
       br(),
       p("Progress_per_week is based off of a linear model between the number 
         of scoring sessions completed and the total_scores out of 300 for each of the sessions.")
    )
  )
))
