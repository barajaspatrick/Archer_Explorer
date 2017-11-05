
library(shiny)
library(gsheet)
library(tidyverse)

## Variables analyzed so far:
## ARCHER_ID, Date, End_Score

## function for reading a google sheet doc and saving it in the global enviroment.
gURL <- "https://docs.google.com/spreadsheets/d/16yHM6ldzaxMbLmHTuWU0w0LQJo7S_BwqJ-qhWotjz9g/edit?usp=sharing"
read_sheet <- function(URL){gsheet2tbl(URL)}
scores <- read_sheet(gURL)

shinyServer(function(input, output){

        ## Reactive function for filtering the 'scores' data set based on selected archer
        ## and adding a index column for later use
        Archer_scores <- reactive({
                scores$`Date (MM/DD/YY)` <- as.Date(scores$`Date (MM/DD/YY)`, format='%m/%d/%y') ## DATE
                scores <- filter(scores, `ARCHER (FIRSTNAME_LASTNAME)` == input$archer_name) ## ARCHER_ID
                scores$arch_index <- as.numeric(array(1:length(scores$`END SCORE`)))
                scores
        })

        ## Output plot to be rendered with parameters
        output$scoreplot <- renderPlot({
                name <- input$archer_name
                ggplot(Archer_scores(), aes(`Date (MM/DD/YY)`,`END SCORE`)) +   ## DATE, END_SCORE
                        ## stat_smooth(method = 'loess', color = "coral", size = 1.5) +
                        stat_smooth(method = "lm", color = "royalblue1", size = 1.3) +
                        geom_point(shape = 16, size = 4, color = "coral") +
                        geom_text(label = Archer_scores()$`END SCORE`,  ## END_SCORE
                                  vjust = -1, nudge_y = 5, size = 4) +
                        labs(title =  name, x = "Date", y = "Score") +
                        theme_minimal() +
                        theme(plot.title = element_text(color="black", face="bold", size=24, hjust=0)) +
                        theme(axis.title = element_text(color="black", size=16)) +
                        ylim(0,350) + theme(aspect.ratio = 3/4)
        })

        output$dataframe1 <- renderTable({
                ## Calculate the number of sessions the archer has completed
                SSC <- length(Archer_scores()$`ARCHER (FIRSTNAME_LASTNAME)`)    ## ARCHER_ID
                ## Calulate the average arrow score of all the arrows shot during scoring practice
                AAS <- (sum(Archer_scores()$`END SCORE`))/(SSC * 30)            ## END_SCORE
                ## Create linear model based off of practice end score and number of scoring sessions.
                temp <- lm(`END SCORE` ~ arch_index, Archer_scores())    ## END_SCORE, ARCHER_ID
                IPW <- temp$coefficients[2]

                ##data table to be rendered displaying all the states above
                data.frame("Scoring_Sessions_Completed" = c(SSC),
                          "Average_Arrow_Score" = c(AAS),
                          "Progress_Per_Week" = c(IPW))
        })
})

## Possible future improvements:
## calculate average arrow score per session
## additional stats?
