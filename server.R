
library(shiny)
library(gsheet)
library(tidyverse)

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
                scores
                ##scores$arch_index <- array(1:length(Archer_scores$`END SCORE`))
        })

        ## Output plot to be rendered with parameters
        output$scoreplot <- renderPlot({
                name <- input$archer_name
                ggplot(Archer_scores(), aes(`Date (MM/DD/YY)`,`END SCORE`)) +   ## DATE, END_SCORE
                        geom_point(shape = 16, size = 3, color = "salmon") +
                        geom_text(label = Archer_scores()$`END SCORE`,
                                  vjust = -1, nudge_y = 5, size = 4) +          ## END_SCORE
                        stat_smooth(method = 'lm') +
                        ggtitle(label = name) +
                        xlab(label = "Date") +
                        ylab(label = "Score") +
                        ylim(0,350) +
                        theme(aspect.ratio = 3/4)
        })

        output$dataframe1 <- renderTable({
                SSC <- length(Archer_scores()$`ARCHER (FIRSTNAME_LASTNAME)`)    ## ARCHER_ID
                AAS <- (sum(Archer_scores()$`END SCORE`))/(SSC * 30)            ## END_SCORE

                temp <- lm(index ~ `END SCORE`, Archer_scores())    ## END_SCORE, ARCHER_ID
                IPW <- temp$coefficients[2]

                ##data table to be rendered
                data.frame("Scoring_Sessions_Completed" = c(SSC),
                          "Average_Arrow_Score" = c(AAS),
                          "Improvement_Per_Week" = c(IPW))
        })
})
