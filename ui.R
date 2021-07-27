library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
    headerPanel("James Harden Stat Overview"),
    sidebarPanel(
        selectInput("Season","Choose a season:",
                    season_list),
        selectInput("Regular_off", "Regular or Playoffs", 
                    c("Regular", "Playoffs")), 
        checkboxGroupInput("stat", "Statistics",
                          c("Point Per Game" = "PTS", 
                            "Assist Per Game" = "AST",
                            "Rebounds Per Game" = "TRB",
                            "Steals Per Game" = "STL"))
        ),
    mainPanel(
        h4('Season','-'), 
        verbatimTextOutput("oid1"), 
        h4('You entered'), 
        verbatimTextOutput("oid2"), 
        h4('You entered'), 
        verbatimTextOutput("odate"),
        h4('You entered'), 
        verbatimTextOutput("oid3"),
        tableOutput("data"),
        plotlyOutput("plot1")
        )
        )
)