library(shiny)
library(plotly)
library(reshape2)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(grid)
library(TTR)
library(forecast)

#Data Input
h_r <- read.csv(url("https://raw.githubusercontent.com/jliao12/hiny-Application-and-Reproducible-Pitch-Project/master/original_data/harden_regular.csv"))
h_p <- read.csv(url("https://raw.githubusercontent.com/jliao12/hiny-Application-and-Reproducible-Pitch-Project/master/original_data/harden_playoff.csv"))
season_list <- h_r[,"Season"]
row.names(h_r) <- season_list
row.names(h_p) <- season_list
h_r <- mutate(h_r,year = seq(2009,2020,1))
h_p <- mutate(h_p,year = seq(2009,2020,1))
h_r$FGP <- h_r$FGP*100
h_p$FGP <- h_p$FGP*100
h_r_s <- h_r[,c("G","PTS","FGP","AST","STL","TRB","year")]
h_p_s <- h_p[,c("G","PTS","FGP","AST","STL","TRB","year")]
h_r_s_m <- melt(h_r_s,id.vars = "year",variable.name = "stat",value.name = "value")
h_p_s_m <- melt(h_p_s,id.vars = "year",variable.name = "stat",value.name = "value")
h_r_s_m <- mutate(h_r_s_m,cato = rep("Regular",72))
h_p_s_m <- mutate(h_p_s_m,cato = rep("Playoff",72))
mix <- rbind(h_r_s_m,h_p_s_m)
la <- c("2010-11","2012-13","2014-15","2016-17","2018-19","2020-21")

p <- forecast:::forecast.HoltWinters(HoltWinters(ts(h_r$PTS,start = 2009), 
                beta=FALSE, gamma=FALSE),h=5,level = 0.95)
ft <- data.frame("low95" = p$lower, "up95" = p$upper)
colnames(ft) <-c("low95","up95")
rownames(ft) <-c("2021-22","2022-23","2023-24","2024-25","2025-26")

ui <- fluidPage(

    titlePanel(title=div(img(src="clipart2569860.png",height='80px',width='60px'),
                         "James Harden Career Stat Overview",style = "font-family: Lucida Handwriting, Cursive;
           font-weight: bold;")),
    sidebarPanel(
        selectInput("Season","Choose a season:",
                    season_list),
        selectInput("Regular_off", "Regular or Playoffs", 
                    c("Regular", "Playoffs")), 
        awesomeCheckboxGroup("stat", "Statistics",
                             c("Game Played (G)" = "G",
                               "Point Per Game (PTS)" = "PTS",
                               "Field Goal Pecentage (FGP)" = "FGP",
                               "Assist Per Game (AST)" = "AST",
                               "Rebounds Per Game (TRB)" = "TRB",
                               "Steals Per Game (STL)" = "STL")),
        sliderInput("year", "Number of years to Forecast (Forecast Tab):",
                    min = 1, max = 5,
                    value = 3)
    ),
    mainPanel(
        tabsetPanel(type = "tabs",
            tabPanel("Overview",
                     h3(textOutput("oid1"),
                        style = "font-family: Lucida Handwriting, Cursive;
           font-weight: bold;"), 
                     tableOutput("data"),
                     plotlyOutput("plot1")),
            tabPanel("PTS Forecast", plotOutput("plot2"),
                     tableOutput("fore"))
        
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output,session) {

    output$oid1 <- renderText({
        paste("Season: ",input$Season," ",input$Regular_off)}) 
    output$data <- renderTable({
        if(input$Regular_off == "Playoffs")
            h_p[input$Season,c("Season",input$stat),drop=F]
        else 
            h_r[input$Season,c("Season",input$stat),drop=F]
    })
    output$plot1 <- renderPlotly({
        if(length(input$stat)>0)
            ggplotly(ggplot(filter(mix,stat %in% input$stat),
                            aes(x = year, y = value, colour = cato)) +
                         geom_vline(xintercept = h_r[input$Season,"year"],
                                    linetype='dashed', color='yellow') +
                         geom_point(size = 1,shape = 1) + 
                         geom_smooth() +
                         facet_wrap(~stat,scales = "free") + 
                         theme(panel.spacing.x=unit(0.5, "lines"),
                               panel.spacing.y=unit(2, "lines"),
                               axis.ticks = element_blank(),
                               axis.text.x = element_text(angle = 30,size = 6),
                               axis.text.y =  element_text(size = 6)) +
                         scale_x_continuous(breaks = c(2010,2012,2014,2016,2018,2020),
                                            labels= la) + 
                         xlab(NULL)
                     
            ) %>% layout(legend = list(orientation = "h", x = 0.35, y = -0.2))
    })
    output$plot2 <- renderPlot({
        plot(forecast:::forecast.HoltWinters(
            HoltWinters(ts(h_r$PTS,start = 2009),
                             beta=FALSE, gamma=FALSE),h =input$year),
            main = "PTS Forecasts", ylab = "PTS", xlab = "year")
    })
    output$fore <- renderTable({
        ft[c(1:input$year),]
    },rownames = T)
}

# Run the application 
shinyApp(ui = ui, server = server)
