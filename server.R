library(shiny)

shinyServer(
    function(input, output,session) {
        output$oid1 <- renderPrint({input$Season}) 
        output$oid2 <- renderPrint({input$Regular_off})
        output$oid3 <- renderPrint({input$stat}) 
        output$data <- renderTable({
            if(input$Regular_off == "Playoffs")
            h_playoff[input$Season,c("Season",input$stat),drop=F]
            else 
                h_regular[input$Season,c("Season",input$stat),drop=F]
        })
        output$plot1 <- renderPlotly({
            if(length(input$stat)>0)
                ggplotly(ggplot(filter(mix,stat %in% input$stat),
                        aes(x = year, y = value, colour = cato)) +
                        geom_point() + 
                        facet_wrap(~stat,scales = "free")
                
            )
        })
    }
)
