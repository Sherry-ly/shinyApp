library(shiny)

shinyServer(function(input, output) {
    
    formulaText <- reactive({
        LN <- getwd()
    })
    
    
    output$caption <- renderText({
        formulaText()
    })
})
