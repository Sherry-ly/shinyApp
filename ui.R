library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

        # Show a plot of the generated distribution
        mainPanel(
            h3(textOutput("caption"))
        )
    )
)
