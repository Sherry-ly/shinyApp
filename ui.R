library(shiny)

# Define UI for application that draws 9 plots
shinyUI(fluidPage(

    # Application title
    titlePanel("Plots of the Market Information / Noise Structures"),


        # Show a plot of the generated distribution
        mainPanel(
            h3(textOutput("caption"))
        )
    )
))
