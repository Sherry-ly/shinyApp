library(shiny)

# Define UI for application that draws 9 plots
shinyUI(fluidPage(

    # Application title
    titlePanel("Plots of the Market Information / Noise Structures"),

    # Sidebar with a text input of ticker and feed source
    sidebarLayout(
        sidebarPanel(
            textInput("feedSource", "feed source: "),
            textInput("ticker", "ticker: ")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3(textOutput("caption")),
            plotOutput("plots")
        )
    )
