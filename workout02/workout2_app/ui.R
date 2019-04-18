library(shiny)
library(ggplot2)

fluidPage(
  
  # Application title
  titlePanel("Saving and Investing Scenarios"),
  
  fluidRow(
    column(4,
           sliderInput("initial", "Initial Amount",
                       min = 1,
                       max = 100000,
                       value = 1000, pre = "$", sep = ",")
    ),
    column(4,
           sliderInput("rate", "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5)
    ),
    column(4,
           sliderInput("years", "Years",
                       min = 0, 
                       max = 50, 
                       value = 10)
    )
  ),
  
  
  fluidRow(
    column(4,
           sliderInput("contrib", "Annual Contribution",
                       min = 0,
                       max = 50000,
                       value = 2000, pre = "$", sep = ",")
    ),
    column(4,
           sliderInput("growth", "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2)
    ),
    column(4,
           selectInput("facet", "Facet?",
                       choices = list("No" = FALSE, "Yes" = TRUE))
    )
  ),
  
  
  # Show a plot of the generated distribution
  mainPanel(
    h4("Timelines"),
    plotOutput("plot"),
    h4("Balances"),
    verbatimTextOutput("tbl")
  )
)