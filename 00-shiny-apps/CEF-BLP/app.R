

# Packages ----------------------------------------------------------------

library(ggplot2)
library(shiny)
source("extra-stuff.R")

# Conditional Expectations Function (CEF) ---------------------------------

CEF <- function(x) 10*x^2

### PDF for X
fX <- function(x, a, b, p = -1, q = 1) {
  output <- ((x - p)^(a - 1) * (q - x)^(b - 1)) / 
    ((q - p)^(a + b - 1) * beta(a,b))
  output <- ifelse(x < p | x > q, 0, output)
}


# Best Linear Predictor (BLP) ---------------------------------------------

BLP <- function(a, b) {
  ## Moment function
  E <- function(m) {
    output <- integrate(
      f = function(x) x^m * fX(x, a, b), 
      lower = -Inf, upper = Inf
    )$value
  }
  
  ## Slope
  slope <- function() {
    num <- 10 * (E(3) - E(1) * E(2))
    denom <- E(2) - E(1)^2
    return(num / denom)
  }
  
  ## Intercept
  intercept <- function() {
    10 * E(2) - slope() * E(1)
  }
  return(list(slope = slope(), intercept = intercept()))
}


# Shiny -------------------------------------------------------------------

ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"),
  includeCSS("custom.css"),
  
  h3("Plotting the CEF and the BLP over Different Distributions of X"),
  
  # Sidebar layout with a input and output definitions
  fluidRow(
    
    # Output
    column(width = 8, offset = 0,
           plotOutput(outputId = "CEFapprox"),
           plotOutput(outputId = "distX", height = 150),
           br(), br(), br()),
    
    # Text
    sidebarPanel(
    HTML("This plot replicates Figure 2.2.2. from <i>Foundations of Agnostic Statistics</i> (Aronow & Miller 2019)."),
    br(), br(),
    HTML("Long story short, we must be careful when using the <span style = 'color:steelblue'>Best Linear Predictor (BLP)</span> to approximate a non-linear <span style = 'color:black'>Conditional Expectations Function (CEF)</span>, particularly in those parts of the domain of the CEF where X has a low probability mass."),
    br(), br(),

    # Inputs
    wellPanel(
      # Select lambda
      sliderInput(inputId = "a",
                  label = "a",
                  min = 1, max = 20,
                  value = 10,
                  step = 0.1,
                  ticks = FALSE),
      
      sliderInput(inputId = "b",
                  label = "b",
                  min = 1, max = 20,
                  value = 10,
                  step = 0.1,
                  ticks = FALSE)
      ),
    
    # Text
    HTML("The <strong>a</strong> and <strong>b</strong> parameters correspond to those of a rescaled Beta distribution, with support over [-1, 1].")
    )
  ),
  br()
)

server <- function(input, output) {
  output$distX <- renderPlot({
    ggplot(NULL, aes(x = -1:1)) +
      geom_area(stat = "function", 
                fun = fX,
                args = list(a = input$a, b = input$b),
                fill = "steelblue1",
                alpha = 0.5,
                color = "steelblue1", size = 0.5) +
      labs(y = "density", x = "X") +
      theme(panel.grid = element_blank())
  })
  
  output$CEFapprox <- renderPlot({
    ggplot(data.frame(x = -1:1), aes(x = x)) + 
      stat_function(fun = CEF, color = "black", size = 1) +
      stat_function(
        fun = function(x) BLP(input$a, input$b)$intercept + BLP(input$a, input$b)$slope * x, 
        color = "steelblue1", size = 1) +
      labs(x = NULL, y = "Y")
  })
}

shinyApp(ui, server)
