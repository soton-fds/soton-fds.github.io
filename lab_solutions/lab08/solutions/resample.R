library(shiny)
library(tidyverse)

ui <- fluidPage(
  fluidRow(
    column(
      2,
      numericInput("mu1", label = withMathJax("$$\\mu_1$$"), value = -1, width = "80px")
    ),
    column(
      2,
      numericInput("mu2", label = withMathJax("$$\\mu_2$$"), value = 1, width = "80px")
    ),
    column(
      3,
      actionButton("resample", "Draw a new sample")
    )
  ),
  plotOutput("plot"),
  textOutput("means"),
  tableOutput("table")
)

server <- function(input, output, session) {
  # A sample from the standard normal distribution. Only the button changes
  # it, so it is a source that the server sets: a reactiveVal.
  noise <- reactiveVal(tibble(z1 = rnorm(10), z2 = rnorm(10)))

  # Pressing the button should make something happen (draw new noise), so
  # respond to it with an observer.
  observeEvent(input$resample, {
    noise(tibble(z1 = rnorm(10), z2 = rnorm(10)))
  })

  # Adding mu to a standard normal sample gives a sample with mean mu and
  # standard deviation 1. Changing mu1 or mu2 recomputes this, but reuses the
  # same noise, so the points move rather than being redrawn.
  samples <- reactive(tibble(
    x1 = input$mu1 + noise()$z1,
    x2 = input$mu2 + noise()$z2
  ))

  output$plot <- renderPlot(
    {
      ggplot(samples(), aes(x = x1, y = x2)) +
        geom_point() +
        coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5))
    },
    res = 96
  )

  output$means <- renderText({
    paste(
      "Sample means:",
      round(mean(samples()$x1), 2), "and",
      round(mean(samples()$x2), 2)
    )
  })

  output$table <- renderTable(samples())
}

shinyApp(ui, server)
