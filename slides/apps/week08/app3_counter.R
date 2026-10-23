library(shiny)

ui <- fluidPage(
  actionButton("inc", "Increase counter"),
  actionButton("reset", "Reset counter"),
  textOutput("value")
)

server <- function(input, output, session) {
  # Create a reactiveVal starting at 0.
  # Retrieve its value by calling it, counter(); set it by calling it with an
  # argument, counter(new_value).
  counter <- reactiveVal(0)

  # Increase counter when 'inc' button is clicked
  observeEvent(input$inc, {
    counter(counter() + 1)   # update value
  })

  # Reset counter when 'reset' button is clicked
  observeEvent(input$reset, {
    counter(0)
  })

  # Display the current value
  output$value <- renderText({
    paste("Counter is:", counter())   # read value
  })
}

shinyApp(ui, server)
