library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel(
    tagList(
      textOutput("title", inline = TRUE),
      "Shiny App"
    ),
    # Without this, the browser tab would show the HTML of the title above.
    windowTitle = "Shiny App"
  ),
  textInput("name", label = "Enter your Name"),
  numericInput("age", label = "Enter your Age", value = 21),
  selectInput(
    "colour",
    label = "Choose your favourite colour.",
    choices = c(
      "Green" = "green",
      "Red" = "red",
      "Orange" = "orange"
    )
  ),
  textOutput("greeting"),
  uiOutput("coloured_greeting")
)

server <- function(input, output, session) {
  # req() leaves the output blank until a name has been entered, so the
  # heading reads just "Shiny App".
  output$title <- renderText({
    req(input$name)
    paste0(input$name, "'s")
  })

  # Both greetings use the same text, so compute it once in a reactive
  # expression and read it with greeting(). The req() here stops both
  # greetings until a name has been entered, since both read greeting().
  greeting <- reactive({
    req(input$name)
    paste0(
      "Hello ",
      input$name,
      ", it is great to know you are ", input$age, " years old!"
    )
  })
  output$greeting <- renderText(greeting())

  # span() escapes its text, so a name such as "<b>Bob</b>" is displayed as
  # typed. Pasting the name into an HTML() string instead would let the user
  # insert their own HTML, or even JavaScript, into the page.
  output$coloured_greeting <- renderUI(
    span(style = paste0("color: ", input$colour, ";"), greeting())
  )
}

shinyApp(ui, server)
