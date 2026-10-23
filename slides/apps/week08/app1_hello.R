library(shiny)

ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num", "Choose a number:", 1, 100, 50)
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

server <- function(input, output, session) {
  output$result <- renderText({
    paste("You selected:", input$num)
  })
}

shinyApp(ui, server)
