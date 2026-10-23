library(shiny)
library(tidyverse)

# Read the data once, when the app starts (not inside the server).
penguins <- read_csv("data/penguins.csv")

ui <- fluidPage(
  titlePanel("Penguin explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "species",
        "Choose a species:",
        choices = c("(Choose a species)" = "", "Adelie", "Gentoo", "Chinstrap")
      ),
      sliderInput(
        "mass",
        "Body mass (g):",
        min = 2700,
        max = 6300,
        value = c(2700, 6300),
        sep = ""
      ),
      checkboxInput("trend", "Show a trend line", value = TRUE)
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  selected <- reactive({
    penguins |>
      filter(
        species == input$species,
        body_mass_g >= input$mass[1],
        body_mass_g <= input$mass[2]
      )
  })

  output$plot <- renderPlot({
    req(input$species)   # do nothing until a species is chosen
    plot <- ggplot(selected(), aes(x = flipper_length_mm, y = body_mass_g)) +
      geom_point() +
      coord_cartesian(xlim = c(170, 235), ylim = c(2700, 6300)) +
      labs(
        title = paste("Body mass against flipper length:", input$species, "penguins"),
        x = "Flipper length (mm)",
        y = "Body mass (g)"
      )
    if (input$trend) {
      plot <- plot + geom_smooth(se = FALSE)
    }
    plot
  })

  output$table <- renderTable({
    req(input$species)
    selected() |>
      summarise(
        n = n(),
        mean_mass = mean(body_mass_g, na.rm = TRUE)
      )
  })
}

shinyApp(ui, server)
