library(shiny)
library(tidyverse)

# Read the data once, when the app starts. Shiny runs the app from its own
# folder, so the path is relative to solutions/.
penguins <- read_csv("../src/data/penguins.csv")

# The names are displayed in the dropdowns; the values are the column names
# that input$... will hold.
numeric_choices <- c(
  "Bill length (mm)" = "bill_length_mm",
  "Bill depth (mm)" = "bill_depth_mm",
  "Flipper length (mm)" = "flipper_length_mm",
  "Body mass (g)" = "body_mass_g"
)

categorical_choices <- c(
  "Species" = "species",
  "Island" = "island",
  "Sex" = "sex"
)

# Use the label the user has typed, or `default` if the box is empty.
# (labs(x = "") would remove the axis label altogether.)
label_or_default <- function(label, default) {
  if (label == "") default else label
}

ui <- fluidPage(
  titlePanel("Penguin visualiser"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "x-axis variable:", choices = numeric_choices),
      selectInput(
        "y_var",
        "y-axis variable:",
        choices = numeric_choices,
        selected = "body_mass_g"
      ),
      selectInput("colour_var", "Colour points by:", choices = categorical_choices),
      textInput("x_label", "x-axis label:"),
      textInput("y_label", "y-axis label:"),
      textInput("title", "Plot title:"),
      selectInput("filter_var", "Filter by:", choices = categorical_choices),
      # The dropdown of values to keep is built in the server, since its
      # choices depend on input$filter_var.
      uiOutput("filter_value_ui")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  output$filter_value_ui <- renderUI({
    values <- sort(unique(na.omit(penguins[[input$filter_var]])))
    selectInput("filter_value", "Show only:", choices = c("(All)" = "", values))
  })

  # The filtered data is a reactive expression, so it is only recomputed when
  # the filter changes, not when (say) a label is edited.
  selected <- reactive({
    # input$filter_value is NULL until the dropdown first appears, and just
    # after a new filter variable is chosen it still holds a value of the old
    # variable (e.g. "Dream" while filtering by sex). Filtering on it then
    # would briefly leave no rows, so only filter on a value that belongs to
    # the current variable; "(All)" is "", which never does.
    if (isTRUE(input$filter_value %in% penguins[[input$filter_var]])) {
      penguins |>
        filter(.data[[input$filter_var]] == input$filter_value)
    } else {
      penguins
    }
  })

  output$plot <- renderPlot({
    # input$x_var etc. are strings, so .data[[...]] is needed to refer to the
    # columns they name.
    ggplot(
      selected(),
      aes(
        x = .data[[input$x_var]],
        y = .data[[input$y_var]],
        colour = .data[[input$colour_var]]
      )
    ) +
      geom_point() +
      # Fix the axes to cover the whole dataset, so that plots of different
      # subsets can be compared, rather than each being rescaled to fit its
      # own points.
      coord_cartesian(
        xlim = range(penguins[[input$x_var]], na.rm = TRUE),
        ylim = range(penguins[[input$y_var]], na.rm = TRUE)
      ) +
      labs(
        # By default, label each axis with the name shown in its dropdown.
        x = label_or_default(input$x_label, names(numeric_choices)[numeric_choices == input$x_var]),
        y = label_or_default(input$y_label, names(numeric_choices)[numeric_choices == input$y_var]),
        colour = names(categorical_choices)[categorical_choices == input$colour_var],
        title = label_or_default(input$title, NULL)
      )
  })
}

shinyApp(ui, server)
