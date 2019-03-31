library(shinydashboard)
source("global.R")

# TODO: Retrieve transfer data for last six months once a day? Will save
# multiple DB queries.

# Define UI for application that draws a histogram
ui <- dashboardPage(
  title = "Hohmann | Transfer time estimation",
  skin = "purple",

  # Application title
  dashboardHeader(title = tags$a(href = "https://en.wikipedia.org/wiki/Hohmann_transfer_orbit",
    tags$img(src = "hohmann-black.png", height = 20), "Hohmann"),
    titleWidth = "25%"),

  # Sidebar with inputs to select account type and provider
  dashboardSidebar(
    width = "25%",
    selectInput(
      width = "100%",
      inputId = "provider_input",
      label = "Provider",
      choices = list()
    ),
    selectInput(
      width = "100%",
      inputId = "product_input",
      label = "Product type",
      choices = list(
        Pension = "SIPP",
        ISA = "ISA",
        Unwrapped = "Unwrapped"
      )
    ),
    selectInput(
      width = "100%",
      inputId = "method_input",
      label = "Stock or cash transfer?",
      choices = list(
        Stock = "Stock",
        Cash = "Cash"
      )
    ),
    actionButton(
      inputId = "go_button",
      icon = icon("directions"),
      label = "Look up transfer data"
    ),
    br(),
    # Message box
    div(width = 12, style = "text-align:center;",
        h4(textOutput("message_text", inline = TRUE))
    )
  ),

  # Main body
    dashboardBody(
      # Link to custom CSS
      tags$link(rel = "stylesheet", type = "text/css", href = "master.css"),
      # Boxes to display values
      fluidRow(
        box(title = "Postal rereg", width = 12,
          valueBox(
            color = "green",
            value = textOutput("low_estimate_postal"),
            subtitle = "25% of transfers complete within this time"
          ),
          valueBox(
            color = "orange",
            value = textOutput("medium_estimate_postal"),
            subtitle = "50% of transfers complete within this time"
          ),
          valueBox(
            color = "red",
            value = textOutput("high_estimate_postal"),
            subtitle = "75% of transfers complete within this time"
          )
        )
      ),
      fluidRow(
        box(title = "Electronic rereg", width = 12,
            valueBox(
              color = "green",
              value = textOutput("low_estimate_electronic"),
              subtitle = "25% of transfers complete within this time"
            ),
            valueBox(
              color = "orange",
              value = textOutput("medium_estimate_electronic"),
              subtitle = "50% of transfers complete within this time"
            ),
            valueBox(
              color = "red",
              value = textOutput("high_estimate_electronic"),
              subtitle = "75% of transfers complete within this time"
            )
        )
      ),
      # Plot of historic transfer times
      fluidRow(
        box(
          width = 12,
          plotOutput("plot_output")
        )
      )
    )
)

server <- function(input, output, session) {

  # Populate the dropdown with the list of providers
  withProgress(message = "Checking available providers", {
    updateSelectInput(session, inputId = "provider_input",
                      choices = get_provider_list())
  })

  observeEvent(input$go_button, {
    message("Fetching data...")

    # Collect the info from the input
    provider <- isolate(input$provider_input)
    method <- isolate(input$method_input)
    product <- isolate(input$product_input)

    # Retrieve data
    withProgress(message = "Querying transfers database", value = 0.5, {
      # TODO: Make sure the result contains a decent number of transfers
      result <- data[data$provider == provider & data$product == product & data$method == method, ]
    })

    success <- nrow(result) > 0

    # Update chart header
    output$message_text <- renderText(
      create_message(provider, product, method, success)
    )

    # Extract low med and high estimates for postal and electronic rereg
    # TODO: How will this handle missing values? Probably best to pass the
    # entire `result` dataset in and let `get_estimates` figure out what to do.
    estimates <- get_estimates(result)

    if (success) {
      # Show the plot
      output$plot_output <- renderPlot(make_plot(result))
    } else {
      # Render a blank plot to clear the plot area
      output$plot_output <- renderPlot(ggplot() + theme_void())
    }

    # Update the box content
    output$low_estimate_postal <- renderText(estimates$low_estimate_postal)
    output$medium_estimate_postal <- renderText(estimates$medium_estimate_postal)
    output$high_estimate_postal <- renderText(estimates$high_estimate_postal)
    output$low_estimate_electronic <- renderText(estimates$low_estimate_electronic)
    output$medium_estimate_electronic <- renderText(estimates$medium_estimate_electronic)
    output$high_estimate_electronic <- renderText(estimates$high_estimate_electronic)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
