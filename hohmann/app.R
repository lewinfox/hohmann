library(shinydashboard)
source("global.R")

# TODO: Retrieve transfer data for last six months once a day? Will save
# multiple DB queries.

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "purple",

   # Application title
   dashboardHeader(title = "Hohmann"),

   # Sidebar with a slider input for number of bins
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
        label = "Lookup transfer data"
      )
    ),

  # Main body
    dashboardBody(
      # Message box
      fluidRow(
        infoBox(
          title = "Something",
          subtitle = "Something else",
          value = textOutput("message_text", inline = TRUE),
          width = 12)
      ),
      # Boxes to display values
      fluidRow(
        valueBox(
          color = "green",
          value = textOutput("low_estimate"),
          subtitle = "Low transfer time estimate"
          ),
        valueBox(
          color = "orange",
          value = textOutput("medium_estimate"),
          subtitle = "Medium transfer time estimate"
        ),
        valueBox(
          color = "red",
          value = textOutput("high_estimate"),
          subtitle = "High transfer time estimate"
        )
      ),
      # Plot of hisoric transfer times
      fluidRow(
        box(
          width = 12,
          height = "60vh",
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
    message("Click")

    # Collect the info from the input
    provider <- isolate(input$provider_input)
    method <- isolate(input$method_input)
    product <- isolate(input$product_input)

    # Retrieve data
    withProgress(message = "Querying transfers database", value = 0.5, {
      result <- data[data$provider == provider & data$product == product & data$method == method, ]
    })

    success <- nrow(result) > 0

    # Update chart header
    output$message_text <- renderText(
      create_message(provider, product, method, success)
    )

    if (success) {
      # Extract low med and high estimates
      low_estimate <- rnorm(1)
      medium_estimate <- rnorm(1)
      high_estimate <- rnorm(1)

      # Update info boxes with real data
      output$low_estimate <- renderText(create_duration_string(low_estimate))
      output$medium_estimate <- renderText(create_duration_string(medium_estimate))
      output$high_estimate <- renderText(create_duration_string(high_estimate))

      # Show the plot
      output$plot_output <- renderPlot(make_plot(result))
    } else {
      # Clear the estimates
      output$low_estimate <- renderText("No data")
      output$medium_estimate <- renderText("No data")
      output$high_estimate <- renderText("No data")

      # Render a blank plot to clear the plot area
      output$plot_output <- renderPlot(ggplot() + theme_void())
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

