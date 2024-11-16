install.packages("shiny")
# Load necessary library
library(shiny)

# Define the function for dP/dt with sliders
dP_function <- function(P, a, b, c, k6) {
  return((a * P) / (b + P) + c - k6 * P)
}

# Define the UI for the app
ui <- fluidPage(
  # Title of the app
  titlePanel("Plot of f(P) with Sliders for Parameters"),
  
  # Sidebar layout with sliders
  sidebarLayout(
    sidebarPanel(
      sliderInput("a", "Parameter a:", min = 0, max = 20, value = 10, step = 0.1),
      sliderInput("b", "Parameter b:", min = 0, max = 10, value = 2, step = 0.1),
      sliderInput("c", "Parameter c:", min = -10, max = 10, value = 1, step = 0.1),
      sliderInput("k6", "Parameter k6:", min = 0, max = 3, value = 1, step = 0.05)
    ),
    
    # Main panel to display the plot
    mainPanel(
      plotOutput("dPPlot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive expression to generate the plot based on slider inputs
  output$dPPlot <- renderPlot({
    # Get values from the sliders
    a <- input$a
    b <- input$b
    c <- input$c
    k6 <- input$k6
    
    # Create a range of P values
    P_values <- seq(0, 50, by = 0.5)
    
    # Compute dP/dt for each value of P using dP_function
    dP_values <- sapply(P_values, function(P) dP_function(P, a, b, c, k6))
    
    # Plot the function dP/dt vs P
    plot(P_values, dP_values, type = "l", col = "blue", 
         xlab = "P", ylab = "dP/dt", 
         main = "Interactive Plot of f(P)", lwd = 2)
    abline(h = 0, col = "red", lwd = 2)  # Add y = 0 line
    grid()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
