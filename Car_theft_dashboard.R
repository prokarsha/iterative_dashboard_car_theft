library(shiny)
library(ggplot2)

# Load the dataset
data <- read.csv(file.choose())


# Define UI
ui <- fluidPage(
  titlePanel("Vehicle Theft Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State:", choices = unique(data$State)),
      sliderInput("year_range", "Select Year Range:",
                  min = min(data$Model.Year), max = max(data$Model.Year),
                  value = c(min(data$Model.Year), max(data$Model.Year)), step = 1)
    ),
    mainPanel(
      plotOutput("theft_plot"),
      plotOutput("rank_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$theft_plot <- renderPlot({
    filtered_data <- data[data$State == input$state & 
                            data$Model.Year >= input$year_range[1] & 
                            data$Model.Year <= input$year_range[2], ]
    ggplot(filtered_data, aes(x = Model, y = Thefts, fill = Model)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = paste("Vehicle Theft in", input$state),
           x = "Vehicle Model", y = "Thefts")
  })
  
  output$rank_plot <- renderPlot({
    filtered_data <- data[data$State == input$state & 
                            data$Model.Year >= input$year_range[1] & 
                            data$Model.Year <= input$year_range[2], ]
    ggplot(filtered_data, aes(x = Model.Year, y = Rank, group = Model, color = Model)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Rank of Vehicle Models in", input$state),
           x = "Model Year", y = "Rank")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
