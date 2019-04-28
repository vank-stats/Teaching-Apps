# Make so only numbers 1-100 are added to the count
# Provide a message if they have duplicates?


library(shiny)

# Define the yield of the 100 plots of land in the field
yield <- matrix(c(6, 17, 20, 38, 47, 55, 69, 76, 82, 97,
                  7, 14, 23, 34, 43, 56, 63, 75, 81, 92,
                  2, 14, 28, 30, 50, 50, 62, 80, 85, 96,
                  9, 15, 27, 34, 43, 51, 65, 72, 88, 91,
                  4, 15, 28, 32, 44, 50, 64, 76, 82, 97,
                  5, 16, 27, 31, 48, 59, 69, 72, 86, 99,
                  5, 18, 28, 34, 50, 60, 62, 75, 90, 90,
                  8, 15, 20, 38, 40, 54, 62, 77, 88, 93,
                  7, 17, 29, 39, 44, 53, 61, 77, 80, 90,
                  7, 19, 22, 33, 49, 53, 67, 76, 86, 97),
                ncol = 10, byrow = TRUE)

# Create a matrix numbered 1-100 to choose samples
sampling_matrix <- matrix(1:100, nrow = 10, ncol = 10, byrow = TRUE)


ui <- fluidPage(
  titlePanel("STS110 - Estimating Corn Yield"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("num1", "Use the ten blanks below to enter numbers corresponding to 
the ten plots you wish to select for you sample",
                  value = NA, min = 1, max = 100, step = 1),
      numericInput("num2", label = NA,
                   value = NA, min = 1, max = 100, step = 1),
      numericInput("num3", label = NA,
                   value = NA, min = 1, max = 100, step = 1),
      numericInput("num4", label = NA,
                   value = NA, min = 1, max = 100, step = 1),
      numericInput("num5", label = NA,
                   value = NA, min = 1, max = 100, step = 1),
      numericInput("num6", label = NA,
                   value = NA, min = 1, max = 100, step = 1),
      numericInput("num7", label = NA,
                   value = NA, min = 1, max = 100, step = 1),
      numericInput("num8", label = NA,
                   value = NA, min = 1, max = 100, step = 1),
      numericInput("num9", label = NA,
                   value = NA, min = 1, max = 100, step = 1),
      numericInput("num10", label = NA,
                   value = NA, min = 1, max = 100, step = 1),
      textInput("password", "Reveal the yield of the entire field by entering the
                code below", value="")
      ),
 
    mainPanel(#img(src='River.png', align = "right"),
              h4("Choose a sample from these numbered plots"),
              tableOutput("SamplingMatrix"),
              h4("Yield of plots in your Sample"),
              tableOutput("Sample"),
              textOutput("Results"))
  )
)

server <- function(input, output, session) {

    # collect the numbers for the 10 plots chosen by the user
    sample <- reactive({
      c(input$num1, input$num2, input$num3, input$num4, input$num5,
        input$num6, input$num7, input$num8, input$num9, input$num10)
    })
    
    yieldest <- reactive({
      yieldest <- yield
      yieldest[!(sampling_matrix %in% sample())] <- "__"
      if(input$password == "census") {yield} else {yieldest}
    })
    
    sampled_matrix <- reactive({
      sampled_matrix <- sampling_matrix
      sampled_matrix[(sampling_matrix %in% sample())] <- "---"
      sampled_matrix
    })
    
    count <- reactive({
      count <- length(table(as.numeric(sample())))
      count
    })
    
    total <- reactive({
      total <- sum(as.numeric(yieldest()), na.rm = TRUE)
      total
    })
    
    output$SamplingMatrix <- renderTable({
      sampled_matrix()
    }, colnames = FALSE, bordered = TRUE)
    
    output$Sample <- renderTable({
      yieldest()
    }, colnames = FALSE, bordered = TRUE, digits = 0)
    
    output$Results <- renderText({
      ifelse(input$password == "census",
         paste("The overall yield of the field is 5004"),
         paste("Your ", count(), " plots of land have a total yield of ",
            total())
      )
    })
}

shinyApp(ui = ui, server = server)


