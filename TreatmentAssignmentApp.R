# To do 
#    - warning if things other than A-E entered

library(shiny)

# Define the base yield of the field without fertilizer
base_yield <- matrix(c(16, 17, 13, 17, 14, 
                    35, 33, 37, 37, 38,
                    54, 56, 56, 53, 54,
                    77, 73, 72, 75, 73,
                    92, 91, 95, 92, 95), ncol = 5, byrow = FALSE)

# Define the effect of the 5 fertilizers
A.eff <- 1
B.eff <- 2
C.eff <- 3
D.eff <- 4
E.eff <- -10

# Function to apply fertilizer effects
fertilize <- function(x, y) {
  for(i in 1:ncol(x)) {
    for(j in 1:nrow(x)) {
      y[i,j] <- ifelse(x[i,j] == "A", y[i,j] + A.eff, ifelse(
        x[i,j] == "B", y[i,j] + B.eff, ifelse(
          x[i,j] == "C", y[i,j] + C.eff, ifelse(
            x[i,j] == "D", y[i,j] + D.eff, ifelse(
              x[i,j] == "E", y[i,j] + E.eff, ifelse(
                x[i,j] == "X", y[i,j], NA))))))
    }
  }
  y
}

ui <- fluidPage(
  titlePanel("STS325 - Designing an Experiment to Find the Best Fertilizer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("methodInput", "Choose a Fertilizer Assignment Method",
                  choices = c("User-Defined", "Random"),
                  selected = "User-Defined"),
      "For user-defined assignment, enter treatment assignments for each row.",
      br(),
      br(),
      "Use only letters A through E and don't include spaces or punctuation",
      br(),
      "e.g. ABCDE or BEDAC",
      br(), br(),
      textInput("row1Input", "Enter treatment assignments by row", 
                placeholder = "Row 1 treatment assignments"),
      textInput("row2Input", label = NA, 
                placeholder = "Row 2 treatment assignments"),
      textInput("row3Input", label = NA, 
                placeholder = "Row 3 treatment assignments"),
      textInput("row4Input", label = NA, 
                placeholder = "Row 4 treatment assignments"),
      textInput("row5Input", label = NA, 
                placeholder = "Row 5 treatment assignments")
    ),
    mainPanel(h3("Your Fertilizer Assignments"),
              tableOutput("treatmentAssignment"),
              h3("Results from Harvest"),
              tableOutput("harvest"),
              h3("Some Relevant Averages"),
              textOutput("grand_ave"),
              textOutput("A_ave"),
              textOutput("B_ave"),
              textOutput("C_ave"),
              textOutput("D_ave"),
              textOutput("E_ave"))
  )
)

server <- function(input, output, session) {
  
  trt_matrix <- reactive({
    if(input$methodInput == "Random") {
      trts <- matrix(sample(c(rep("A", 5), rep("B", 5), rep("C", 5),
                              rep("D", 5), rep("E", 5))), ncol = 5)
    }
    if(input$methodInput == "User-Defined") {
      row1 <- if(input$row1Input == "") { rep("___", 5) } else {
        c(toupper(unlist(strsplit(input$row1Input, ""))), rep("___", 5))[1:5]
      }
      row2 <- if(input$row2Input == "") { rep("___", 5) } else {
        c(toupper(unlist(strsplit(input$row2Input, ""))), rep("___", 5))[1:5]
      }
      row3 <- if(input$row3Input == "") { rep("___", 5) } else {
        c(toupper(unlist(strsplit(input$row3Input, ""))), rep("___", 5))[1:5]
      }
      row4 <- if(input$row4Input == "") { rep("___", 5) } else {
        c(toupper(unlist(strsplit(input$row4Input, ""))), rep("___", 5))[1:5]
      }
      row5 <- if(input$row5Input == "") { rep("___", 5) } else {
        c(toupper(unlist(strsplit(input$row5Input, ""))), rep("___", 5))[1:5]
      }
      trts <- matrix(rbind(row1, row2, row3, row4, row5), ncol = 5)
    }
    colnames(trts) <- c(" ", " ", " ", " ", " ")
    trts
    })
  
  yield <- reactive({
    yld <- fertilize(trt_matrix(), base_yield) 
    dimnames(yld) <- list(c("Row1", "Row2", "Row3", "Row4", "Row5"), 
                           c("Col1", "Col2", "Col3", "Col4", "Col5"))
    colnames(yld) <- c(" ", " ", " ", " ", " ")
    yld
  })
  
  output$treatmentAssignment <- renderTable({
    trt_matrix()
  }, bordered = TRUE, colnames = FALSE, width = "200", align = "c")
  
  output$harvest <- renderTable({
    yield()
  }, digits = 0, align = "r", bordered = TRUE, colnames = FALSE, width = "200")
  
  output$grand_ave <- renderText({
    grand <- mean(yield(), na.rm = TRUE)
    paste("The grand mean is: ", round(grand, 2))
  })
  
  output$A_ave <- renderText({
    a_mean <- mean(yield()[trt_matrix() == "A"])
    paste("The fertilizer A mean is: ", round(a_mean, 2))
  })
  
  output$B_ave <- renderText({
    b_mean <- mean(yield()[trt_matrix() == "B"])
    paste("The fertilizer B mean is: ", round(b_mean, 2))
  })
  
  output$C_ave <- renderText({
    c_mean <- mean(yield()[trt_matrix() == "C"])
    paste("The fertilizer C mean is: ", round(c_mean, 2))
  })
  
  output$D_ave <- renderText({
    d_mean <- mean(yield()[trt_matrix() == "D"])
    paste("The fertilizer D mean is: ", round(d_mean, 2))
  })
  
  output$E_ave <- renderText({
    e_mean <- mean(yield()[trt_matrix() == "E"])
    paste("The fertilizer E mean is: ", round(e_mean, 2))
  })
}

shinyApp(ui = ui, server = server)


