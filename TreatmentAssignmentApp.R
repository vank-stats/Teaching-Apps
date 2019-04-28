library(shiny)

base_yield <- matrix(c(16, 17, 13, 17, 14, 
                    35, 33, 37, 37, 38,
                    54, 56, 56, 53, 54,
                    77, 73, 72, 75, 73,
                    92, 91, 95, 92, 95), ncol = 5, byrow = FALSE)

A.eff <- 1
B.eff <- 2
C.eff <- 3
D.eff <- 4
E.eff <- -10

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
      "For user-defined assignment, enter your treatment assignments for each row.",
      br(),
      "Don't include spaces or punctuation",
      br(),
      "e.g. ABCDE or BEDAC",
      br(), br(),
      textInput("row1Input", "Row 1 Treatments", 
                placeholder = "Enter your row 1 treatment assignments"),
      textInput("row2Input", "Row 2 Treatments", 
                placeholder = "Enter your row 2 treatment assignments"),
      textInput("row3Input", "Row 3 Treatments", 
                placeholder = "Enter your row 3 treatment assignments"),
      textInput("row4Input", "Row 4 Treatments", 
                placeholder = "Enter your row 4 treatment assignments"),
      textInput("row5Input", "Row 5 Treatments", 
                placeholder = "Enter your row 5 treatment assignments")
    ),
    mainPanel(#img(src='River.png', align = "right"),
              h3("Your Fertilizer Assignments"),
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
      row1 <- toupper(unlist(strsplit(input$row1Input, "")))
      row2 <- toupper(unlist(strsplit(input$row2Input, "")))
      row3 <- toupper(unlist(strsplit(input$row3Input, "")))
      row4 <- toupper(unlist(strsplit(input$row4Input, "")))
      row5 <- toupper(unlist(strsplit(input$row5Input, "")))
      trts <- matrix(rbind(row1, row2, row3, row4, row5), ncol = 5)
      if(sum(dim(trts) == c(5, 5)) !=2) {return(NULL)}
#      dimnames(trts) <- list(c("Row1", "Row2", "Row3", "Row4", "Row5"), 
#                           c("Col1", "Col2", "Col3", "Col4", "Col5"))
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
  })
  output$harvest <- renderTable({
    if(is.null(trt_matrix())) {return("The yield of your harvest will appear
                                      here once all treatment assignments have
                                      been specified.")}
    yield()
  })
  output$grand_ave <- renderText({
    if(is.null(trt_matrix())) {return("A grand mean and treatment means will appear
                                      here once all treatment assignments have been
                                      specified.")}
    grand <- mean(yield())
    paste("The grand mean is: ", grand)
  })
  output$A_ave <- renderText({
    if(is.null(trt_matrix())) {return()}
    a_mean <- mean(yield()[trt_matrix() == "A"])
    paste("The fertilizer A mean is: ", a_mean)
  })
  output$B_ave <- renderText({
    if(is.null(trt_matrix())) {return()}
    b_mean <- mean(yield()[trt_matrix() == "B"])
    paste("The fertilizer B mean is: ", b_mean)
  })
  output$C_ave <- renderText({
    if(is.null(trt_matrix())) {return()}
    c_mean <- mean(yield()[trt_matrix() == "C"])
    paste("The fertilizer C mean is: ", c_mean)
  })
  output$D_ave <- renderText({
    if(is.null(trt_matrix())) {return()}
    d_mean <- mean(yield()[trt_matrix() == "D"])
    paste("The fertilizer D mean is: ", d_mean)
  })
  output$E_ave <- renderText({
    if(is.null(trt_matrix())) {return()}
    e_mean <- mean(yield()[trt_matrix() == "E"])
    paste("The fertilizer E mean is: ", e_mean)
  })
}

shinyApp(ui = ui, server = server)


