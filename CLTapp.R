#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2) # for plotting approx. sampling distributions.
library(gridExtra) # for arranging plots in grid
library(dplyr) # for case_when function to choose dist. to sample from

reps <- 10000
dist.df <- data.frame(
  dists = c("Poisson(2)", "Exp(1)", "Weibull(10, 3.5)", "Beta(2, 2)",
            "Uniform(0, 1)", "Normal(100, 10)"),
  num = 1:6,
  mean = c(2, 1, 10/3, .5, .5, 100)
)

# Define UI for application that draws approximate sampling distributions
ui <- fluidPage(
   
   # Application title
   titlePanel("STS342 - Notes 3 - Central Limit Theorem"),
   
   # Sidebar with inputs for sample sizes and four distributions to draw from 
   sidebarLayout(
      sidebarPanel(
         radioButtons("n",
                     "Sample Size",
                     choices = c(2, 10, 30, 100, 1000),
                     selected = 2),
         selectInput("dist1",
                     "Choose Distribution 1",
                     choices = c("Poisson(2)", "Exp(1)", 
                                 "Weibull(10, 3.5)", "Beta(2, 2)",
                                 "Uniform(0, 1)", "Normal(100, 10)"),
                     selected = "Poisson(2)"),
         selectInput("dist2",
                     "Choose Distribution 2",
                     choices = c("Poisson(2)", "Exp(1)", 
                                 "Weibull(10, 3.5)", "Beta(2, 2)",
                                 "Uniform(0, 1)", "Normal(100, 10)"),
                     selected = "Exp(1)"),
         selectInput("dist3",
                     "Choose Distribution 3",
                     choices = c("Poisson(2)", "Exp(1)", 
                                 "Weibull(10, 3.5)", "Beta(2, 2)",
                                 "Uniform(0, 1)", "Normal(100, 10)"),
                     selected = "Weibull(10, 3.5)"),
         selectInput("dist4",
                     "Choose Distribution 4",
                     choices = c("Poisson(2)", "Exp(1)", 
                                 "Weibull(10, 3.5)", "Beta(2, 2)",
                                 "Uniform(0, 1)", "Normal(100, 10)"),
                     selected = "Beta(2, 2)")
      ),
      
      # Show a plot of the generated distributions
      mainPanel(
        h4("Below are approximate sampling distributions for the mean based on 
           10,000 random samples of size n drawn from each distribution"),
        br(),
        plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw approximate sampling distributions
server <- function(input, output) {

# Generate 10,000 random values from each of the distributions

     samples <- reactive({
       data.frame(a = replicate(reps, mean(rpois(input$n, 2))),
                  b = replicate(reps, mean(rexp(input$n, 1))),
                  c = replicate(reps, mean(rweibull(input$n, 10, 3.5))),
                  d = replicate(reps, mean(rbeta(input$n, 2, 2))),
                  e = replicate(reps, mean(runif(input$n, 0, 1))),
                  f = replicate(reps, mean(rnorm(input$n, 100, 10))))
     })

# Match four distributions to samples from those distributions
     dist1 <- reactive({
      samp1 <- case_when(input$dist1 == "Poisson(2)" ~ samples()$a,
                       input$dist1 == "Exp(1)" ~ samples()$b,
                       input$dist1 == "Weibull(10, 3.5)" ~ samples()$c,
                       input$dist1 == "Beta(2, 2)" ~ samples()$d,
                       input$dist1 == "Uniform(0, 1)" ~ samples()$e,
                       input$dist1 == "Normal(100, 10)" ~ samples()$f)
      data.frame(x = samp1)
   })
   dist2 <- reactive({
     samp2 <- case_when(input$dist2 == "Poisson(2)" ~ samples()$a,
                        input$dist2 == "Exp(1)" ~ samples()$b,
                        input$dist2 == "Weibull(10, 3.5)" ~ samples()$c,
                        input$dist2 == "Beta(2, 2)" ~ samples()$d,
                        input$dist2 == "Uniform(0, 1)" ~ samples()$e,
                        input$dist2 == "Normal(100, 10)" ~ samples()$f)
     data.frame(x = samp2)
   })
   dist3 <- reactive({
     samp3 <- case_when(input$dist3 == "Poisson(2)" ~ samples()$a,
                        input$dist3 == "Exp(1)" ~ samples()$b,
                        input$dist3 == "Weibull(10, 3.5)" ~ samples()$c,
                        input$dist3 == "Beta(2, 2)" ~ samples()$d,
                        input$dist3 == "Uniform(0, 1)" ~ samples()$e,
                        input$dist3 == "Normal(100, 10)" ~ samples()$f)
     data.frame(x = samp3)
   })
   dist4 <- reactive({
     samp4 <- case_when(input$dist4 == "Poisson(2)" ~ samples()$a,
                        input$dist4 == "Exp(1)" ~ samples()$b,
                        input$dist4 == "Weibull(10, 3.5)" ~ samples()$c,
                        input$dist4 == "Beta(2, 2)" ~ samples()$d,
                        input$dist4 == "Uniform(0, 1)" ~ samples()$e,
                        input$dist4 == "Normal(100, 10)" ~ samples()$f)
      data.frame(x = samp4)
   })

# Create approx. sampling distribution graphs for each plot   
   output$distPlot <- renderPlot({
     p1 <- ggplot(data = dist1(), aes(x = x)) +
       geom_histogram(binwidth = (1 / as.numeric(input$n))) +
       labs(title = input$dist1,
            x = paste("Sample Means (n = ", input$n, ")", sep = "")) +
       geom_vline(xintercept = subset(dist.df, dists == input$dist1)$mean, 
                  color = "red")
     p2 <- ggplot(data = dist2(), aes(x = x)) +
       geom_density() +
       labs(title = input$dist2,
            x = paste("Sample Means (n = ", input$n, ")", sep = "")) +
       geom_vline(xintercept = subset(dist.df, dists == input$dist2)$mean, 
                  color = "red")
     p3 <- ggplot(data = dist3(), aes(x = x)) +
       geom_density() +
       labs(title = input$dist3,
            x = paste("Sample Means (n = ", input$n, ")", sep = "")) +
       geom_vline(xintercept = subset(dist.df, dists == input$dist3)$mean, 
                  color = "red")
     p4 <- ggplot(data = dist4(), aes(x = x)) +
       geom_density() +
       labs(title = input$dist4,
            x = paste("Sample Means (n = ", input$n, ")", sep = "")) +
       geom_vline(xintercept = subset(dist.df, dists == input$dist4)$mean, 
                  color = "red")

     grid.arrange(p1, p2, p3, p4, nrow = 2)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

