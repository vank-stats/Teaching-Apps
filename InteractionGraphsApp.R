library(shiny)
library(ggplot2)
library(dplyr)
efflabels <- c("Factor A", "Factor B", "Interaction")

ui <- fluidPage(
   
   # Application title
   titlePanel("Exploring Interaction Effect Graphs"),
   
   # Sidebar with user inputs to define the true values for the experiment 
   sidebarLayout(
      sidebarPanel(
        radioButtons("aeff", "Factor A Effect", 
                     choices = c("None" = "0", "Small" = "2", "Large" = "8"), 
                     selected = "2"),
        radioButtons("beff", "Factor B Effect", 
                     choices = c("None" = "0", "Small" = "2", "Large" = "8"), 
                     selected = "2"),
        radioButtons("inteff", "Interaction Effect", 
                     choices = c("None" = "0", "Small" = "2", "Medium" = "8"), 
                     selected = "0"),
        radioButtons("noise", "Amount of Noise", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Medium"),
        radioButtons("reps", "Observations per Treatment Combo", 
                     choices = c(2, 4, 8, 16), 
                     selected = 2)
      ),
      
      # Show an interaction plot, ANOVA results, and summarize
      mainPanel(
         plotOutput("intPlot"),
         verbatimTextOutput("data"),
         br(),
         verbatimTextOutput("result"),
         br(),
         verbatimTextOutput("reality")
      )
   )
)

server <- function(input, output) {
  
  # Convert inputs to numeric
  effects <- reactive({
    as.numeric(c(input$aeff, input$beff, input$inteff))
  })
  reps <- reactive({
    as.numeric(input$reps)
  })
  
  # Define a standard deviation based on user inputs
  sd <- reactive({
    case_when(input$noise == "Small" ~ .1*max(effects()),
              input$noise == "Medium" ~ .5*max(effects()),
              input$noise == "Large" ~ max(effects()))
  })
  
  # Create data based on user inputs
  factora <- reactive({
    c(rep(1, 2 * reps()), rep(2, 2 * reps()))
  })
  factorb <- reactive({
    rep(c(1, 2), 2 * reps())
  })
  means <- reactive({
    factora()*effects()[1] + factorb()*effects()[2] +
      (factora() == factorb())*effects()[3]
  })
  response <- reactive({
    rnorm(4 * reps(), means(), sd())
  })
  df <- reactive({
    data.frame(factora = as.factor(factora()), factorb = as.factor(factorb()), 
               response = response())
  })
  df_means <- reactive({
    aggregate(response ~ factora * factorb, data = df(), mean)
  })
  
  # Calculate an ANOVA table based on the "experiment"
  results <- reactive({
    summary(aov(response ~ factora*factorb, data = df()))
  })
  
  # Determine which effects were real and which were detected (at .05 level)
  detect <- reactive({
    as.vector(unlist(results())[c("Pr(>F)1", "Pr(>F)2", "Pr(>F)3")]) < .05
  })
  actual <- reactive({
    as.vector(as.numeric(c(input$aeff, input$beff, input$inteff)) > 0)
  })
  
  # Create an interaction plot based on the data
  output$intPlot <- renderPlot({
    
    ggplot(df(), aes(x = factora, y = response, color = factorb)) + 
      geom_point(size = 3, alpha = .6) +
      geom_line(data = df_means(), aes(x = factora, y = response, group = factorb,
                                       linetype = factorb)) +
      scale_colour_manual(values = c("red4", "darkgoldenrod1")) +
      theme_classic()
  })
  
  # Print the ANOVA table
  output$data <- renderPrint({
    results()
  })
  
  # Describe the detected (at alpha = .05) and actual effects to the user
  output$result <- renderText({
    paste("We detected effects for:", 
          paste(efflabels[detect()], collapse = ", "), sep="\t\t")
  })
  output$reality <- renderText({
    paste("In reality there were effects for:     ", 
          paste(efflabels[actual()], collapse = ", "), sep="\t")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

