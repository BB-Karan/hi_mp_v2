library(shiny)

s<-function(input, output) {
  
  # Return the requested dataset. Note that we use `eventReactive()`
  # here, which takes a dependency on input$update (the action
  # button), so that the output is only updated when the user
  # clicks the button.
  datasetInput <- eventReactive(input$update, {
    # switch(input$dataset,
    #        "rock" = rock,
    #        "pressure" = pressure,
    #        "cars" = cars)
  return(eval(parse(text = input$dataset)))
    }, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations. The use of `isolate()` here
  # is necessary because we don't want the table to update
  # whenever input$obs changes (only when the user clicks the
  # action button).
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
}


u<-fluidPage(
  
  # Application title.
  titlePanel("More Widgets"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view. The helpText function is
  # also used to include clarifying text. Most notably, the
  # inclusion of an actionButton defers the rendering of output
  # until the user explicitly clicks the button (rather than
  # doing it immediately when inputs change). This is useful if
  # the computations required to render output are inordinately
  # time-consuming.
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      
      numericInput("obs", "Number of observations to view:", 10),
      
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
      actionButton("update", "Update View")
    ),
    
    # Show a summary of the dataset and an HTML table with the
    # requested number of observations. Note the use of the h4
    # function to provide an additional header above each output
    # section.
    mainPanel(
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      h4("Observations"),
      tableOutput("view"),
      
      h4("test changes")
    )
  )
)

shinyApp(ui=u,server=s)
