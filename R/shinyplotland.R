.shinyplot_spatial<-function(x) {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "plot_var",
          label = "Variable", 
          choices = .getAllowedVars(),
          selected = .getAllowedVars()[1]
        ),
      ),
      mainPanel(
        plotOutput("spatial_plot")
      )
    )
  )
  server <- function(input, output, session) {
  
    output$spatial_plot <- renderPlot({
      plot(x, input$plot_var)
    })
  }
  
  shinyApp(ui = ui, server = server)
}
.shinyplot_results<-function(x) {
  vars = colnames(x$summarylist[[1]])
  dates = rownames(x$summarylist[[1]])
  if(inherits(x, c("spwbland", "growthland"))) {
    vars = c("DailyRunoff", vars)
  }
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "plot_var",
          label = "Variable", 
          choices = vars,
          selected = vars[1]
        ),
        selectInput(
          inputId = "plot_date",
          label = "Date", 
          choices = dates,
          selected = dates[1]
        )
      ),
      mainPanel(
        plotOutput("summary_plot")
      )
    )
  )
  server <- function(input, output, session) {
    
    output$summary_plot <- renderPlot({
      plot(x, variable = input$plot_var, date = input$plot_date)
    })
  }
  shinyApp(ui = ui, server = server)
}
shinyplotland<-function(x) {
  if(inherits(x, c("SpatialPointsLandscape", "SpatialPixelsLandscape", "SpatialGridLandscape")))
    return(.shinyplot_spatial(x))
  else if(inherits(x, c("summarypoints", "summarygrid", "summarypixels")))
    return(.shinyplot_results(x))
  else {
    stop("Wrong class type for 'x'")
  }
}


  