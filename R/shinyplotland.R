.shinyplot_spatial<-function(x, SpParams) {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "plot_main_type",
          label = "Category",
          choices = c("Topography","Soil", "Forest stand"),
          selected = c("Topography")
        ),
        selectInput(
          inputId = "plot_var",
          label = "Variable", 
          choices = .getAllowedTopographyVars(),
          selected = .getAllowedTopographyVars()[1]
        ),
      ),
      mainPanel(
        plotOutput("spatial_plot")
      )
    )
  )
  server <- function(input, output, session) {
    observe({
      main_plot <- input$plot_main_type
      if(main_plot=="Topography") sub_choices = .getAllowedTopographyVars()
      else if(main_plot=="Soil") sub_choices = .getAllowedSoilVars()
      else if(main_plot=="Forest stand") sub_choices = .getAllowedForestStandVars()
      updateSelectInput(session, "plot_var",
                        choices = sub_choices)
    })
    output$spatial_plot <- renderPlot({
      plot(x, y= input$plot_var, SpParams = SpParams)
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
shinyplotland<-function(x, SpParams = NULL) {
  if(inherits(x, c("SpatialPointsLandscape", "SpatialPixelsLandscape", "SpatialGridLandscape")))
    return(.shinyplot_spatial(x, SpParams))
  else if(inherits(x, c("summarypoints", "summarygrid", "summarypixels")))
    return(.shinyplot_results(x))
  else {
    stop("Wrong class type for 'x'")
  }
}


  