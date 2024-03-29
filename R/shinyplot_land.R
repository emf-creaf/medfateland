.shinyplot_spatial<-function(x, SpParams) {
  plot_main_choices = c("Topography","Soil", "Forest stand")
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "plot_main_type",
          label = "Category",
          choices = plot_main_choices,
          selected = plot_main_choices[1]
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
      else if(main_plot=="Forest stand") sub_choices = .getAllowedForestStandVars(SpParams)
      else if(main_plot=="Watershed") sub_choices = .getAllowedWatershedVars()
      updateSelectInput(session, "plot_var",
                        choices = sub_choices)
    })
    output$spatial_plot <- renderPlot({
      plot_variable(x, variable = input$plot_var, SpParams = SpParams)
    })
  }
  
  shinyApp(ui = ui, server = server)
}
.shinyplot_results<-function(x) {
  not_null <- which(!unlist(lapply(x$summary, is.null)))
  vars = colnames(x$summary[[not_null[1]]])
  dates = rownames(x$summary[[not_null[1]]])
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
      plot_summary(x, variable = input$plot_var, date = input$plot_date)
    })
  }
  shinyApp(ui = ui, server = server)
}

#' Shiny app with interactive plots and maps
#' 
#' Creates a shiny app with interactive plots for spatial inputs and simulation results 
#' 
#' @param x The object of class 'sf' containing information to be drawn (see details).
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}), required for most forest stand variables.
#' 
#' @details Only run this function in interactive mode. The shiny app can be used to display spatial inputs or simulation results. 
#' 
#'   \emph{Spatial inputs}:
#'     This is the case if the user supplies an object of class \code{\link{sf}} with simulation inputs.
#'     
#'   \emph{Simulation result summaries}:
#'     This is the case if the user supplies an object of class \code{\link{sf}} with simulation summaries. Available plots depend on the summary function used to create the result summaries.
#'     
#' @return An object that represents the shiny app 
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link{plot_summary}}, \code{\link{extract_variables}}
#' 
#' @export
shinyplot_land<-function(x, SpParams = NULL) {
  if(inherits(x, "sf") && ("elevation" %in% names(x))) {
    return(.shinyplot_spatial(x, SpParams))
  }
  else if(inherits(x, "sf") && ("summary" %in% names(x))) {
    not_null <- which(!unlist(lapply(x$summary, is.null)))
    if(length(not_null)>0) return(.shinyplot_results(x))
    else stop("Column 'summary' is NULL for all elements")
  }
  else {
    stop("Wrong class type for 'x'")
  }
}


  
