.shinyplot_spatial<-function(x, SpParams, r = NULL) {
  plot_main_choices = c("Topography","Soil", "Forest stand", "Watershed")
  maps <- tabPanel("Maps",
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
                         choices = .getAllowedTopographyVars(x),
                         selected = .getAllowedTopographyVars(x)[1]
                       ),
                     ),
                     mainPanel(
                       plotOutput("spatial_plot")
                     )
                   )
  )
  ui <- navbarPage("Interactive plots", maps)
  server <- function(input, output, session) {
    observe({
      main_plot <- input$plot_main_type
      if(main_plot=="Topography") sub_choices = .getAllowedTopographyVars(x)
      else if(main_plot=="Soil") sub_choices = .getAllowedSoilVars(x)
      else if(main_plot=="Forest stand") sub_choices = .getAllowedForestStandVars(x, SpParams)
      else if(main_plot=="Watershed") sub_choices = .getAllowedWatershedVars(x)
      updateSelectInput(session, "plot_var",
                        choices = sub_choices)
    })
    output$spatial_plot <- renderPlot({
      plot_variable(x, variable = input$plot_var, SpParams = SpParams, r = r)
    })
  }
  
  shinyApp(ui = ui, server = server)
}
.shinyplot_results<-function(x, r = NULL) {
  if(inherits(x, "sf")) {
    x_map <- x
  } else if(inherits(x, "spwb_land") || inherits(x, "growth_land") || inherits(x, "fordyn_land")) {
    x_map <- x$sf
    wb_plot_choices <- .getWatershedWaterBalancePlotTypes()
    dates <- as.Date(x$watershed_balance$dates)
    watershed <- tabPanel("Watershed-level",
                          sidebarLayout(
                            sidebarPanel(
                              tabPanel("Plot selection",
                                       selectInput(
                                         inputId = "plot_type",
                                         label = "Plot type", 
                                         choices = wb_plot_choices,
                                         selected = wb_plot_choices[1]
                                       ),
                                       sliderInput(
                                         inputId = "date_range",
                                         label = "Date range",
                                         value = c(dates[1],dates[length(dates)]),
                                         min = dates[1],
                                         max = dates[length(dates)]
                                       ),
                                       selectInput(
                                         inputId = "summary_type",
                                         label = "Temporal aggregation", 
                                         choices = c("None (days)" = "day", 
                                                     "By weeks" = "week", 
                                                     "By months" = "month", 
                                                     "By seasons" = "quarter", 
                                                     "By years" = "year"),
                                         selected = "day"
                                       )
                              ),
                            ),
                            mainPanel(
                              plotOutput("watershed_plot")
                            )
                          ))
  }
  
  not_null <- which(!unlist(lapply(x_map$summary, is.null)))
  map_variables <- colnames(x_map$summary[[not_null[1]]])
  map_dates <- as.Date(rownames(x_map$summary[[not_null[1]]]))
  
  maps <- tabPanel("Maps",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput(
                               inputId = "map_var",
                               label = "Variable", 
                               choices = map_variables,
                               selected = map_variables[1]
                             ),
                             selectInput(
                               inputId = "map_date",
                               label = "Summary date", 
                               choices = map_dates,
                               selected = map_dates[1]
                             )
                           ),
                           mainPanel(
                             plotOutput("summary_map")
                           )
                         )
                      )
  if(inherits(x, "sf")) {
    ui <- navbarPage("Interactive plots", maps)
  } else {
    ui <- navbarPage("Interactive plots", watershed, maps)
  }
  server <- function(input, output, session) {
    
    output$summary_map <- renderPlot({
      plot_summary(x_map, variable = input$map_var, date = input$map_date, r = r)
    })
    output$watershed_plot <- renderPlot({
      date_lim <- input$date_range
      date_range <- dates[dates >= date_lim[1] & dates <= date_lim[2]]
      plot(x,
           type = input$plot_type,
           summary.freq = input$summary_type,
           dates = date_range)
    })
  }
  shinyApp(ui = ui, server = server)
}

#' Shiny app with interactive plots and maps
#' 
#' Creates a shiny app with interactive plots for spatial inputs and simulation results 
#' 
#' @param x The object of class 'sf' containing information to be drawn (see details). Alternatively, an object of class 'spwb_land', 'growth_land' or 'fordyn_land'.
#' @param SpParams A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}), required for most forest stand variables.
#' @param r An object of class \code{\link[terra]{SpatRaster}}, defining the raster topology.
#' 
#' @details Only run this function in interactive mode. The shiny app can be used to display spatial inputs or simulation results. 
#' 
#'   \emph{Spatial inputs}:
#'     This is the case if the user supplies an object of class \code{\link[sf]{sf}} with simulation inputs.
#'     
#'   \emph{Simulation result summaries}:
#'     This is the case if the user supplies an object of class \code{\link[sf]{sf}} with simulation summaries. Available plots depend on the summary function used to create the result summaries.
#'     
#' @return An object that represents the shiny app 
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#' 
#' @seealso \code{\link{plot_summary}}, \code{\link{extract_variables}}
#' 
#' @export
shinyplot_land<-function(x, SpParams = NULL, r = NULL) {
  if(inherits(x, "sf") && ("elevation" %in% names(x))) {
    return(.shinyplot_spatial(x, SpParams, r))
  }
  else if(inherits(x, "sf") && ("summary" %in% names(x))) {
    not_null <- which(!unlist(lapply(x$summary, is.null)))
    if(length(not_null)>0) return(.shinyplot_results(x, r))
    else stop("Column 'summary' is NULL for all elements")
  }
  else if(inherits(x, "spwb_land") || inherits(x, "growth_land") || inherits(x, "fordyn_land")) {
    x_map <- x$sf
    if("summary" %in% names(x_map)) {
      not_null <- which(!unlist(lapply(x_map$summary, is.null)))
      if(length(not_null)>0) return(.shinyplot_results(x, r))
      else stop("Column 'summary' is NULL for all elements")
    }
  } else {
    stop("Wrong class type for 'x'")
  }
}


  
