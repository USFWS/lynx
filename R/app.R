################################################################################
# A shiny app for displaying and summarizing lynx collar data                  #
#                                                                              #
# Produces maps, home ranges (MCPs, kernel, and Brownian Bridge methods), and  #
# plots movement statistics.                                                   #
#                                                                              #
# Author: McCrea Cobb <mccrea_cobb@fws.gov>                                    #
# Date last edited: 7/18/2018                                                  #
################################################################################

# Clear global env. and free up memory:
rm(list=ls())

# Source required scripts:
source("global.R")

# Install required packages:
library(shiny)
library(dplyr)
library(readr)
library(leaflet)
library(data.table)
library(gridExtra)
library(geojsonio)
library(lubridate)
library(ggplot2)
library(sp)
library(adehabitatHR)
library(adehabitatLT)
library(maptools)
library(shinyjs)
library(magrittr)
library(highcharter)

# Load GPS collar data:
load("data/collardata.RData")

#-------------------------------------------------------------------------------
##----User interface

ui <- navbarPage(title = "Boreal Lynx Project Collar Viewer",
                 id = "nav",
                 tabPanel('Input Data', div(class = "pg1",
                                            fluidPage(
                                              fluidRow(
                                                column(width=3,
                                                       htmlOutput("dataInfo"),
                                                       hr(),
                                                       h4("STEP 1. Make your selections:"),
                                                       selectInput(inputId = "si.site",
                                                                   label = "Study site:",
                                                                   choices = c("Tetlin" = "TET",
                                                                               "Koyukuk/Nowitna/Innoko" = "KOY",
                                                                               "Yukon Flats" = "YKF",
                                                                               "Kanuti" = "KAN",
                                                                               "Wiseman" = "WIS"),
                                                                   multiple = TRUE,
                                                                   selectize = TRUE),
                                                       selectInput(inputId = 'si.id', 
                                                                   label = 'CTN:', 
                                                                   choices = unique(dat.sub$id), 
                                                                   multiple = TRUE,
                                                                   selectize = TRUE),
                                                       selectInput(inputId = "si.sex",
                                                                   label = "Sex:",
                                                                   choices = c("Male" = "M",
                                                                               "Female" = "F"),
                                                                   multiple = TRUE,
                                                                   selectize = TRUE),
                                                       selectInput(inputId = "si.age",
                                                                   label = "Age class:",
                                                                   choices = c("Adult" = "A",
                                                                               "Kitten" = "K"),
                                                                   multiple = TRUE,
                                                                   selectize = TRUE),
                                                       dateRangeInput(inputId = "dr.dates",
                                                                      label = "Date Range:",
                                                                      start = "2018-01-01",
                                                                      format = "mm/dd/yyyy",
                                                                      sep = "/"),
                                                       hr(),
                                                       h4("STEP 2. View home ranges and movements:"),
                                                       actionButton("ac_UseData", 
                                                                    label = "Go",class = "btn-success fltr-btn")
                                                       # downloadButton(outputId = "downloadData", 
                                                       #                label = "Download")
                                                ),
                                                
                                                column(width=9,
                                                       tabsetPanel(type = "tabs",
                                                                   tabPanel("Map",
                                                                            leafletOutput("preview", height = "800px")),
                                                                   tabPanel("Summary Table", DT::dataTableOutput(outputId = "dt.animals"))))
                                              )
                                            ))
                 ),
                 
                 #--------------------------------------------------------------
                 # HOME RANGE ANALYSES
                 tabPanel("Home Ranges", id = "spatial",
                          div(class = "outer", tags$head(includeCSS("style.css")),
                              tags$head(includeCSS("style.css")),
                              leafletOutput("map", width = "100%", height = "100%"),
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                                            width = 330, top = "110", bottom = "auto",
                                            left = 10, right = "auto",
                                            h2("Spatial Analysis"),
                                            p("Select inputs"),
                                            selectizeInput("sl_HomeRange", "Home Range Estimation Method",
                                                           choices = c('Display Points', "Minimum Convex Polygon", "Kernel Density", "Brownian Bridge"),
                                                           selected = 'Display Points'),
                                            textInput('tx_Contour', 'Contour Percentages', placeholder = '95', value = '95'),
                                            actionButton("ac_UpdateMap", "Update Map")
                                            #downloadButton('dl_Shape', 'Download Polygon')
                              )
                          )
                 ),
                 
                 #--------------------------------------------------------------
                 # MOVEMENT ANALYSIS
                 tabPanel("Movement",
                          fluidRow(column(3,
                                          selectInput('fig.type', 'Figure Type',
                                                      choices = c('point', 'line', 'histogram'),
                                                      selected = 'line'),
                                          selectInput("y.input", "Y Axis",
                                                      choices = c("Distance", "NSD", "sigDist", "Speed", "dTime"),
                                                      selected = 'NSD')
                          ),
                          column(3, br(), br(), br(), br(), br(),
                                 actionButton('ac_RunAnalysis', 'Create Graphs', class = 'btn-primary'))
                          ),
                          fluidRow(
                            plotOutput('move.plot', width = '100%', height = 600)
                          ),
                          hr(),
                          fluidRow(
                            column(3,
                                   selectizeInput('slz_nsdID', 'Select CTN', choices = dat$id, multiple = TRUE)),
                            column(4,
                                   p('Choose animals to display in the figure below. Multiple selections are supported.
                                     Net Squared Displacement is a metric used to classify movement strategies. The
                                     are transformed to be between 0 and 1.'))
                                   ),
                          fluidRow(
                            highcharter::highchartOutput('nsdTimeSeries')
                          )
                                   ),
                 
                 #--------------------------------------------------------------
                 # REPORT
                 # tabPanel("Reports",
                 #          fluidPage(
                 #            fluidRow(
                 #              column(width=3,
                 #                     h4("Make your selection for your report"),
                 #                     hr(),
                 #                     selectInput(inputId = "si.site",
                 #                                 label = "Study site:",
                 #                                 choices = c("TET" = "TET",
                 #                                             "KNI Refuge" = "KOY",
                 #                                             "Yukon Flats Refuge" = "YKF",
                 #                                             "Kanuti Refuge" = "KAN",
                 #                                             "Wiseman" = "WIS"),
                 #                                 multiple = TRUE,
                 #                                 selectize = TRUE),
                 #                     dateRangeInput(inputId = "dr.dates",
                 #                                    label = "Date Range:",
                 #                                    start = "2014-08-01",
                 #                                    format = "mm/dd/yyyy",
                 #                                    sep = "/"),
                 #                     hr(),
                 #                     downloadButton("report", "Generate a report")
                 #              )
                 #            ))
                 # ),
                 
                 # ABOUT PAGE
                 tabPanel("About",
                          h3("Exploratory Analysis and Visualization of Lynx Space Use"),
                          hr(),
                          h4("Version: 0.2")
                 )
                 
)






#-------------------------------------------------------------------------------
##----Server

server <- function(input, output, session) {
  
  #-----------------------------------------------------------------------------
  ## PAGE 1 - MAP
  
  # Subset by site, CTN, date and month:
  dat.sub <- reactive({          # Subsets the data based on user inputs
    
    df <- dat %>% filter(date > input$dr.dates[1] & date < input$dr.dates[2])
    
    if (!(is.null(input$si.site) | '' %in% input$si_site)) {  # By site
      df <- dat %>% filter(Site == input$si.site)
    }
    if (!(is.null(input$si.sex) | '' %in% input$si.sex)) {  # By sex
      df <- dat %>% filter(Sex == input$si.sex)
    }
    if (!(is.null(input$si.age) | '' %in% input$si.age)) {  # By age
      df <- dat %>% filter(AgeClass == input$si.age)
    }
    if (!(is.null(input$si.id) | '' %in% input$si.id)) {  # By CTN
      df <- dat %>% filter(id == input$si.id)
    }
    
    return(df)
  })
  
  # Subset data for summary table:
  dat.sub.tbl <- reactive({
    dat.sub() %>% 
      group_by("Study site" = Site,
               "CTN" = id, 
               "AgeClass" = AgeClass,
               "Sex" = Sex) %>%  # Creates a summary table of the data
      summarise(
        "Capture date" = as.Date(first(datecapture)),
        "Fix schedule" = last(fixsched),
        "Current fix rate" = last(fixrate),
        "First fix" = min(date),
        "Last fix" = max(date)
      )
  })
  
  # Displays a summary table of the data:
  output$dt.animals <- DT::renderDataTable({        
    DT::datatable(dat.sub.tbl(), options = list(pageLength = 10))
  })
  
  # Create a map of the subsetted data:
  output$preview <- renderLeaflet({
    CollarMap(dat.sub())
  })
  
  # Download the subsetted data as a .csv:
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("CollarData_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(x = dat.sub(), file, row.names = FALSE)
    }
  )
  
  # Count of NAs (lat, long) for error rate
  dat.na <- reactive({
    return(dat.sub() %>% filter(is.na(lon) | is.na(lat)) %>% 
             nrow() %>% 
             as.numeric())
  })
  
  # Output info for animals selected in map
  output$dataInfo <- renderUI({
    HTML(
      paste(sep = "<br/>",
            paste("<b>Total Animals:</b> ", length(unique(dat.sub()$id))),
            paste("<b>Total Fixes:</b> ", nrow(dat.sub())),
            # paste("<b>Error Rate:</b> ",
            #       round(100 * (dat.na() / nrow(dat.sub())), 4), '%'),
            paste("<b>Min. Date:</b> ", date(min(dat.sub()$date))),
            paste("<b>Max. Date:</b> ", date(max(dat.sub()$date)))
      ))
  })
  
  # Change tab to "Spatial"
  observeEvent(input$ac_UseData, {
    # shinyjs::logjs('use data button pushed')
    updateNavbarPage(session, "nav", "Home Ranges")
  })
  
  #-----------------------------------------------------------------------------
  ## PAGE 2 - HOME RANGES
  
  ## Create dataframe of movement parameters for analysis
  dat.move <- eventReactive(input$ac_UpdateMap, {
    df <- xyConv(dat.sub())
    move <- df %>%
      group_by(id) %>%
      mutate(Distance = moveDist(x, y),
             sigDist = cumsum(Distance),
             NSD = moveNSD(x, y),
             dTime = moveDt(fixtime),
             Speed = moveSpeed(Distance, dTime),
             Year = year(fixtime),
             Month = month(fixtime),
             Day = day(fixtime),
             Hour = hour(fixtime)) %>%
      ungroup()
    return(move)
  })
  
  ## countour list for home range contours
  pct_contour <- reactive({
    return(as.numeric(strsplit(input$tx_Contour, ', ')[[1]]))
  })
  
  ## home range estimation
  hr_ud <- eventReactive(input$ac_UpdateMap, {
    df <- as.data.frame(dat.move())
    if (input$sl_HomeRange == 'Minimum Convex Polygon') {
      spdf <- SpatialPointsDataFrame(coordinates(cbind(df$x, df$y)),
                                     data = df, proj4string = CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
      cp <- mcp(spdf[,"id"], percent = 99)
      cp <- spTransform(cp, CRS('+proj=longlat'))
      ids <- cp$id
      hr <- vector("list", length(cp$id))
      for (i in seq_along(ids)) {
        poly <- cp[cp$id == ids[i], ]
        poly <- geojson_json(poly)
        hr[[i]] <- poly
      }
      names(hr) <- ids
    } else if (input$sl_HomeRange == 'Kernel Density') {
      kd <- SpatialPointsDataFrame(coordinates(cbind(df$lon, df$lat)), data = df,
                                   proj4string = CRS('+proj=longlat'))
      kd <- kernelUD(kd[, "id"])
      hr <- lapply(kd, function(x) getContours(x, pct_contour()))
    } else if (input$sl_HomeRange == 'Brownian Bridge') {
      bb <- to_ltraj(df)
      bb <- estimate_bbmm(bb)
      bb <- bbBugFix(bb)
      hr <- lapply(bb, function(x) getContours(x, pct_contour()))
      for (i in seq_along(bb)) {
        hr[[i]]@proj4string <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
        hr[[i]] <- spTransform(hr[[i]], CRS('+proj=longlat'))
      }
      
      ## spdf to geojson, wrapping this in an event reactive
      # hr <- lapply(hr, function(x) geojson_json(x))
    }
    
    return(hr)
  })
  
  ## BASEMAP
  lfMap <- eventReactive(input$ac_UpdateMap, {
    hr <- hr_ud()
    if (input$sl_HomeRange == 'Brownian Bridge' | input$sl_HomeRange == 'Kernel Density') {
      hr <- lapply(hr, function(x) geojson_json(x))
    }
    
    lflt <- leaflet() %>% addProviderTiles('Esri.WorldTopoMap',
                                           options = providerTileOptions(attribution = NA))
    
    if (input$sl_HomeRange == 'Display Points') {
      lflt <- lflt %>% mapPoints(dat.move())
    } else {
      shinyjs::logjs(paste(sep = ' - ', 'homerange', input$sl_HomeRange, dput(hr)))
      lflt <- lflt %>% mapPolygons(hr) %>% mapPoints(dat.move())
    }
  })
  
  # MAP OUTPUT
  output$map <- renderLeaflet({
    shinyjs::logjs(paste(dput(lfMap())))
    lfMap()
  })
  
  # SHAPEFILE OUTPUT
  output$dl_Shape <- downloadHandler(
    filename = function() 'UtlzDist_Export.zip',
    content = function(file) {
      if (length(Sys.glob('UtlzDist_shp.*')) > 0){
        file.remove(Sys.glob('UtlzDist_shp.*'))
      }
      shpOut <- correctIDs(hr_ud())
      shpOut <- Reduce(rbind, shpOut)
      writePolyShape(shpOut, 'UtlzDist_shp')
      zip(zipfile = 'UtlzDist_Export.zip', files = Sys.glob('UtlzDist_shp.*'))
      file.copy('UtlzDist_Export.zip', file)
      if(length(Sys.glob('UtlzDist_shp.*')) > 0){
        file.remove(Sys.glob('UtlzDist_shp.*'))
      }
    },
    contentType = 'application/zip'
  )
  
  # HIDE POLYGON OUTPUT IF MCP IS USED
  observeEvent(input$sl_HomeRange, {
    if (input$sl_HomeRange == 'Brownian Bridge' | input$sl_HomeRange == 'Kernel Density') {
      shinyjs::show('dl_Shape')
    } else {
      shinyjs::hide('dl_Shape')
    }
  })
  
  # SPATIAL DATA OUTPUT
  output$downloadData <- downloadHandler(
    filename = function() {paste("CollarData", ".", sep = "")},
    content = function(file) {
      write.csv(dat.move(), file)
    }
  )
  
  #-----------------------------------------------------------------------------
  ## PAGE 3 - MOVEMENT ANALYSES
  
  move_plots <- eventReactive(input$ac_RunAnalysis, {
    p <- movement_eda(dat.move(), plot_var = input$y.input, type = input$fig.type)
    return(p)
  })
  output$move.plot <- renderPlot({
    move_plots()
  })
  
  observeEvent(input$slz_id, {
    ids <- input$slz_id
    updateSelectizeInput(session, 'slz_nsdID', choices = sort(ids), selected = ids[1])
  })
  
  output$nsdTimeSeries <- highcharter::renderHighchart({
    df <- dat.move() %>%
      mutate(ts = as_date(fixtime)) %>%
      arrange(id, ts) %>%
      group_by(id, ts) %>%
      slice(1) %>%
      ungroup()
    
    ids <- input$slz_nsdID
    hc <- highchart()
    for(i in seq_along(ids)) {
      d <- df %>% filter(id == ids[i])
      hc <- hc_add_series_times_values(hc, dates = d$ts, values = d$NSD,
                                       color = color_pal[i], name = ids[i]) %>%
        hc_yAxis(max = 1)
    }
    hc
  })
  
  #-----------------------------------------------------------------------------
  ## PAGE 4 - MARKDOWN REPORT (in progress..)
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copies the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}



shinyApp(ui, server)