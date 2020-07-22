#' @title Collar viewer
#' 
#' @description  An R shiny app for visualizing and summarizing GPS collar data.
#' @author McCrea Cobb <mccrea_cobb@fws.gov>
#' 
#' @example shiny::runGitHub( "collar-viewer", "USFWS", launch.browser=T)


packages <- c("shiny", "shinyjs", "tidyverse", "leaflet", "shinyWidgets", "DT", "sp", 
              "rgdal","adehabitatLT", "adehabitatHR", "lubridate", "geojsonio",
              "maptools")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(shiny)
library(shinyjs)
library(tidyverse)
library(leaflet)
library(shinyWidgets)
library(DT)
library(sp)
library(rgdal)
library(adehabitatLT)
library(adehabitatHR)
library(lubridate)
library(geojsonio)
library(maptools)
library(dplyr)

# Source files:
source("code/functions/global.R")

# Define UI for application that draws a histogram
ui <- navbarPage("CollarViewer v.0.2.0", id="nav",
                 
                 # Tab 1: Load data
                 tabPanel("Load data",
                          # Sidebar layout
                          fluidPage(
                            fileInput(inputId = "file", 
                                      label = "Select collar data:",
                                      multiple = FALSE,
                                      accept = ".RData")), # used to get the file upload control option
                          
                          DT::DTOutput("sum.tbl")
                 ),
                 
                 # Tab 2: Interactive map
                 tabPanel("Interactive map",
                          div(class = "outer",
                              
                              tags$head(includeCSS("css/style.css")),
                              
                              leafletOutput("map", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", 
                                            class = "panel panel-default", 
                                            fixed = TRUE,
                                            draggable = TRUE, 
                                            top = 60, 
                                            left = 20, 
                                            right = "auto", 
                                            bottom = "auto",
                                            width = "20%", 
                                            height = "auto",
                                            
                                            htmlOutput("dataInfo"),  # Adds info about the # of fixes, first/last fix, etc
                                            
                                            selectizeGroupUI(
                                              id = "my-filters",
                                              inline = FALSE,
                                              params = list(
                                                site = list(inputId = "site", title = "Site:", placeholder = 'select'),
                                                sex = list(inputId = "sex", title = "Sex:", placeholder = 'select'),
                                                age = list(inputId = "age", title = "Age class:", placeholder = 'select'),
                                                id = list(inputId = "ctn", title = "CTN:", placeholder = 'select')
                                              )
                                            )
                              )
                          )
                 ),
                 
                 # Tab 3: Home Ranges
                 tabPanel("Home Ranges",
                          div(class = "outer",
                              
                              tags$head(includeCSS("css/style.css")),
                              
                              leafletOutput("map_hrs", width = "100%", height = "100%"),
                              
                              absolutePanel(id = "controls",
                                            class = "panel panel-default", 
                                            fixed = TRUE,
                                            draggable = TRUE, 
                                            top = 60, 
                                            left = 20, 
                                            right = "auto", 
                                            bottom = "auto",
                                            width = "20%", 
                                            height = "auto",
                                            
                                            HTML("<br>"),
                                            
                                            selectizeInput("sl_HomeRange", "Make a selection:",
                                                           choices = c('Display Points', "Minimum Convex Polygon", "Kernel Density", "Brownian Bridge"),
                                                           selected = 'Display Points'),
                                            textInput('tx_Contour', 'Contour Percentages:', placeholder = '50, 95', value = '50, 95'),
                                            actionButton("ac_UpdateMap", "Update Map"),
                              )
                          )
                 )
)


#-------------------------------------------------------------------------------



# Define server logic 
server <- function(input, output, session) {
  
  #----
  ## Tab 1: Load data
  
  # Define the uploaded Rdata file
  dat <- reactive({
    req(input$file)
    if (is.null(input$file)) 
      return(NULL)
    inFile <- input$file
    file <- inFile$datapath
    # Load the file into new environment and get it from there
    e = new.env()
    name <- load(file, envir = e)
    dat <- e[[name]]
    dat <- dat %>%
      filter(!is.na(lat))
    dat
  })
  
  # Subset data for summary table:
  dat.tbl <- reactive({
    req(input$file)
    dat() %>% 
      group_by("CTN" = ctn, 
               "Study site" = site,
               "Age class" = age,
               "Sex" = sex) %>%  # Creates a summary table of the data
      summarise(
        "Capture date" = as.Date(first(capture_date)),
        "Fix schedule" = last(fixsched),
        "Current fix rate" = last(fixrate),
        "First fix" = min(as.Date(fixtime)),
        "Last fix" = max(as.Date(fixtime)))
  })
  
  # Displays a summary table of the data:
  output$sum.tbl <- DT::renderDT(dat.tbl(), options = list(pageLength = 20))
  
  # dat.map <- dat %>%
  #   select(c(id, fixtime, lat, lon, sex, age, )
  
  
  #----
  ## Tab 2: Map
  
  # Subset the data based on user input:
  dat.sub <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = dat,
    vars = c("site", "sex", "age", "ctn")
  )
  
  # Create a map of the subsetted data:
  output$map <- renderLeaflet({
    collar_map(dat.sub())
  })
  
  # Output info for animals selected in map
  output$dataInfo <- renderUI({
    HTML(
      paste(sep = "<br/>",
            paste("<br>"),
            paste("<b>Total Collars:</b> ", length(unique(dat.sub()$ctn))),
            paste("<b>Total Fixes:</b> ", nrow(dat.sub())),
            paste("<b>Min. Date:</b> ", as.Date(min(dat.sub()$fixtime))),
            paste("<b>Max. Date:</b> ", as.Date(max(dat.sub()$fixtime))),
            paste("<br>")
      ))
  })
  
  #----
  ## Tab 3: Home Ranges
  
  dat.move <- eventReactive(input$ac_UpdateMap, {
    ## Creates a dataframe of movement parameters for analysis
    
    df <- xy_conv(dat.sub())  # Adds projected x and y coordinates to the df used for distance and speed calculations 
    
    move <- df %>%
      arrange(fixtime) %>%
      group_by(id) %>%
      mutate(Distance = move_dist(x, y),  # sourced from global.R
             sigDist = cumsum(Distance),
             NSD = move_nsd(x, y), # sourced from global.R
             dTime = move_dt(fixtime),  # sourced from global.R
             Speed = move_speed(Distance, dTime),  # sourced from global.R
             Year = lubridate::year(fixtime),
             Month = lubridate::month(fixtime),
             Day = lubridate::day(fixtime),
             Hour = lubridate::hour(fixtime)) %>%
      ungroup()
    return(move)
  })
  
  ## Create a comma seperated list of values for the home range contours based on user input
  pct_contour <- reactive({
    return(as.numeric(strsplit(input$tx_Contour, ', ')[[1]]))
  })
  
  ## Home range estimation
  hr_ud <- eventReactive(input$ac_UpdateMap, {
    df <- as.data.frame(dat.move())
    if (input$sl_HomeRange == 'Minimum Convex Polygon') {
      # If user selects Minimum Convex Polygon home range
      
      spdf <- sp::SpatialPointsDataFrame(coordinates(cbind(df$x, df$y)),
                                         data = df, proj4string = CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
      cp <- adehabitatHR::mcp(spdf[,"id"], percent = 99)
      cp <- sp::spTransform(cp, CRS('+proj=longlat'))
      ids <- cp$id
      hr <- vector("list", length(cp$id))
      for (i in seq_along(ids)) {
        poly <- cp[cp$id == ids[i], ]
        poly <- geojson_json(poly)
        hr[[i]] <- poly
      }
      names(hr) <- ids
      
    } else if (input$sl_HomeRange == 'Kernel Density') {
      # If user selects Kernel home range
      
      kd <- sp::SpatialPointsDataFrame(coordinates(cbind(df$lon, df$lat)), data = df,
                                       proj4string = CRS('+proj=longlat'))
      kd <- adehabitatHR::kernelUD(kd[, "id"])
      hr <- lapply(kd, function(x) get_contours(x, pct_contour()))  # get_contours() sourced from global.R
      
    } else if (input$sl_HomeRange == 'Brownian Bridge') {
      # If user selects Brownian Bridge home range
      
      bb <- to_ltraj(df)  # sourced from global.R
      bb <- estimate_bbmm(bb)  # sourced from global.R
      #bb <- bb_fix(bb)  # sourced from global.R
      hr <- lapply(bb, function(x) get_contours(x, pct_contour()))  # get_contours() sourced from global.R
      for (i in seq_along(bb)) {
        hr[[i]]@proj4string <- sp::CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
        hr[[i]] <- sp::spTransform(hr[[i]], CRS('+proj=longlat'))
      }
    }
    
    return(hr)
  })
  
  
  lfMap <- eventReactive(input$ac_UpdateMap, {
    # Create map of home ranges
    
    hr <- hr_ud()
    if (input$sl_HomeRange == 'Brownian Bridge' | input$sl_HomeRange == 'Kernel Density') {
      hr <- lapply(hr, function(x) geojson_json(x))
    }
    
    lflt <- leaflet() %>% addProviderTiles('Esri.WorldTopoMap',
                                           options = providerTileOptions(attribution = NA)) %>%
      addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers")
    
    if (input$sl_HomeRange == 'Display Points') {
      lflt <- lflt %>% 
        map_pts(dat.move())
    } else {
      shinyjs::logjs(paste(sep = ' - ', 'homerange', input$sl_HomeRange, dput(hr)))
      lflt <- lflt %>% 
        map_polygons(hr) %>% 
        map_pts(dat.move())
    }
  })
  
  # Map Output
  output$map_hrs <- renderLeaflet({
    shinyjs::logjs(paste(dput(lfMap())))
    lfMap()
  })
  
  # Hide Polygon output if MCP is selected
  observeEvent(input$sl_HomeRange, {
    if (input$sl_HomeRange == 'Brownian Bridge' | input$sl_HomeRange == 'Kernel Density') {
      shinyjs::show('dl_Shape')
    } else {
      shinyjs::hide('dl_Shape')
    }
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
