################################################################################
# Functions Used in the Shiny App for Visualizing Lynx GPS Collars
#
# Author: McCrea Cobb <mccrea_cobb@fws.gov>
# Date created: 9/7/2018
################################################################################


#-------------------------------------------------------------------------------
## AESTHETICS

# color palatte used in the application
color_pal <- c("#3366CC", "#DC3912", "#FF9900", "#109618", "#990099", "#0099C6",
               "#DD4477", "#66AA00", "#B82E2E", "#316395", "#994499", "#22AA99",
               "#AAAA11", "#6633CC", "#E67300", "#8B0707", "#651067", "#329262",
               "#5574A6", "#3B3EAC")

#-------------------------------------------------------------------------------
## MAPPING FUNCTIONS

# Map collar data on page one. One point per day for lines, First, and Last point
CollarMap <- function(dataframe) {
  df <- dataframe %>%
    filter(!(is.na(lon) | is.na(lat))) %>%
    arrange(id, fixtime) %>%
    group_by(id, as_date(fixtime)) %>%
    slice(1) %>%
    ungroup()
  
  ids <- unique(df$id)
  pal <- rep_len(color_pal, length(ids))

  map <- leaflet() %>%
    addProviderTiles("Esri.WorldTopoMap", options = providerTileOptions(attribution = NA)) %>%
    addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers")
  for (i in seq_along(ids)) {
    d <- df %>% filter(id == ids[i])
    dp <- d[c(1, nrow(d)), ]
    map <- addPolylines(map, lng = d$lon, lat = d$lat,
                        weight = 1,
                        color = pal[i],
                        opacity = .4)
    map <- addCircleMarkers(map, lng = dp$lon, lat = dp$lat,
                            stroke = FALSE,
                            radius = 4,
                            color = pal[i],
                            fillOpacity = 1,
                            popup = paste(sep = "<br>",
                                          paste("<b>Collar ID:<b>", ids[i]),
                                          paste("<b>Study site:<b>", d$Site),
                                          paste("<b>Last fix:<b> ", d$fixtime)))
  }
  return(map)
}

# Map points and lines on leaflet map (Spatial Tab)
mapPoints <- function(map, df) {
  ids <- unique(df$id)
  pal <- rep_len(color_pal, length(ids))
  layers <- list()

  for(i in seq_along(ids)) {
    dat <- df %>% filter(id == ids[i])
    map <- addPolylines(map, lng = dat$lon, lat = dat$lat,
                        group = as.character(ids[i]),
                        color = pal[i], weight = 1)
    map <- addCircleMarkers(map, lng = dat$lon, lat = dat$lat,
                            group = as.character(ids[i]), color = pal[i],
                            radius = 3, stroke = FALSE, fillOpacity = .3,
                            popup = paste(sep = "<br>",
                                          paste("<b>Collar ID:</b> ", ids[i]),
                                          paste("<b>Date:</b> ", dat$fixtime)))
    layers <- c(layers, as.character(ids[i]))
  }
  map <- addLayersControl(map, overlayGroups = layers)
  return(map)
}

# map polygons on leaflet map
mapPolygons <- function(map, geojson) {
  pal <- rep_len(color_pal, length(geojson))
  for (i in seq_along(geojson)) {
    map <- addGeoJSON(map, geojson[[i]], color = pal[i],
                      weight = 1, group = names(geojson)[i])
  }
  return(map)
}

# Calculates net squared displacement:
Calculate_NSD <- function(dat) {
  dat <- as.data.frame(dat)
  #dat$fixtime <- as.character(dat$fixtime)
  #dat$fixtime <- as.POSIXlt.character(dat$fixtime,
  #                                      format = ("%m/%d/%Y %H:%M:%S %p"))
  dat <- dat[complete.cases(dat[, c("lon", "lat")]), ]
  geocoord <- sp::SpatialPoints(cbind(as.numeric(dat$lon),
                                      as.numeric(dat$lat)),
                                proj4string = sp::CRS("+proj=longlat"))
  utmcoord <- as.data.frame(sp::spTransform(geocoord, sp::CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")))
  colnames(utmcoord) <- c("Easting", "Northing")
  dat <- cbind(dat, utmcoord)
  unq_id <- unique(dat$id)
  df <- data.frame()
  for (i in unq_id) {
    x <- dat[dat$id == i, ]
    x$NSD <- (x$Easting - x$Easting[1])**2 + (x$Northing - x$Northing[1])**2
    df <- rbind(df, x)
  }
  return(df)
}

# Plots net squared displacement:
Plot_NSD <- function(dataframe) {
  p <- ggplot(dataframe, aes(x = date, y = NSD, group = id)) +
    geom_line(color = 'firebrick4', size = .75) +
    facet_wrap(~id) +
    labs(y = 'Net Squared Displacement') +
    theme(panel.background = element_rect(fill = 'white'),
          plot.background = element_rect(fill = 'white'),
          panel.grid.major.x = element_line(color = 'grey75', size = 1, linetype = 'dotted'),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = 'grey75', size = 1, linetype = 'dotted'),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(color = 'grey50', size = 14),
          axis.text.x = element_text(color = 'grey50', size = 10),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(color = 'grey50', size = 12))
  return(p)
}

DeviceMapping <- function(dataframe, basemap = "Esri.WorldTopoMap") {
  dat <- as.data.table(dataframe)
  dat <- dat[complete.cases(dat[, .(lon, lat)])]
  unq_id <- unique(dat$id)
  pal <- rep_len(color_pal, length(unq_id))

  device.map <- leaflet() %>%
    addProviderTiles(basemap, options = providerTileOptions(attribution = NA))
  layer.group <- list()

  for(i in 1:length(unq_id)) {
    df <- dat[id == unq_id[i]]
    device.map <- addPolylines(device.map,
                               lng = df$lon, lat = df$lat,
                               group = as.character(unq_id[i]),
                               color = pal[i],
                               weight = 1
    )
    #df <- df[, .SD[c(seq(1, .N, 5), .N)]]
    device.map <- addCircleMarkers(device.map,
                                   lng = df$lon, lat = df$lat,
                                   group = as.character(unq_id[i]),
                                   radius = 4,
                                   stroke = FALSE,
                                   fillOpacity = .3,
                                   color = pal[i],
                                   popup = paste(sep = "<br>",
                                                 paste("<b>CTN:</b> ", unq_id[i]),
                                                 paste("<b>Date:</b> ", df$fixtime))
    )
    layer.group <- c(layer.group, as.character(unq_id[i]))
  }
  device.map <- addLayersControl(device.map, overlayGroups = layer.group)
  return(device.map)
}

DeviceMapping_geojson <- function(device.map, geojson) {
  pal <- rep_len(color_pal, length(geojson))
  for (i in seq_along(geojson)) {
    device.map <- addGeoJSON(device.map, geojson[[i]], color = pal[i],
                             weight = 1, group = names(geojson)[i])
  }
  return(device.map)
}

to_ltraj <- function(dat) {
  dat <- as.data.frame(dat)
  dat$fixtime <- as.POSIXct(dat$fixtime, format = '%Y-%m-%d %H:%M:%S')
  dat <- dat[complete.cases(dat[, c("x", "y", "fixtime")]), ]
   # coord_conv <- SpatialPoints(cbind(as.numeric(dat$lon),
   #                                   as.numeric(dat$lat)),
   #                             proj4string = CRS("+proj=longlat"))
   # coord_conv <- as.data.frame(spTransform(coord_conv, CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")))
   # colnames(coord_conv) <- c("Easting", "Northing")
   # dat <- cbind(dat, coord_conv)

  traj <- as.ltraj(dat[, c("x", "y")], date = dat$fixtime, id = dat$id)
  return(traj)
}

# Fucntion to get contours from and plot Brownian Bridge HR:
estimate_bbmm <- function(traj) {
  sig1 <- liker(traj, sig2 = 40, rangesig1 = c(0, 10), plotit = FALSE)
  bb <- kernelbb(traj, sig1[[1]]$sig1, 40, grid = 100)
  return(bb)
}

get_ud <- function(ud, pct_contour) {
  ud_list <- list(length(pct_contour))
  print('----- Entering contour loop')
  print(paste('-----', pct_contour))
  for (i in seq_along(pct_contour)) {
    print(paste('-----', pct_contour[[i]]))
    ctr <- getverticeshr(x = ud, percent = pct_contour[[i]])
    ctr@proj4string <- CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    ctr <- spTransform(ctr, CRS('+proj=longlat'))
    ctr <- geojson_list(ctr)
    ud_list[[i]] <- ctr
  }
  geojson <- geojson_json(Reduce(`+`, ud_list))
  return(geojson)
}

# Function to get contours from kernel density estimates
getContours <- function(ud, pct) {
  ids <- as.character(pct)
  x <- getvolumeUD(ud)
  xyma <- coordinates(x)
  xyl <- list(x = unique(xyma[, 1]), y = unique(xyma[, 2]))
  z <- as.image.SpatialGridDataFrame(x[, 1])$z
  
  cl <- lapply(pct, function(x) {
    contourLines(x = xyl$x, y = xyl$y, z, nlevels = 1, levels = x)
  })
  
  plys <- lapply(seq_along(cl), function(i) {
    Polygons(lapply(seq_along(cl[[i]]), function(j) {
      m <- cl[[i]][[j]]
      ply <- cbind(m$x, m$y)
      ply <- rbind(ply, ply[1, ])
      Polygon(ply)
    }), ID = ids[i])
  })
  
  plys <- lapply(plys, function(x) checkPolygonsHoles(x))
  splys <- SpatialPolygons(plys)
  dff <- data.frame(id = ids)
  row.names(dff) <- ids
  spdf <- SpatialPolygonsDataFrame(splys, dff)
  return(spdf)
}
get_mud <- function(ud, pct_contour) {
  gjm_list <- list()
  print(paste('n elements', length(ud)))
  print('entering for loop')
  for (i in 1:length(ud)) {
    print(paste('before ud', i, 'CTN', names(ud)[i]))
    gj <- get_ud(ud[[i]], pct_contour)
    print(paste('before list assignment', i))
    gjm_list[[i]] <- gj
  }
  print('exit loop')
  names(gjm_list) <- names(ud)
  return(gjm_list)
}

#-------------------------------------------------------------------------------
## MOVEMENT ANALYSIS FUNCTIONS

# Reproject points
xyConv <- function(df, xy = c('lon', 'lat'), CRSin = '+proj=longlat',
                   CRSout = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") { # Alaska Albers
  df <- df[complete.cases(df[, xy]), ]
  conv <- SpatialPoints(coordinates(cbind('x' = df[, xy[1]],
                                          'y' = df[, xy[2]])),
                        proj4string = CRS(CRSin))
  conv <- spTransform(conv, CRS(CRSout))
  conv <- data.frame(conv)
  colnames(conv) <- c('x', 'y')
  df <- cbind(df, conv)

  return(df)
}

# Calculate movement distance:
moveDist <- function(x, y) {
  dist <- c(0, sqrt((x[-1] - x[-length(x)])**2 +
                      (y[-1] - y[-length(y)])**2))
  return(dist) # same unit as input (meters)
}
# Calculates net square displacement:
moveNSD <- function(x, y) {
  r2n <- (x - x[1])**2 + (y - y[1])**2
  r2n <- (r2n - min(r2n)) / (max(r2n) - min(r2n))
  return(r2n)
}
# Calculates ??
moveDt <- function(time) {
  dt <- c(0, unclass(time[-1]) - unclass(time[-length(time)]))
  return(dt / 3600) # seconds
}
# Calculates movement speed
moveSpeed <- function(dist, time) {
  speed <- (dist / 1000) / (time)
  speed[is.nan(speed)] <- 0
  return(speed)
}

# Create plots of movement statistics
movement_eda <- function(dat, plot_var, type = 'line') {
  pal <- rep_len(color_pal, length(unique(dat$id)))

  p <- ggplot(dat, aes(group = id, color = factor(id), fill = factor(id)))
  if(type == 'histogram'){
    p <- p + geom_histogram(aes_string(x = plot_var))
  } else if (type == 'line'){
    p <- p + geom_line(aes_string(x = 'fixtime', y = plot_var), size = .75)
  } else if (type == 'point'){
    p <- p + geom_point(aes_string(x = 'fixtime', y = plot_var), size = 1.5)
  }
  p <- p + facet_wrap(~id, scales = 'free') +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    theme(panel.background = element_rect(fill = 'white'),
          plot.background = element_rect(fill = 'white'),
          panel.grid.major.x = element_line(color = 'grey90', size = .5),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = 'grey90', size = .5),
          panel.grid.minor.y = element_blank(),
          legend.position = 'none',
          axis.title.x = element_blank(),
          axis.title.y = element_text(color = 'grey50', size = 14),
          axis.text.x = element_text(color = 'grey50', size = 10),
          axis.text.y = element_text(color = 'grey50', size = 10),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(color = 'grey50', size = 12))
  return(p)
}


# rewrites spatial dataframe names, input must be a list. Facilitates merging into a single spatial dataframe
correctIDs <- function(contour) {
  nm <- names(contour)
  for(i in seq_along(contour)) {
    row.names(contour[[i]]) <- paste(nm[i], row.names(contour[[i]]))
    print(row.names(contour[[i]]))
  }
  return(contour)
}

# A fix for the Brownian bridge calculations:
bbBugFix <- function(bb) {
  if (class(bb) == 'estUDm') {
    v <- bb
  } else {
    v <- list(bb)
  }
  return(v)
}

