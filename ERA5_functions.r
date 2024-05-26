library(move2)
library(sf)
library(lubridate)
library(ecmwfr)
library(terra)
library(dplyr)
library(plyr)
library(doMC)
library(data.table)

# Function to define the area for CDS API
area2request <- function(x, ext = 0.01) {
  #area: [north, west, south, east]
  (bounds <- st_bbox(x))
  x_range <- bounds$xmax - bounds$xmin
  y_range <- bounds$ymax - bounds$ymin
  west <- floor(bounds$xmin - ext * x_range)
  if(west < -180){west <- -180}
  east <- ceiling(bounds$xmax + ext * x_range)
  if(east > 180){east <- 180}
  south <- floor(bounds$ymin - ext * y_range)
  if(south < -90){south <- -90}
  north <- ceiling(bounds$ymax + ext * y_range)
  if(north > 90){north <- 90}
  return(paste(north, west, south, east, sep="/"))
}

# Function to generate request table from date range
date2request <- function(x) {
  DateTime <- sort(unique(c(floor_date(x$timestamp, "hour"), floor_date(x$timestamp+3600, "hour"))))
  reqTable <- data.frame(year=year(DateTime), month=sprintf("%02d", month(DateTime)), day=sprintf("%02d", day(DateTime)), hour=sprintf("%02d",hour(DateTime)))
  #split the table into a list with all the observations with the same year, month, day
  reqTable <- split(reqTable, apply(reqTable[,1:3], 1, paste0, collapse=""))
  #collapse to one line per day
  reqTable <- do.call(rbind, lapply(reqTable, function(x) cbind(x[1,1:3], hours= paste0(x[,4], collapse=" "))))
  return(reqTable)
}

# Function to create a list of requests
create_request_list <- function(reqTable, areaS) {
  reqList <- lapply(seq_len(nrow(reqTable)), function(i) {
    list(
      product_type = "reanalysis",
      variable = c("geopotential", "u_component_of_wind", "v_component_of_wind"),
      pressure_level = c("500", "550", "600", "650", "700", "750", "775", "800", "825",
                         "850", "875", "900", "925", "950", "975", "1000"),
      year = reqTable$year[i],
      month = reqTable$month[i],
      day = reqTable$day[i],
      time = unlist(lapply(strsplit(reqTable$hours[i], " "), paste, ":00", sep = "")),
      area = areaS,
      format = "netcdf",
      dataset_short_name = "reanalysis-era5-pressure-levels",
      target = paste("download_", "ERA5_", reqTable$year[i], reqTable$month[i], reqTable$day[i], ".nc", sep = "")
    )
  })
  return(reqList)
}

send_requests <- function(reqList, localPath, user, batch_size) {
  all_requests <- list()
  live_requests <- list()
  dld_requests <- list()
  pause <- 0
  #only submit until all requests are submitted
  while (length(all_requests) < length(reqList)) {
    
    #check whether there is a list of all the requests so far
    #update the status of all requests
    if(file.exists(file.path(localPath, "all_requests.rds"))){
      all_requests <- readRDS(file.path(localPath, "all_requests.rds"))
      #check for those requests that are listed as completed and should still be available to download and store in live_requests
      live_requests <- try(lapply(all_requests[which(unlist(lapply(all_requests, function(x) x$get_status()))!="deleted")], function(x) x$update_status()), TRUE)
      if(inherits(live_requests, "try-error")){
        live_requests <- list()
        dld_requests <- list()
        unlink(file.path(localPath, "all_requests.rds"))
      }else{
        #remove the requests that have already been downloaded and store them in dld_requests
        dld_requests <- all_requests[which(unlist(lapply(all_requests, function(x) x$get_status()))=="deleted")]
      }
    }
    
    # Submit new requests if there are fewer than batch_size active requests
    if(length(live_requests) < batch_size && length(all_requests) < length(reqList)) {
      #submit missing requests
      for(i in 1:(batch_size-length(live_requests))){
        req <- reqList[[length(all_requests) + 1]]
        ncfile <- wf_request(user = user, request = req, transfer = FALSE, path = localPath, verbose = FALSE)
        #store the request in the active_requests list
        live_requests <- append(live_requests, list(ncfile))
        all_requests <- append(all_requests, list(ncfile))
        #adjust the number of submitted requests and active requests
      }
    }
    
    #update the status of the live requests
    live_requests <- lapply(live_requests, function(x) x$update_status())
    #list the ones which are completed
    dldList <- which(unlist(lapply(live_requests, function(x) x$get_status()))=="completed")
    if(length(dldList) == 0){
      message("No requests completed. Pausing for 30 seconds.")
      Sys.sleep(30)
      pause <- pause + 1
      message(paste("Resuming after ", pause, " pauses", ".", sep=""))
    } else {
      #download the completed requests, and add the request calls to the list of downloaded request
      dld_requests <- append(dld_requests, lapply(live_requests[dldList], function(x) {
        x$transfer()
        x$delete()
      }
      ))
      #remove the completed requests from the active requests
      live_requests <- live_requests[-dldList]
    }
    message(paste("Number of active requests:", length(live_requests)))
    #save the list of all requests for resuming broken downloads
    saveRDS(all_requests, file = file.path(localPath, "all_requests.rds"))
  }
  
  message(paste("Total requests submitted:", length(all_requests)))
  message(paste("Total files downloaded:", length(dld_requests)))
}


# Function to extract relevant data from the bird track and ERA5 file
extract_track_and_era5_data <- function(bird_track, era5_file) {
  # Extract the date from the ERA5 file name
  era5_date <- ymd(as.numeric(strsplit(basename(era5_file), "_")[[1]][3] %>% substr(1, 8)))
  
  # Find the event ID for the given date
  event_id <- bird_track$event_id[ymd(paste(year(bird_track$timestamp), sprintf("%02d", month(bird_track$timestamp)), sprintf("%02d", day(bird_track$timestamp)))) == era5_date]
  
  # Extract coordinates, times, and heights for the given date
  coords <- as.matrix(st_coordinates(bird_track[ymd(paste(year(bird_track$timestamp), sprintf("%02d", month(bird_track$timestamp)), sprintf("%02d", day(bird_track$timestamp)))) == era5_date, ]))
  times <- mt_time(bird_track[ymd(paste(year(bird_track$timestamp), sprintf("%02d", month(bird_track$timestamp)), sprintf("%02d", day(bird_track$timestamp)))) == era5_date, ])
  heights <- bird_track$height_above_ellipsoid[ymd(paste(year(bird_track$timestamp), sprintf("%02d", month(bird_track$timestamp)), sprintf("%02d", day(bird_track$timestamp)))) == era5_date]
  
  # Create a list with the extracted data
  extracted_data <- list(
    era5_file_name = era5_file,
    era5_date = era5_date,
    bird_track_data = data.frame(
      event_id = event_id,
      coords = coords,
      times = times,
      heights = heights
    )
  )
  
  # Remove rows with missing values
  extracted_data$bird_track_data <- extracted_data$bird_track_data[complete.cases(extracted_data$bird_track_data), ]
  
  return(extracted_data)
}

# Function to annotate the bird track with wind speed and direction data
annotate_wind_data <- function(extracted_data) {
  # Load the ERA5 raster file
  era5_raster <- rast(extracted_data$era5_file_name)
  
  # Extract coordinates from the input data
  coords <- extracted_data$bird_track_data[, c("coords.X", "coords.Y")]
  
  # Extract bilinear interpolated data for u, v, and z variables
  bilinear_data <- terra::extract(era5_raster, coords, method = "bilinear")
  bilinear_data <- bilinear_data[, grep("u_|v_|z_", names(bilinear_data))]
  
  # Prepare time interval data
  time_intervals <- data.frame(
    matrix(unlist(strsplit(names(bilinear_data), "_")), ncol = 3, byrow = TRUE)[, 1:2],
    time = time(era5_raster)
  )
  layers_to_process <- unique(time_intervals[, 1:2])
  
  # Calculate trilinear interpolated data
  trilinear_data <- data.frame(apply(layers_to_process, 1, function(x) {
    layer_id <- grep(paste0(x, collapse = "_"), names(bilinear_data))
    tmp <- bilinear_data[, layer_id]
    ret <- unlist(lapply(1:nrow(tmp), function(y) approx(time(era5_raster)[layer_id], tmp[y, ], xout = extracted_data$bird_track_data$times[y])$y))
    return(ret)
  }))
  names(trilinear_data) <- as.vector(apply(layers_to_process, 1, paste0, collapse = "_"))
  
  # Calculate U and V wind components
  wind_data <- data.frame(
    U_wind = unlist(lapply(1:nrow(trilinear_data), function(x) approx(trilinear_data[x, grep("z_", names(trilinear_data))] / 9.80665, trilinear_data[x, grep("u_", names(trilinear_data))], xout = extracted_data$bird_track_data$heights[x])$y)),
    V_wind = unlist(lapply(1:nrow(trilinear_data), function(x) approx(trilinear_data[x, grep("z_", names(trilinear_data))] / 9.80665, trilinear_data[x, grep("v_", names(trilinear_data))], xout = extracted_data$bird_track_data$heights[x])$y))
  )
  
  # Combine the input data and calculated wind components
  wind_data <- cbind(extracted_data$bird_track_data, wind_data)
  
  return(wind_data)
}


