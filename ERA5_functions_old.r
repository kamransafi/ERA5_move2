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
  #check if the input is a bbox
  if(!is(x, "bbox")){
    bounds <- x
  }
  #check whether the input is a vector of length 4
  if(is(x, "numeric") && length(x) == 4){
    bounds <- data.frame(xmin = x[1],  ymin = x[3], xmax = x[2], ymax = x[4])
  }
  #area: [north, west, south, east]
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
# For each timestamp, request data for the hour before and after (e.g. for 12:40:00, request data at 12:00:00 and 13:00:00 of that day)
date2request <- function(x) {
  DateTime <- sort(unique(c(floor_date(x, "hour"), floor_date(x+3600, "hour"))))
  reqTable <- data.frame(year=year(DateTime), month=sprintf("%02d", month(DateTime)), day=sprintf("%02d", day(DateTime)), hour=sprintf("%02d",hour(DateTime)))
  #split the table into a list with all the observations with the same year, month, day
  reqTable <- split(reqTable, apply(reqTable[,1:3], 1, paste0, collapse=""))
  #collapse to one line per day
  reqTable <- do.call(rbind, lapply(reqTable, function(x) cbind(x[1,1:3], hours= paste0(x[,4], collapse=" "))))
  return(reqTable)
}

# Function to create a list of requests
# I added a few arguments for additional flexibility in the request
create_request_list <- function(reqTable, areaS,
                                datasetName = "reanalysis-era5-pressure-levels", #one of "reanalysis-era5-pressure-levels" or "reanalysis-era5-single-levels"
                                vars = c("geopotential", "u_component_of_wind", "v_component_of_wind"),
                                levels = c("500", "550", "600", "650", "700", "750", "775", "800", "825",
                                           "850", "875", "900", "925", "950", "975", "1000")) {
  if(datasetName == "reanalysis-era5-single-levels" & length(levels)>0){
    print("Request to download surface (single level) variables, levels are ignored")
    levels <- NULL
    ds <- "singleLev"
  }else{
    levels <- levels
    ds <- "pressLev"}
  reqList <- lapply(seq_len(nrow(reqTable)), function(i) {
    list(
      product_type = "reanalysis",
      variable = vars,
      pressure_level = levels,
      year = reqTable$year[i],
      month = reqTable$month[i],
      day = reqTable$day[i],
      time = unlist(lapply(strsplit(reqTable$hours[i], " "), paste, ":00", sep = "")),
      area = areaS,
      format = "netcdf",
      dataset_short_name = datasetName,
      target = paste0("download_ERA5_", reqTable$year[i], reqTable$month[i], reqTable$day[i],"_",ds, ".nc")
    )
  })
  return(reqList)
}

# Might consider adding a timeout option to avoid problems with curl
send_requests <- function(reqList, localPath, user, batch_size, requestFileName="all_requests.rds") {
  all_requests <- list()
  live_requests <- list()
  dld_requests <- list()
  pause <- 0
  #only submit until all requests are submitted
  while (length(all_requests) < length(reqList)) {
    
    #check whether there is a list of all the requests so far
    #update the status of all requests
    if(file.exists(file.path(localPath, paste0(requestFileName,".rds")))){
      all_requests <- readRDS(file.path(localPath, paste0(requestFileName,".rds")))
      #check for those requests that are listed as completed and should still be available to download and store in live_requests
      live_requests <- try(lapply(all_requests[which(unlist(lapply(all_requests, function(x) x$get_status()))!="deleted")], function(x) x$update_status()), TRUE)
      if(inherits(live_requests, "try-error")){
        live_requests <- list()
        dld_requests <- list()
        unlink(file.path(localPath, paste0(requestFileName,".rds")))
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
        ncfile <- wf_request(user = user, request = req, transfer = FALSE, 
                             path = localPath, verbose = FALSE) #time_out=
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
    saveRDS(all_requests, file = file.path(localPath, paste0(requestFileName,".rds")))
  }
  
  message(paste("Total requests submitted:", length(all_requests)))
  message(paste("Total files downloaded:", length(dld_requests)))
}


# Function to extract relevant data from the bird track and ERA5 file
## Change the function to accept move2 or data.frame as input ##
extract_track_and_era5_data <- function(track, era5_file, 
                                        eventCol = "event.id",
                                        timeCol = "timestamp", 
                                        coordsCol = c("location.long","location.lat"), 
                                        heightCol = "height.above.ellipsoid") {
  # Extract the date from the ERA5 file name
  era5_date <- ymd(as.numeric(strsplit(basename(era5_file), "_")[[1]][3] %>% substr(1, 8)))
  ds <- strsplit(basename(era5_file), "_|\\.")[[1]][4]
  
  # Extract event id (row id), coordinates, times, and heights for the given date
  if(is(track, "move2")){
    event_id <- track$event_id[ymd(paste(year(track$timestamp), sprintf("%02d", month(track$timestamp)), sprintf("%02d", day(track$timestamp)))) == era5_date]
    coords <- as.matrix(st_coordinates(track[ymd(paste(year(track$timestamp), sprintf("%02d", month(track$timestamp)), sprintf("%02d", day(track$timestamp)))) == era5_date, ]))
    times <- mt_time(track[ymd(paste(year(track$timestamp), sprintf("%02d", month(track$timestamp)), sprintf("%02d", day(track$timestamp)))) == era5_date, ])
    heights <- track[ymd(paste(year(track$timestamp), sprintf("%02d", month(track$timestamp)), sprintf("%02d", day(track$timestamp)))) == era5_date, heightCol]
  }
  if(is(track, "data.frame")){
    event_id <- track[ymd(paste(year(track[,timeCol]), sprintf("%02d", month(track[,timeCol])), sprintf("%02d", day(track[,timeCol])))) == era5_date, eventCol]
    coords <- track[ymd(paste(year(track[,timeCol]), sprintf("%02d", month(track[,timeCol])), sprintf("%02d", day(track[,timeCol])))) == era5_date, coordsCol]
    times <- track[ymd(paste(year(track[,timeCol]), sprintf("%02d", month(track[,timeCol])), sprintf("%02d", day(track[,timeCol])))) == era5_date,timeCol]
    heights <- track[ymd(paste(year(track[,timeCol]), sprintf("%02d", month(track[,timeCol])), sprintf("%02d", day(track[,timeCol])))) == era5_date, heightCol]
  }
  # Create a list with the extracted data
  extracted_data <- list(
    era5_file_name = era5_file,
    dataset = ds,
    era5_date = era5_date,
    track_data = data.frame(
      event_id = event_id,
      Long = coords[,1],
      Lat = coords[,2],
      timestamp = times,
      heights = heights
    )
  )
  # Remove rows with missing values
  extracted_data$track_data <- extracted_data$track_data[complete.cases(extracted_data$track_data), ]
  
  return(extracted_data)
}



# Function to annotate the bird track with wind speed and direction data
# Trying to add flexibility in n. of variables to annotate
annotate_era5_data <- function(extracted_data, pathToFolder) { #optional path to save files separately
  # Load the ERA5 raster file
  era5_raster <- rast(extracted_data$era5_file_name)
  # if there are locations after 23:00, add to the era5 raster the first hour of the file from the following day
  # if the file doesn't exist, locations after 23:00 will not be annotated (era5 vars == NA)
  if(max(hour(extracted_data$track_data$timestamp)) == 23){ 
    day <- unique(date(extracted_data$track_data$timestamp))
    file_nextDay <- gsub(gsub("-","",day), gsub("-","",day+1), extracted_data$era5_file_name)
    if(!file_nextDay %in% list.files(gsub("/.*","",file_nextDay), full.name=T)){
      warning("Data have locations after 11 pm. Raster file for following date is not available, therefore locations after 11 pm are not annotated.")
    }else{
      era5_nextDay <- rast(file_nextDay)
      era5_nextDay <- era5_nextDay[[hour(time(era5_nextDay)) < 1]]
      names(era5_nextDay) <- sub("_1","_0",names(era5_nextDay))
      era5_raster <- c(era5_raster, era5_nextDay)
    }
  }
  # Variables to annotate
  vars <- unique(paste0(varnames(era5_raster), "_")) #vars are only one letter; add _ to avoid grepping other colnames including that letter
  vars_longname <- unique(gsub(" ","_",paste0(vars, longnames(era5_raster))))
  
  # Extract coordinates from the input data
  coords <- extracted_data$track_data[, c("Long", "Lat")]
  
  ### Interpolate in space: extract bilinear interpolated data of all variables at the track locations
  interpolated_inSpace <- terra::extract(era5_raster, coords, method = "bilinear")
  interpolated_inSpace <- interpolated_inSpace[, grep(paste(vars, collapse="|"), names(interpolated_inSpace))]
  
  ### Data format and processing changes between the single level and the pressure levels datasets
  ### Surface (single lev) variables have column names of extracted raster with only 2 information: variable_timeStep
  ### Pressure level variables have column names of extracted raster with 3 information: variable_pressLevel_timeStep
  if(grepl("single", extracted_data$dataset)){
    layers_to_process <- unique(vars)
  }else{
    pressLevs <- unique(sapply(strsplit(names(interpolated_inSpace), "_"), "[", 2)) # unique pressure levels
    layers_to_process <- apply(expand.grid(vars, pressLevs), 1, paste, collapse="")
  }
  ### Interpolate in time: calculate trilinear interpolated data, calculate the closest values of the variables based on the track timestamps
  interpolated_inTime <- do.call(cbind, lapply(layers_to_process, function(x) { # for each layer to process and each variable
    layer_id <- grep(x, names(interpolated_inSpace)) 
    tmp <- interpolated_inSpace[, layer_id]
    # each row is one location/observation of a certain variable, each column is a time step
    # for each row (track location)
    # (and, for press lev data, each potential pressure level):
    ret <- data.frame(unlist(lapply(1:nrow(tmp), function(y){ 
      approx(unique(time(era5_raster)), # take unique time steps
             tmp[y, ], # take corresponding values at those time steps, for each variable and pressure levels
             xout = extracted_data$track_data$times[y])$y} # interpolate value at exact time of track location
    )))
    names(ret) <- x
    return(ret)
  }))
  # For press lev data, interpolatedInTime will have n.col = var-press level combination. For single level data as many columns as the number of variables.
  
  ### For surface variables nothing more is needed
  if(grepl("single", extracted_data$dataset)){
    # Combine the input data and annotated env data
    names(interpolated_inTime) <- vars_longname # rename more understandably
    annotated_data <- cbind(extracted_data$track_data, interpolated_inTime)
  }
  
  ### For pressure level variables we interpolate in height: interpolate the value of the variables at the closest pressure level, depending on the track height at each location
  ### Each level has a constant pressure, but the corresponding height in m vary depending on the Geopotential (gravitational potential energy of a unit mass at a particular location m2/s2, relative to mean sea level). 
  ### We calculate it below by dividing the geopotential by the Earth's gravitational acceleration, g (=9.80665 m s-2).
  if(grepl("press", extracted_data$dataset)){
    interpolated_inHeight <- do.call(cbind, lapply(vars[-grep("z_",vars)], function(v){
      interpolatedVar <- data.frame(unlist(lapply(1:nrow(interpolated_inTime), function(x){
        approx(interpolated_inTime[x, grep("z_", names(interpolated_inTime))] / 9.80665, #geopotential height
               interpolated_inTime[x, grep(v, names(interpolated_inTime))], #var
               xout = extracted_data$track_data$heights[x])$y # one interpolated value per location
      })))# the output is one interpolated value for each track location/time/height
      names(interpolatedVar) <- vars_longname[grep(v, vars)]
      return(interpolatedVar)
    })) # for each var
    
    # Combine the input data and annotated env data
    annotated_data <- cbind(extracted_data$track_data, interpolated_inHeight)
  }
  
  if(!missing(pathToFolder)){
    saveRDS(annotated_data, file = file.path(pathToFolder, paste0("annotatedData_",extracted_data$era5_date, "_", extracted_data$dataset, ".rds")))
  }
  return(annotated_data)
}
