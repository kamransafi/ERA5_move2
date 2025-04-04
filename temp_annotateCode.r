annotate_era5_data <- function(extracted_data, pathToFolder) {
  # Load ERA5 raster file
  era5_raster <- terra::rast(extracted_data$era5_file_name)
  
  # Get the maximum timestamp in the location data
  max_timestamp <- max(extracted_data$location_data$timestamp)
  
  # Determine if next period's file is needed
  need_next_file <- FALSE
  folder <- dirname(extracted_data$era5_file_name)
  file_type <- extracted_data$file_type
  if (file_type == "daily") {
    next_date <- extracted_data$era5_date_end + lubridate::days(1)
    file_nextPeriod <- file.path(folder, paste0("ERA5_pl_", format(next_date, "%Y%m%d"), ".nc"))
    need_next_file <- TRUE
  } else if (file_type == "weekly") {
    next_date <- extracted_data$era5_date_end + lubridate::days(1)
    next_year <- lubridate::year(next_date)
    next_month <- lubridate::month(next_date)
    next_week <- lubridate::week(next_date)
    file_nextPeriod <- file.path(folder, paste0("ERA5_pl_", 
                                                sprintf("%04d%02d", next_year, next_month), 
                                                "CW", sprintf("%02d", next_week), 
                                                ".nc"))
    need_next_file <- TRUE
  } else if (file_type == "monthly") {
    next_date <- lubridate::ceiling_date(extracted_data$era5_date_end, "month")
    file_nextPeriod <- file.path(folder, paste0("ERA5_pl_", format(next_date, "%Y%m"), ".nc"))
    need_next_file <- TRUE
  }
  
  # Load and concatenate the next period's raster file if needed
  if (need_next_file && file.exists(file_nextPeriod)) {
    era5_nextPeriod <- terra::rast(file_nextPeriod)
    
    # Concatenate rasters while preserving layer names
    era5_raster <- c(era5_raster, era5_nextPeriod)
    
    # Filter layers where valid_time exceeds ceiling(max_timestamp + 1 hour)
    cutoff_time <- lubridate::ceiling_date(max_timestamp + lubridate::hours(1), "hour")
    # Parse valid_time from layer names
    layer_times <- as.POSIXct(
      as.numeric(gsub(".*valid_time=([0-9]+).*", "\\1", names(era5_raster))),
      origin = "1970-01-01",
      tz = "UTC"
    )
    
    # Filter layers where valid_time exceeds cutoff_time
    valid_layers <- which(layer_times <= cutoff_time)
    
    # Subset raster to retain only valid layers
    era5_raster <- era5_raster[[valid_layers]]
    }
  
  # Extract coordinates from the input data
  coords <- extracted_data$location_data[, c("Long", "Lat")]
  
  ### Interpolate in space: extract bilinear interpolated data of all variables at the track locations
  interpolated_inSpace <- terra::extract(era5_raster, coords, method = "bilinear")
  #drop ID column
  interpolated_inSpace <- interpolated_inSpace[,-1]
  ### Now implement temporal interpolation
  interpolated_inTime <- .interpolate_in_time(interpolated_inSpace, extracted_data)
  
    # Combine location data with interpolated results
  annotated_data <- cbind(extracted_data$location_data, interpolated_inTime)
  ###########NEEDS FIXING FROM HERE ONWARDS################
  
  ### Implement temporal interpolation
  layers_to_process <- unique(vars_longname)
  
  interpolated_inTime <- do.call(cbind, lapply(layers_to_process, function(x) {
    .interpolate_in_time(
      interpolated_in_space = interpolated_inSpace,
      layer_name = x,
      location_timestamps = extracted_data$location_data$timestamp
    )
  }))
  
  ### For pressure level variables, interpolate in height using helper function (if needed)
  if (grepl("pl", extracted_data$dataset)) {
    var_names <- unique(gsub("_pressure.*", "", names(interpolated_inTime)))
    var_names <- var_names[!grepl("^z", var_names)]
    
    interpolated_inHeight <- do.call(cbind, lapply(var_names, function(v) {
      var_longname <- paste0(v, "_interpolated")
      
      .interpolate_in_height(
        interpolated_in_time = interpolated_inTime,
        variable_name = v,
        location_heights = extracted_data$location_data$heights,
        variable_longname = var_longname
      )
    }))
    
    annotated_data <- cbind(extracted_data$location_data, interpolated_inHeight)
  } else {
    annotated_data <- cbind(extracted_data$location_data, interpolated_inTime)
  }
  
  ### Save results if path is provided
  if (!missing(pathToFolder)) {
    if (!dir.exists(pathToFolder)) {
      dir.create(pathToFolder, recursive = TRUE)
    }
    
    saveRDS(annotated_data, file = file.path(pathToFolder, paste0("annotatedData_", 
                                                                  format(max_timestamp, "%Y%m%d_%H%M"), 
                                                                  ".rds")))
  }
  
  return(annotated_data)
}
