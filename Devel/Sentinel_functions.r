#' @title Sentinel Data Retrieval Library for Copernicus Climate Data Store
#' @description A comprehensive library for requesting and processing Sentinel satellite 
#'              data from the Copernicus Climate Data Store (CDS). This library facilitates 
#'              the request, download, and extraction of data from Sentinel-1, Sentinel-2, 
#'              Sentinel-3, and Sentinel-5P satellites for tracking animal movements.
#'
#' @details The library provides functions to:
#'          1. Create request tables based on movement tracking data
#'          2. Generate satellite-specific CDS API requests
#'          3. Submit requests and manage downloads with rate limiting
#'          4. Extract and process data at movement track locations
#'
#' @author Your Name
#' @version 1.0.0
#' @date March 16, 2025
#'
#' @dependencies move2, sf, lubridate, ecmwfr (>=2.0.0), terra, dplyr, data.table
#'
#' @note This library requires a Copernicus Climate Data Store account with an API key
#'       and manual license acceptance for each dataset through the CDS web interface.

#########################################################################################


#' @title Define Area for CDS API Request
#' @description Creates a formatted area string for Copernicus Climate Data Store API requests
#'              with optional extension of the spatial extent.
#'
#' @param x A bbox object from sf or a numeric vector of length 4 with coordinates 
#'          in the order: xmin, xmax, ymin, ymax
#' @param ext Numeric. Extension factor for the bounding box (default: 0.01)
#'
#' @return Character string with area coordinates in format "north/west/south/east"
#'
#' @details The function ensures that coordinates stay within valid ranges:
#'          -180 <= longitude <= 180
#'          -90 <= latitude <= 90
#'
#' @examples
#' # With a bbox object
#' bbox <- st_bbox(some_sf_object)
#' area_str <- sentinel_area_request(bbox, ext = 0.02)
#'
#' # With numeric vector
#' area_str <- sentinel_area_request(c(-10, 10, 40, 50), ext = 0.01)
sentinel_area_request <- function(x, ext = 0.01) {
  #check if the input is a bbox
  if(is(x, "bbox")){
    bounds <- x
  }
  #check whether the input is a vector of length 4 
  if(is(x, "numeric") && length(x) == 4){
    bounds <- data.frame(xmin = x[1], xmax = x[2], ymin = x[3], ymax = x[4])
  }
  #area: [north, west, south, east]
  x_range <- bounds$xmax - bounds$xmin
  y_range <- bounds$ymax - bounds$ymin
  west <- floor(bounds$xmin - ext * x_range)
  if(west <= -180){west <- -180}
  east <- ceiling(bounds$xmax + ext * x_range)
  if(east >= 180){east <- 180}
  south <- floor(bounds$ymin - ext * y_range)
  if(south <= -90){south <- -90}
  north <- ceiling(bounds$ymax + ext * y_range)
  if(north >= 90){north <- 90}
  return(paste(north, west, south, east, sep="/"))
}

#' @title Generate Date Request Table for Sentinel Data
#' @description Creates a request table with date and time information based on a vector
#'              of timestamps, aggregated by the specified temporal unit.
#'
#' @param x Vector of POSIXct timestamps from tracking data
#' @param unit Character. Temporal aggregation unit: "day", "week", or "month"
#'
#' @return Data frame with columns for year, month, days, and hours, where multiple
#'         values per time unit are concatenated with spaces
#'
#' @details For each temporal unit (day, week, month), the function:
#'          1. Groups timestamps by the specified unit
#'          2. Extracts unique days and hours within each unit
#'          3. Creates row names with appropriate date format
#'
#' @examples
#' # Create request table with daily aggregation
#' timestamps <- mt_time(track_data)
#' daily_req <- sentinel_date_request(timestamps, "day")
#'
#' # Create request table with weekly aggregation
#' weekly_req <- sentinel_date_request(timestamps, "week")
sentinel_date_request <- function(x, unit) {
  if(!unit %in% c("day", "week", "month")){stop("Unit has to be either 'day', 'week', or 'month'")}
  DateTime <- sort(unique(c(floor_date(x, "hour"), floor_date(x+3600, "hour"))))
  reqTable <- data.frame(year=year(DateTime), month=sprintf("%02d", month(DateTime)), 
                         week=sprintf("%02d", week(DateTime)), days=sprintf("%02d", day(DateTime)), 
                         hours=sprintf("%02d",hour(DateTime)))
  if(unit=="day"){
    #split the table into a list with all the observations with the same year, month, day
    reqTable <- split(reqTable, apply(reqTable[,c("year", "month", "days")], 1, paste0, collapse=""))
    #collapse to one line per day
    reqTable <- do.call(rbind, lapply(reqTable, function(x) 
      cbind(x[1,c("year", "month", "days")], hours= paste0(x[,"hours"], collapse=" "))))
  }
  if(unit=="week"){
    #split the table into a list with all the observations with the same year, month, week
    reqTable <- split(reqTable, apply(reqTable[,c("year", "month", "week")], 1, paste0, collapse=""))
    #collapse to one line per day
    reqTable <- do.call(rbind, lapply(reqTable, function(x) 
      cbind(x[1,c("year", "month", "week")], days=  paste0(unique(x[,"days"]), collapse=" "), 
            hours= paste0(unique(x[,"hours"]), collapse=" "))))
    row.names(reqTable) <- unlist(apply(reqTable, 1, 
                                        function(x) paste0(x[1], x[2], "CW", x[3], collapse="")))
    reqTable <- reqTable[,c("year", "month", "days", "hours")]
  }
  if(unit == "month"){
    #split the table into a list with all the observations with the same year and month
    reqTable <- split(reqTable, apply(reqTable[,1:2], 1, paste0, collapse=""))
    #collapse to one line per month
    reqTable <- do.call(rbind, lapply(reqTable, function(x) 
      cbind(x[1,c("year", "month")], days = paste0(unique(x[,"days"]), collapse=" "), 
            hours = paste0(unique(x[,"hours"]), collapse=" "))))
  }
  return(reqTable)
}

#' @title Create Sentinel Request Table from Multiple Spatial-Temporal Input Types
#' @description Creates a structured request table for Sentinel satellite data based on
#'              various spatial-temporal inputs (move2 object, sf object, dataframe, or extent + timerange)
#'
#' @param track Input object. Can be: 
#'        - move2 object with tracking data
#'        - sf object with time information
#'        - data.frame with coordinates and timestamps
#'        - numeric vector or bbox object specifying an extent
#' @param timeUnit Character. Temporal aggregation unit: "day", "week", or "month"
#' @param area Character. Spatial extent strategy: "complete" or "byTimeUnit"
#' @param cloud_coverage Numeric. Maximum acceptable cloud coverage percentage (default: 30)
#' @param area_ext Numeric. Extension factor for bounding boxes (default: 0.01)
#' @param timeCol Character. Column name containing timestamps (for sf and data.frame inputs)
#' @param lonCol Character. Column name for longitude (for data.frame input only)
#' @param latCol Character. Column name for latitude (for data.frame input only)
#' @param start_time POSIXct. Start time (for extent input only)
#' @param end_time POSIXct. End time (for extent input only)
#' @param time_res Character. Time resolution for generating timestamps (for extent input only)
#'
#' @return Data frame with request information including year, month, days, hours,
#'         bounding box coordinates, and cloud coverage
sentinel_create_request_table <- function(track, 
                                          timeUnit = c("day", "week", "month"), 
                                          area = c("complete", "byTimeUnit"),
                                          cloud_coverage = 30,
                                          area_ext = 0.01,
                                          timeCol = NULL,
                                          lonCol = NULL, 
                                          latCol = NULL,
                                          start_time = NULL,
                                          end_time = NULL,
                                          time_res = "1 hour") {
  
  # Determine input type and prepare data accordingly
  if ("move2" %in% class(track)) {
    # CASE 1: move2 object (original use case)
    if (any(st_is_empty(track$geometry))) {
      stop("Your 'track' has missing coordinates! Remove rows with missing coordinates.")
    }
    if (grep("+proj=longlat", st_crs(track)$proj4string) != 1) {
      stop("The projection of your track is not '+proj=longlat +datum=WGS84'. Please reproject!")
    }
    
    ts <- mt_time(track)
    extent <- st_bbox(track)
    use_track <- track
    input_type <- "move2"
    
  } else if ("sf" %in% class(track) && !is.null(timeCol)) {
    # CASE 2: sf object with time column
    if (any(st_is_empty(track$geometry))) {
      stop("Your 'track' has missing coordinates! Remove rows with missing coordinates.")
    }
    if (grep("+proj=longlat", st_crs(track)$proj4string) != 1) {
      stop("The projection of your track is not '+proj=longlat +datum=WGS84'. Please reproject!")
    }
    
    ts <- track[[timeCol]]
    if (is.null(ts)) {
      stop(paste("Time column", timeCol, "not found in sf object"))
    }
    extent <- st_bbox(track)
    use_track <- track
    input_type <- "sf_with_time"
    
  } else if (is.data.frame(track) && !("sf" %in% class(track))) {
    # CASE 3: data.frame with coordinates and time
    if (is.null(timeCol) || is.null(lonCol) || is.null(latCol)) {
      stop("For data.frame inputs, you must specify timeCol, lonCol, and latCol parameters")
    }
    
    if (is.null(track[[timeCol]]) || is.null(track[[lonCol]]) || is.null(track[[latCol]])) {
      stop("The specified timeCol, lonCol, or latCol columns do not exist in the data.frame")
    }
    
    # Convert data.frame to sf object
    use_track <- st_as_sf(track, coords = c(lonCol, latCol), crs = 4326)
    ts <- track[[timeCol]]
    extent <- st_bbox(use_track)
    input_type <- "dataframe"
    
  } else if ((is.numeric(track) && length(track) == 4) || ("bbox" %in% class(track))) {
    # CASE 4: extent (bbox or numeric vector) and time range
    if (is.null(start_time) || is.null(end_time)) {
      stop("For extent inputs, you must specify start_time and end_time parameters")
    }
    
    # Generate a sequence of timestamps from start to end time
    ts <- seq(from = start_time, to = end_time, by = time_res)
    
    # Use the provided extent
    if ("bbox" %in% class(track)) {
      extent <- track
    } else if (is.numeric(track) && length(track) == 4) {
      # Create a bbox from numeric vector [xmin, ymin, xmax, ymax]
      extent <- st_bbox(c(xmin = track[1], ymin = track[2], xmax = track[3], ymax = track[4]), crs = st_crs(4326))
    }
    use_track <- NULL
    input_type <- "extent_timerange"
    
  } else if ("sf" %in% class(track) && is.null(timeCol)) {
    # CASE 1b: sf object without specified time column (assuming it has a time field)
    if (any(st_is_empty(track$geometry))) {
      stop("Your 'track' has missing coordinates! Remove rows with missing coordinates.")
    }
    if (grep("+proj=longlat", st_crs(track)$proj4string) != 1) {
      stop("The projection of your track is not '+proj=longlat +datum=WGS84'. Please reproject!")
    }
    
    # Try to find a time column (common names)
    time_columns <- c("time", "timestamp", "date", "datetime")
    found_time <- FALSE
    
    for (col in time_columns) {
      if (col %in% names(track) && !found_time) {
        message(paste("Using", col, "as time column for sf object"))
        ts <- track[[col]]
        found_time <- TRUE
        break
      }
    }
    
    if (!found_time) {
      stop("No time column specified and no default time column found in sf object. Please specify timeCol parameter.")
    }
    
    extent <- st_bbox(track)
    use_track <- track
    input_type <- "sf_with_time"
    
  } else {
    stop("Input type not recognized or missing required parameters")
  }
  
  # Split data by timeUnit
  reqTable <- sentinel_date_request(x = ts, unit = timeUnit)
  
  # Handle area specification
  if (area == "complete") {
    reqTable$bbox <- sentinel_area_request(x = extent, ext = area_ext)
  } else if (area == "byTimeUnit") {
    if (input_type %in% c("move2", "sf_with_time", "dataframe")) {
      # For track-based inputs, we can compute time-specific bounding boxes
      if (timeUnit == "day") {
        reqDT <- unlist(lapply(ts, function(x) 
          paste0(year(x), sprintf("%02d", month(x)), sprintf("%02d", day(x)), collapse = "")))
        reqTable$bbox <- lapply(rownames(reqTable), function(x) 
          sentinel_area_request(st_bbox(use_track[reqDT == x, ]), ext = area_ext))
      } else if (timeUnit == "week") {
        reqDT <- unlist(lapply(ts, function(x) 
          paste0(year(x), sprintf("%02d", month(x)), "CW", sprintf("%02d", week(x)), collapse = "")))
        reqTable$bbox <- lapply(rownames(reqTable), function(x) 
          sentinel_area_request(st_bbox(use_track[reqDT == x, ]), ext = area_ext))
      } else if (timeUnit == "month") {
        reqDT <- unlist(lapply(ts, function(x) 
          paste0(year(x), sprintf("%02d", month(x)), collapse = "")))
        reqTable$bbox <- lapply(rownames(reqTable), function(x) 
          sentinel_area_request(st_bbox(use_track[reqDT == x, ]), ext = area_ext))
      }
    } else {
      # For extent input, use the same extent for all time periods
      message("Using the same spatial extent for all time periods with extent-based input")
      reqTable$bbox <- sentinel_area_request(x = extent, ext = area_ext)
    }
  }
  
  # Add cloud coverage parameter to request table
  reqTable$cloud_coverage <- cloud_coverage
  
  return(reqTable)
}

#' @title Create Sentinel-1 Specific Data Requests
#' @description Generates a list of Sentinel-1 data request parameters for the CDS API
#'              based on the provided request table.
#'
#' @param reqTable Request table generated by sentinel_create_request_table()
#' @param polarisation Character vector. Polarization modes to download (default: c("VV", "VH"))
#' @param product_type Character. Sentinel-1 product type (default: "GRD")
#'
#' @return List of CDS API request parameters for each row in the request table
#'
#' @details Creates formatted request parameters specific to Sentinel-1 SAR data,
#'          including appropriate file naming and area specification.
#'
#' @examples
#' # Create Sentinel-1 data requests with custom polarizations
#' s1_requests <- sentinel_request_S1(
#'   reqTable = req_table,
#'   polarisation = c("VV", "VH"),
#'   product_type = "GRD"
#' )
sentinel_request_S1 <- function(reqTable, 
                                polarisation = c("VV", "VH"),
                                product_type = "GRD") {
  
  reqList <- lapply(seq_len(nrow(reqTable)), function(i) {
    filename <- paste0("Sentinel1_", product_type, "_", rownames(reqTable[i,]), ".zip")
    
    # Split day values for the request format
    days <- sort(unlist(strsplit(reqTable$days[i], " ")))
    
    # Extract area values from the bbox string
    area_values <- as.numeric(unlist(strsplit(unlist(reqTable$bbox[i]), "/")))
    area_string <- c(area_values[1], area_values[2], area_values[3], area_values[4])
    
    # Create the request list
    list(
      dataset_short_name = "satellite-sar",
      product_type = paste0("sentinel-1-", tolower(product_type)),
      polarisation = polarisation,
      year = reqTable$year[i],
      month = reqTable$month[i],
      day = days,
      area = area_string,
      format = "zip",
      target = filename
    )
  })
  
  return(reqList)
}

#' @title Create Sentinel-2 Specific Data Requests
#' @description Generates a list of Sentinel-2 data request parameters for the CDS API
#'              based on the provided request table.
#'
#' @param reqTable Request table generated by sentinel_create_request_table()
#' @param processing_level Character. Processing level of Sentinel-2 data (default: "Level-2A")
#' @param bands Character vector. Spectral bands to download (default: c("B02", "B03", "B04", "B08"))
#'
#' @return List of CDS API request parameters for each row in the request table
#'
#' @details Creates formatted request parameters specific to Sentinel-2 optical data,
#'          including appropriate file naming, cloud coverage, and band specification.
#'
#' @examples
#' # Create Sentinel-2 data requests with specific bands
#' s2_requests <- sentinel_request_S2(
#'   reqTable = req_table,
#'   processing_level = "Level-2A",
#'   bands = c("B02", "B03", "B04", "B08", "B11", "B12")
#' )
sentinel_request_S2 <- function(reqTable, 
                                processing_level = "Level-2A",  # Level-2A has atmospheric correction
                                bands = c("B02", "B03", "B04", "B08")) {  # RGB + NIR by default
  
  reqList <- lapply(seq_len(nrow(reqTable)), function(i) {
    filename <- paste0("Sentinel2_", processing_level, "_", rownames(reqTable[i,]), ".zip")
    
    # Split day values for the request format
    days <- sort(unlist(strsplit(reqTable$days[i], " ")))
    
    # Extract area values from the bbox string (north/west/south/east)
    area_values <- as.numeric(unlist(strsplit(unlist(reqTable$bbox[i]), "/")))
    area_string <- c(area_values[1], area_values[2], area_values[3], area_values[4])
    
    # Create the request list
    list(
      dataset_short_name = "satellite-surface-reflectance",
      product_type = "sentinel-2-l2a",  # Product type depends on processing_level
      variable = bands,
      year = reqTable$year[i],
      month = reqTable$month[i],
      day = days,
      time = "00:00",  # For Sentinel, this is typically acquisition time
      area = area_string,
      format = "zip",  # Sentinel data usually comes as zip archives containing GeoTIFFs
      cloud_cover = reqTable$cloud_coverage,
      target = filename
    )
  })
  
  return(reqList)
}

#' @title Create Sentinel-3 Specific Data Requests
#' @description Generates a list of Sentinel-3 data request parameters for the CDS API
#'              based on the provided request table.
#'
#' @param reqTable Request table generated by sentinel_create_request_table()
#' @param instrument Character. Sentinel-3 instrument: "OLCI", "SLSTR", or "SRAL"
#' @param product_type Character. Product type for selected instrument (if NULL, uses defaults)
#' @param processing_level Character. Processing level of data (default: "L2")
#' @param variables Character vector. Variables/bands to download (if NULL, uses defaults)
#'
#' @return List of CDS API request parameters for each row in the request table
#'
#' @details Creates formatted request parameters specific to Sentinel-3 data,
#'          setting default products and variables based on the selected instrument.
#'
#' @examples
#' # Create Sentinel-3 OLCI data requests
#' s3_requests <- sentinel_request_S3(
#'   reqTable = req_table,
#'   instrument = "OLCI",
#'   processing_level = "L2"
#' )
sentinel_request_S3 <- function(reqTable, 
                                instrument = c("OLCI", "SLSTR", "SRAL"),
                                product_type = NULL,
                                processing_level = "L2",
                                variables = NULL) {
  
  instrument <- match.arg(instrument)
  
  # Set default product types based on instrument
  if(is.null(product_type)) {
    if(instrument == "OLCI") {
      product_type <- "EFR" # Earth Full Resolution
    } else if(instrument == "SLSTR") {
      product_type <- "WST" # Water Surface Temperature
    } else if(instrument == "SRAL") {
      product_type <- "WAT" # Water products
    }
  }
  
  # Set default variables based on instrument
  if(is.null(variables)) {
    if(instrument == "OLCI") {
      variables <- c("Oa01", "Oa02", "Oa03", "Oa04", "Oa08") # Blue, Green, Red, NIR bands
    } else if(instrument == "SLSTR") {
      variables <- c("S1", "S2", "S3", "S4", "S5", "S6") # Visible and SWIR bands
    } else if(instrument == "SRAL") {
      variables <- c("range_ocean", "significant_wave_height", "backscatter_coefficient")
    }
  }
  
  # Create request list for each row in the request table
  reqList <- lapply(seq_len(nrow(reqTable)), function(i) {
    filename <- paste0("Sentinel3_", instrument, "_", product_type, "_", processing_level, "_", rownames(reqTable[i,]), ".zip")
    
    # Split day values for the request format
    days <- sort(unlist(strsplit(reqTable$days[i], " ")))
    
    # Extract area values from the bbox string (north/west/south/east)
    area_values <- as.numeric(unlist(strsplit(unlist(reqTable$bbox[i]), "/")))
    area_string <- c(area_values[1], area_values[2], area_values[3], area_values[4])
    
    # Create the request list
    list(
      dataset_short_name = "satellite-ocean-colour",
      product_type = paste0("sentinel-3-", tolower(instrument), "-", tolower(product_type)),
      variable = variables,
      processing_level = processing_level,
      year = reqTable$year[i],
      month = reqTable$month[i],
      day = days,
      area = area_string,
      format = "zip",
      target = filename
    )
  })
  
  return(reqList)
}

#' @title Create Sentinel-5P Specific Data Requests
#' @description Generates a list of Sentinel-5P data request parameters for the CDS API
#'              based on the provided request table.
#'
#' @param reqTable Request table generated by sentinel_create_request_table()
#' @param product_type Character. Atmospheric constituent: "NO2", "O3", "CO", "CH4", "SO2", 
#'                    "HCHO", "AER", or "CLOUD"
#' @param processing_level Character. Processing level of data (default: "L2")
#' @param variables Character vector. Variables to download (if NULL, uses defaults)
#'
#' @return List of CDS API request parameters for each row in the request table
#'
#' @details Creates formatted request parameters specific to Sentinel-5P atmospheric data,
#'          setting default variables based on the selected atmospheric constituent.
#'
#' @examples
#' # Create Sentinel-5P NO2 data requests
#' s5p_requests <- sentinel_request_S5P(
#'   reqTable = req_table,
#'   product_type = "NO2",
#'   processing_level = "L2"
#' )
sentinel_request_S5P <- function(reqTable, 
                                 product_type = c("NO2", "O3", "CO", "CH4", "SO2", "HCHO", "AER", "CLOUD"),
                                 processing_level = "L2",
                                 variables = NULL) {
  
  product_type <- match.arg(product_type)
  
  # Set default variables based on product type
  if(is.null(variables)) {
    if(product_type == "NO2") {
      variables <- c("NO2_column_number_density", "tropospheric_NO2_column_number_density")
    } else if(product_type == "CO") {
      variables <- c("CO_column_number_density")
    } else if(product_type == "O3") {
      variables <- c("O3_column_number_density", "O3_effective_temperature")
    } else if(product_type == "CH4") {
      variables <- c("CH4_column_volume_mixing_ratio_dry_air")
    } else if(product_type == "SO2") {
      variables <- c("SO2_column_number_density")
    } else if(product_type == "HCHO") {
      variables <- c("tropospheric_HCHO_column_number_density")
    } else if(product_type == "AER") {
      variables <- c("aerosol_index", "aerosol_optical_depth")
    } else if(product_type == "CLOUD") {
      variables <- c("cloud_fraction", "cloud_top_pressure", "cloud_top_height")
    }
  }
  
  # Format CDS product name
  cds_product <- paste0("sentinel-5p-", tolower(product_type))
  
  # Create request list for each row in the request table
  reqList <- lapply(seq_len(nrow(reqTable)), function(i) {
    filename <- paste0("Sentinel5P_", product_type, "_", processing_level, "_", rownames(reqTable[i,]), ".zip")
    
    # Split day values for the request format
    days <- sort(unlist(strsplit(reqTable$days[i], " ")))
    
    # Extract area values from the bbox string (north/west/south/east)
    area_values <- as.numeric(unlist(strsplit(unlist(reqTable$bbox[i]), "/")))
    area_string <- c(area_values[1], area_values[2], area_values[3], area_values[4])
    
    # Create the request list
    list(
      dataset_short_name = "satellite-atmospheric-composition",
      product_type = cds_product,
      variable = variables,
      processing_level = processing_level,
      year = reqTable$year[i],
      month = reqTable$month[i],
      day = days,
      time = "00:00",  # For most atmospheric data, daily values are used
      area = area_string,
      format = "zip",
      target = filename
    )
  })
  
  return(reqList)
}

#' @title Create Appropriate Sentinel Data Request List
#' @description Main function to create satellite-specific request lists for any
#'              Sentinel satellite product based on the request table.
#'
#' @param reqTable Request table generated by sentinel_create_request_table()
#' @param satellite Character. Which Sentinel satellite data to request: "Sentinel-1",
#'                 "Sentinel-2", "Sentinel-3", or "Sentinel-5P"
#' @param product_options List. Additional parameters for specific satellite products
#'
#' @return List of CDS API request parameters for the specified satellite
#'
#' @details This function serves as a dispatcher that calls the appropriate
#'          satellite-specific request function based on the selected satellite,
#'          handling default options for each satellite type.
#'
#' @examples
#' # Create Sentinel-2 request list with custom bands
#' s2_options <- list(
#'   processing_level = "Level-2A",
#'   bands = c("B02", "B03", "B04", "B08", "B11")
#' )
#' s2_requests <- sentinel_create_request_list(
#'   reqTable = req_table,
#'   satellite = "Sentinel-2",
#'   product_options = s2_options
#' )
#'
#' # Create Sentinel-5P request list for CO data
#' s5p_options <- list(product_type = "CO")
#' s5p_requests <- sentinel_create_request_list(
#'   reqTable = req_table,
#'   satellite = "Sentinel-5P",
#'   product_options = s5p_options
#' )
sentinel_create_request_list <- function(reqTable, 
                                         satellite = c("Sentinel-1", "Sentinel-2", "Sentinel-3", "Sentinel-5P"),
                                         product_options = list()) {
  
  satellite <- match.arg(satellite)
  
  message(paste0("Creating the request list for ", nrow(reqTable), " requests for ", satellite, " data."))
  
  if(satellite == "Sentinel-1") {
    # Default options for Sentinel-1
    if(is.null(product_options$polarisation)) product_options$polarisation <- c("VV", "VH")
    if(is.null(product_options$product_type)) product_options$product_type <- "GRD"
    
    reqList <- sentinel_request_S1(reqTable, 
                                   polarisation = product_options$polarisation,
                                   product_type = product_options$product_type)
  }
  
  else if(satellite == "Sentinel-2") {
    # Default options for Sentinel-2
    if(is.null(product_options$processing_level)) product_options$processing_level <- "Level-2A"
    if(is.null(product_options$bands)) product_options$bands <- c("B02", "B03", "B04", "B08")
    
    reqList <- sentinel_request_S2(reqTable, 
                                   processing_level = product_options$processing_level,
                                   bands = product_options$bands)
  }
  
  else if(satellite == "Sentinel-3") {
    # Default options for Sentinel-3
    if(is.null(product_options$instrument)) product_options$instrument <- "OLCI"
    if(is.null(product_options$product_type)) product_options$product_type <- NULL  # Will use defaults in function
    if(is.null(product_options$processing_level)) product_options$processing_level <- "L2"
    if(is.null(product_options$variables)) product_options$variables <- NULL  # Will use defaults in function
    
    reqList <- sentinel_request_S3(reqTable,
                                   instrument = product_options$instrument,
                                   product_type = product_options$product_type,
                                   processing_level = product_options$processing_level,
                                   variables = product_options$variables)
  }
  
  else if(satellite == "Sentinel-5P") {
    # Default options for Sentinel-5P
    if(is.null(product_options$product_type)) product_options$product_type <- "NO2"
    if(is.null(product_options$processing_level)) product_options$processing_level <- "L2"
    if(is.null(product_options$variables)) product_options$variables <- NULL  # Will use defaults in function
    
    reqList <- sentinel_request_S5P(reqTable,
                                    product_type = product_options$product_type,
                                    processing_level = product_options$processing_level,
                                    variables = product_options$variables)
  }
  
  return(reqList)
}

#' @title Submit Sentinel Data Requests to CDS
#' @description Submits data requests to the Copernicus Climate Data Store API and
#'              handles downloads with rate limiting and error recovery.
#'
#' @param reqList Request list generated by sentinel_create_request_list()
#' @param localPath Character. Directory path to save downloaded files
#' @param batch_size Numeric. Maximum number of concurrent requests (default: 10)
#'
#' @return Invisible list of downloaded request objects
#'
#' @details Implements a robust request submission strategy that:
#'          1. Checks for previously downloaded files
#'          2. Resumes interrupted download sessions
#'          3. Implements rate limiting with exponential backoff
#'          4. Handles various error conditions
#'          5. Downloads completed requests sequentially
#'
#' @examples
#' # Submit Sentinel-2 data requests
#' sentinel_send_requests(
#'   reqList = s2_requests,
#'   localPath = "data/sentinel2",
#'   batch_size = 5
#' )
sentinel_send_requests <- function(reqList, localPath, batch_size = 10) {
  # Remove the requests that have already been downloaded
  existing_files <- list.files(localPath, pattern = "^Sentinel[12]_.*\\.zip$", full.names = TRUE)
  reqList <- reqList[!vapply(reqList, function(req) 
    file.path(localPath, req$target) %in% existing_files, FUN.VALUE = logical(1))]
  
  # Check for a previous request list 
  if(file.exists(paste(localPath, "sentinel_all_requests.rds", sep="/"))) {
    all_requests <- readRDS(paste(localPath, "sentinel_all_requests.rds", sep="/"))
    # Take as live requests only those that have not yet completed with a local downloaded file
    live_requests <- all_requests[!unlist(lapply(lapply(all_requests, function(x) x$get_request()$target), 
                                                 function(y) y %in% list.files(localPath, 
                                                                               pattern = "^Sentinel[12]_.*\\.zip$", full.names = FALSE)))]
    # Clear the all requests lists only for those that are still required to be downloaded
    all_requests <- live_requests
  } else {
    all_requests <- list()
    live_requests <- list()
  }
  
  dld_requests <- list()
  inheritedJobs <- length(all_requests)
  # Remove already requested but not yet downloaded requests from the list of pending requests
  reqList <- reqList[!unlist(lapply(reqList, function(x) x$target)) %in% lapply(live_requests, function(y) y$get_request()$target)]
  nJobs <- inheritedJobs + length(reqList)
  message(paste("Total of ", nJobs, " pending requests remain after removing previously completed jobs and adding still pending requests on CDS!", sep=""))
  
  # Variables for rate limiting
  request_interval <- 10    # Seconds between submitting new requests
  download_interval <- 60   # Seconds between downloading files (larger for Sentinel data)
  backoff_time <- 120       # Initial backoff time for rate limit errors (2 minutes)
  max_backoff <- 1800       # Maximum backoff time (30 minutes)
  
  # Continue until all requests are downloaded
  while (length(dld_requests) < nJobs) {
    # Submit new requests if there are fewer than batch_size active requests
    if(length(live_requests) < batch_size && (nJobs-length(all_requests)) > 0) {
      # Submit missing requests with delay between each
      for(i in 1:min(batch_size-length(live_requests), (nJobs-length(all_requests)))) {
        if ((length(all_requests)-inheritedJobs)+1 <= length(reqList)) {
          req <- reqList[[(length(all_requests)-inheritedJobs)+1]]
          # Use tryCatch to handle potential errors
          result <- tryCatch({
            # Using wf_request instead of the old method
            ncfile <- wf_request(request = req, transfer = FALSE, path = localPath, verbose = FALSE)
            # Store the request in the active_requests list
            live_requests <- append(live_requests, list(ncfile))
            all_requests <- append(all_requests, list(ncfile))
            # Save the list of all requests for resuming broken downloads
            saveRDS(all_requests, paste(localPath, "all_requests.rds", sep="/"))
            "success"
          }, error = function(e) {
            if (grepl("rate limit", e$message, ignore.case = TRUE)) {
              message(paste("Rate limit reached. Waiting", backoff_time, "seconds before retrying..."))
              Sys.sleep(backoff_time)
              backoff_time <- min(backoff_time * 2, max_backoff)  # Exponential backoff
              return("rate_limited")
            } else {
              message(paste("Error submitting request:", e$message))
              return("error")
            }
          })
          
          # Wait between submissions regardless of outcome
          Sys.sleep(request_interval)
          
          # If rate limited, break the submission loop and retry later
          if (result == "rate_limited") {
            break
          }
        }
      }
    }
    
    # Update the status of the live requests
    live_requests <- lapply(live_requests, function(x) {
      tryCatch({
        return(x$update_status())
      }, error = function(e) {
        if (grepl("rate limit", e$message, ignore.case = TRUE)) {
          message(paste("Rate limit reached during status check. Waiting", backoff_time, "seconds..."))
          Sys.sleep(backoff_time)
          backoff_time <- min(backoff_time * 2, max_backoff)
        } else {
          message(paste("Error updating status:", e$message))
        }
        return(x)  # Return unchanged request object if update fails
      })
    })
    
    # Print current status values to help diagnose issues
    current_statuses <- unlist(lapply(live_requests, function(x) {
      tryCatch(x$get_status(), error = function(e) return("unknown"))
    }))
    if(length(current_statuses) > 0) {
      message(paste("Current status values:", paste(unique(current_statuses), collapse=", ")))
    }
    
    # List the ones which are completed - checking for multiple possible status values
    completed_statuses <- c("completed", "successful", "succeed", "done")
    dldList <- which(unlist(lapply(live_requests, function(x) {
      status <- tryCatch({
        tolower(x$get_status())
      }, error = function(e) {
        return("unknown")
      })
      return(status %in% tolower(completed_statuses))
    })))
    
    if(length(dldList) == 0) {
      message("\nWaiting for CDS for 30 seconds.")
      Sys.sleep(30)
      message("------------------------------------------------\n")
    } else {
      message(paste("Found", length(dldList), "completed requests. Downloading..."))
      
      # Download completed requests one by one with delays between them
      for (idx in dldList) {
        tryCatch({
          message(paste("Downloading:", live_requests[[idx]]$get_request()$target))
          live_requests[[idx]]$transfer()
          live_requests[[idx]]$delete()
          # Add delay between downloads to avoid rate limiting
          Sys.sleep(download_interval)
        }, error = function(e) {
          if (grepl("rate limit", e$message, ignore.case = TRUE)) {
            message(paste("Rate limit reached during download. Waiting", backoff_time, "seconds..."))
            Sys.sleep(backoff_time)
            backoff_time <- min(backoff_time * 2, max_backoff)
            # Mark this request for retry in the next iteration
            return(NULL)
          } else {
            message(paste("Error downloading file:", e$message))
          }
        })
      }
      
      # Remove successfully downloaded requests from the active list
      downloaded_files <- list.files(localPath, pattern = "^ERA5_.*\\.nc$", full.names = FALSE)
      to_remove <- which(unlist(lapply(live_requests, function(x) {
        target <- tryCatch(x$get_request()$target, error = function(e) return(""))
        return(target %in% downloaded_files)
      })))
      
      if(length(to_remove) > 0) {
        live_requests <- live_requests[-to_remove]
      }
    }
    
    message(paste("Number of requests alive on CDS: ", length(live_requests)))
    # Fixed bracketing error in this section
    dld_requests <- all_requests[unlist(lapply(all_requests, function(x) {
      target <- tryCatch(x$get_request()$target, error = function(e) return(""))
      return(target %in% list.files(localPath, pattern = "^ERA5_.*\\.nc$", full.names = FALSE))
    }))]
    message(paste("Downloaded ", length(dld_requests), " of ", nJobs, " pending requests."))
    
    # Reset backoff time if we've made progress
    if(length(dld_requests) > 0) {
      backoff_time <- 60  # Reset to initial value
    }
  }
  
  message("FINISHED!")
  message(paste("Total requests submitted:", length(all_requests)))
  message(paste("Total files downloaded:", length(dld_requests)))
  unlink(paste(localPath, "all_requests.rds", sep="/"))
}

#' @title Extract Data from Sentinel-1 Products
#' @description Extracts data from downloaded Sentinel-1 zip files at tracking locations
#'
#' @param track A move2 object or data.frame with coordinates and timestamps
#' @param sentinel_zip_file Character. Path to downloaded Sentinel-1 zip file
#' @param polarisations Character vector. Polarization modes to extract (default: c("VV", "VH"))
#'
#' @return Data frame with extracted values for each tracking location on the given date,
#'         including event_id, timestamp, coordinates, and polarization values
#'
#' @details Processes Sentinel-1 SAR data by:
#'          1. Extracting the acquisition date from the filename
#'          2. Unzipping the archive to a temporary directory
#'          3. Finding and loading polarization data as rasters
#'          4. Extracting values at tracking locations
#'          5. Cleaning up temporary files
#'
#' @examples
#' # Extract VV and VH polarization data at tracking locations
#' s1_data <- sentinel_extract_S1_data(
#'   track = movement_data,
#'   sentinel_zip_file = "data/sentinel1/Sentinel1_GRD_20240215.zip",
#'   polarisations = c("VV", "VH")
#' )
sentinel_extract_S1_data <- function(track, sentinel_zip_file, polarisations = c("VV", "VH")) {
  # Extract the date from the file name
  sentinel_date <- ymd(as.numeric(strsplit(basename(sentinel_zip_file), "_")[[1]][3] %>% substr(1, 8)))
  
  # Unzip the file to a temporary directory
  temp_dir <- file.path(tempdir(), basename(sentinel_zip_file))
  unzip(sentinel_zip_file, exdir = temp_dir)
  
  # Find and load the S1 GRD data files for specified polarizations
  # Note: Sentinel-1 data typically has different file patterns than Sentinel-2
  raster_files <- list.files(temp_dir, 
                             pattern = paste0(".*measurement/*", paste(polarisations, collapse = "|"), ".*\\.tiff$"), 
                             recursive = TRUE, full.names = TRUE)
  
  if(length(raster_files) == 0) {
    stop("No matching polarization files found in the Sentinel-1 archive")
  }
  
  # Load polarization data as a raster stack
  sentinel_raster <- rast(raster_files)
  
  # Extract coordinates from track for the given date
  if(is(track, "move2")) {
    track_subset <- track[ymd(paste(year(track$timestamp), 
                                    sprintf("%02d", month(track$timestamp)), 
                                    sprintf("%02d", day(track$timestamp)))) == sentinel_date, ]
    coords <- as.matrix(st_coordinates(track_subset))
    event_id <- track_subset$event_id
    times <- mt_time(track_subset)
  } else if(is(track, "data.frame")) {
    track_subset <- track[ymd(paste(year(track$timestamp), 
                                    sprintf("%02d", month(track$timestamp)), 
                                    sprintf("%02d", day(track$timestamp)))) == sentinel_date, ]
    coords <- track_subset[, c("longitude", "latitude")]
    event_id <- track_subset$event_id
    times <- track_subset$timestamp
  }
  
  # Extract values at track locations
  extracted_values <- terra::extract(sentinel_raster, coords)
  
  # Create output data frame
  result <- data.frame(
    event_id = event_id,
    timestamp = times,
    longitude = coords[,1],
    latitude = coords[,2],
    extracted_values
  )
  
  # Clean up temporary directory
  unlink(temp_dir, recursive = TRUE)
  
  return(result)
}

#' @title Extract Data from Sentinel-2 Products
#' @description Extracts data from downloaded Sentinel-2 zip files at tracking locations
#'
#' @param track A move2 object or data.frame with coordinates and timestamps
#' @param sentinel_zip_file Character. Path to downloaded Sentinel-2 zip file
#' @param bands Character vector. Spectral bands to extract (default: c("B02", "B03", "B04", "B08"))
#'
#' @return Data frame with extracted values for each tracking location on the given date,
#'         including event_id, timestamp, coordinates, and band values
#'
#' @details Processes Sentinel-2 optical data by:
#'          1. Extracting the acquisition date from the filename
#'          2. Unzipping the archive to a temporary directory
#'          3. Finding and loading band data as rasters
#'          4. Extracting values at tracking locations
#'          5. Cleaning up temporary files
#'
#' @examples
#' # Extract RGB+NIR band data at tracking locations
#' s2_data <- sentinel_extract_S2_data(
#'   track = movement_data,
#'   sentinel_zip_file = "data/sentinel2/Sentinel2_Level-2A_20240215.zip",
#'   bands = c("B02", "B03", "B04", "B08")
#' )
sentinel_extract_S2_data <- function(track, sentinel_zip_file, bands = c("B02", "B03", "B04", "B08")) {
  # Extract the date from the file name
  sentinel_date <- ymd(as.numeric(strsplit(basename(sentinel_zip_file), "_")[[1]][3] %>% substr(1, 8)))
  
  # Unzip the file to a temporary directory
  temp_dir <- file.path(tempdir(), basename(sentinel_zip_file))
  unzip(sentinel_zip_file, exdir = temp_dir)
  
  # Load the raster data for specified bands
  raster_files <- list.files(temp_dir, pattern = paste0(paste(bands, collapse = "|"), "\\.tif$"), 
                             recursive = TRUE, full.names = TRUE)
  
  if(length(raster_files) == 0) {
    stop("No matching band files found in the Sentinel-2 archive")
  }
  
  # Load all bands as a raster stack
  sentinel_raster <- rast(raster_files)
  
  # Extract coordinates from track for the given date
  if(is(track, "move2")) {
    track_subset <- track[ymd(paste(year(track$timestamp), 
                                    sprintf("%02d", month(track$timestamp)), 
                                    sprintf("%02d", day(track$timestamp)))) == sentinel_date, ]
    coords <- as.matrix(st_coordinates(track_subset))
    event_id <- track_subset$event_id
    times <- mt_time(track_subset)
  } else if(is(track, "data.frame")) {
    track_subset <- track[ymd(paste(year(track$timestamp), 
                                    sprintf("%02d", month(track$timestamp)), 
                                    sprintf("%02d", day(track$timestamp)))) == sentinel_date, ]
    coords <- track_subset[, c("longitude", "latitude")]
    event_id <- track_subset$event_id
    times <- track_subset$timestamp
  }
  
  # Extract values at track locations
  extracted_values <- terra::extract(sentinel_raster, coords)
  
  # Create output data frame
  result <- data.frame(
    event_id = event_id,
    timestamp = times,
    longitude = coords[,1],
    latitude = coords[,2],
    extracted_values
  )
  
  # Clean up temporary directory
  unlink(temp_dir, recursive = TRUE)
  
  return(result)
}

#' @title Extract Data from Sentinel-3 Products
#' @description Extracts data from downloaded Sentinel-3 zip files at tracking locations
#'
#' @param track A move2 object or data.frame with coordinates and timestamps
#' @param sentinel_zip_file Character. Path to downloaded Sentinel-3 zip file
#' @param instrument Character. Sentinel-3 instrument: "OLCI", "SLSTR", or "SRAL"
#' @param variables Character vector. Variables to extract (if NULL, uses defaults)
#'
#' @return Data frame with extracted values for each tracking location on the given date,
#'         including event_id, timestamp, coordinates, and variable values
#'
#' @details Processes Sentinel-3 data by:
#'          1. Extracting the acquisition date from the filename
#'          2. Unzipping the archive to a temporary directory
#'          3. Finding and loading data based on instrument type
#'          4. Extracting values at tracking locations
#'          5. Cleaning up temporary files
#'
#' @examples
#' # Extract OLCI data at tracking locations
#' s3_data <- sentinel_extract_S3_data(
#'   track = movement_data,
#'   sentinel_zip_file = "data/sentinel3/Sentinel3_OLCI_EFR_L2_20240215.zip",
#'   instrument = "OLCI"
#' )
sentinel_extract_S3_data <- function(track, sentinel_zip_file, instrument = c("OLCI", "SLSTR", "SRAL"), variables = NULL) {
  instrument <- match.arg(instrument)
  
  # Extract the date from the file name
  sentinel_date <- ymd(as.numeric(strsplit(basename(sentinel_zip_file), "_")[[1]][4] %>% substr(1, 8)))
  
  # Unzip the file to a temporary directory
  temp_dir <- file.path(tempdir(), basename(sentinel_zip_file))
  unzip(sentinel_zip_file, exdir = temp_dir)
  
  # Set default variables if none specified
  if(is.null(variables)) {
    if(instrument == "OLCI") {
      variables <- c("Oa01", "Oa02", "Oa03", "Oa04", "Oa08")
    } else if(instrument == "SLSTR") {
      variables <- c("S1", "S2", "S3", "S4", "S5", "S6")
    } else if(instrument == "SRAL") {
      variables <- c("range_ocean", "significant_wave_height", "backscatter_coefficient")
    }
  }
  
  # Load the data files
  if(instrument %in% c("OLCI", "SLSTR")) {
    # For raster-based instruments
    raster_files <- list.files(temp_dir, pattern = paste0(paste(variables, collapse = "|"), "\\.(nc|tif)$"), 
                               recursive = TRUE, full.names = TRUE)
    
    if(length(raster_files) == 0) {
      stop("No matching data files found in the Sentinel-3 archive")
    }
    
    # Load all bands as a raster stack
    sentinel_raster <- rast(raster_files)
    
    # Extract coordinates from track for the given date
    if(is(track, "move2")) {
      track_subset <- track[ymd(paste(year(track$timestamp), 
                                      sprintf("%02d", month(track$timestamp)), 
                                      sprintf("%02d", day(track$timestamp)))) == sentinel_date, ]
      coords <- as.matrix(st_coordinates(track_subset))
      event_id <- track_subset$event_id
      times <- mt_time(track_subset)
    } else if(is(track, "data.frame")) {
      track_subset <- track[ymd(paste(year(track$timestamp), 
                                      sprintf("%02d", month(track$timestamp)), 
                                      sprintf("%02d", day(track$timestamp)))) == sentinel_date, ]
      coords <- track_subset[, c("longitude", "latitude")]
      event_id <- track_subset$event_id
      times <- track_subset$timestamp
    }
    
    # Extract values at track locations
    extracted_values <- terra::extract(sentinel_raster, coords)
  } else {
    # For SRAL (non-raster data)
    nc_files <- list.files(temp_dir, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)
    
    if(length(nc_files) == 0) {
      stop("No netCDF files found in the Sentinel-3 SRAL archive")
    }
    
    # Process netCDF files (this would require custom processing for SRAL data)
    # This is a placeholder - actual implementation would depend on SRAL data structure
    extracted_values <- data.frame(matrix(NA, nrow = nrow(track_subset), ncol = length(variables)))
    names(extracted_values) <- variables
  }
  
  # Create output data frame
  result <- data.frame(
    event_id = event_id,
    timestamp = times,
    longitude = coords[,1],
    latitude = coords[,2],
    extracted_values
  )
  
  # Clean up temporary directory
  unlink(temp_dir, recursive = TRUE)
  
  return(result)
}

#' @title Extract Data from Sentinel-5P Products
#' @description Extracts data from downloaded Sentinel-5P zip files at tracking locations
#'
#' @param track A move2 object or data.frame with coordinates and timestamps
#' @param sentinel_zip_file Character. Path to downloaded Sentinel-5P zip file
#' @param product_type Character. Atmospheric constituent: "NO2", "O3", "CO", "CH4", "SO2", 
#'                    "HCHO", "AER", or "CLOUD"
#' @param variables Character vector. Variables to extract (if NULL, uses defaults)
#'
#' @return Data frame with extracted values for each tracking location on the given date,
#'         including event_id, timestamp, coordinates, and atmospheric values
#'
#' @details Processes Sentinel-5P atmospheric data by:
#'          1. Extracting the acquisition date from the filename
#'          2. Unzipping the archive to a temporary directory
#'          3. Finding and loading NetCDF data
#'          4. Extracting values at tracking locations
#'          5. Cleaning up temporary files
#'
#' @examples
#' # Extract NO2 data at tracking locations
#' s5p_data <- sentinel_extract_S5P_data(
#'   track = movement_data,
#'   sentinel_zip_file = "data/sentinel5p/Sentinel5P_NO2_L2_20240215.zip",
#'   product_type = "NO2"
#' )
sentinel_extract_S5P_data <- function(track, sentinel_zip_file, product_type = c("NO2", "O3", "CO", "CH4", "SO2", "HCHO", "AER", "CLOUD"), variables = NULL) {
  product_type <- match.arg(product_type)
  
  # Extract the date from the file name
  sentinel_date <- ymd(as.numeric(strsplit(basename(sentinel_zip_file), "_")[[1]][3] %>% substr(1, 8)))
  
  # Unzip the file to a temporary directory
  temp_dir <- file.path(tempdir(), basename(sentinel_zip_file))
  unzip(sentinel_zip_file, exdir = temp_dir)
  
  # Set default variables based on product type if none specified
  if(is.null(variables)) {
    if(product_type == "NO2") {
      variables <- c("NO2_column_number_density", "tropospheric_NO2_column_number_density")
    } else if(product_type == "CO") {
      variables <- c("CO_column_number_density")
    } else if(product_type == "O3") {
      variables <- c("O3_column_number_density", "O3_effective_temperature")
    } else if(product_type == "CH4") {
      variables <- c("CH4_column_volume_mixing_ratio_dry_air")
    } else if(product_type == "SO2") {
      variables <- c("SO2_column_number_density")
    } else if(product_type == "HCHO") {
      variables <- c("tropospheric_HCHO_column_number_density")
    } else if(product_type == "AER") {
      variables <- c("aerosol_index", "aerosol_optical_depth")
    } else if(product_type == "CLOUD") {
      variables <- c("cloud_fraction", "cloud_top_pressure", "cloud_top_height")
    }
  }
  
  # Find NetCDF files containing variables
  nc_files <- list.files(temp_dir, pattern = "\\.nc$", recursive = TRUE, full.names = TRUE)
  
  if(length(nc_files) == 0) {
    stop("No NetCDF files found in the Sentinel-5P archive")
  }
  
  # Load data as a raster
  sentinel_raster <- rast(nc_files)
  
  # Extract coordinates from track for the given date
  if(is(track, "move2")) {
    track_subset <- track[ymd(paste(year(track$timestamp), 
                                    sprintf("%02d", month(track$timestamp)), 
                                    sprintf("%02d", day(track$timestamp)))) == sentinel_date, ]
    coords <- as.matrix(st_coordinates(track_subset))
    event_id <- track_subset$event_id
    times <- mt_time(track_subset)
  } else if(is(track, "data.frame")) {
    track_subset <- track[ymd(paste(year(track$timestamp), 
                                    sprintf("%02d", month(track$timestamp)), 
                                    sprintf("%02d", day(track$timestamp)))) == sentinel_date, ]
    coords <- track_subset[, c("longitude", "latitude")]
    event_id <- track_subset$event_id
    times <- track_subset$timestamp
  }
  
  # Extract values at track locations
  extracted_values <- terra::extract(sentinel_raster, coords)
  
  # Create output data frame
  result <- data.frame(
    event_id = event_id,
    timestamp = times,
    longitude = coords[,1],
    latitude = coords[,2],
    extracted_values
  )
  
  # Clean up temporary directory
  unlink(temp_dir, recursive = TRUE)
  
  return(result)
}
