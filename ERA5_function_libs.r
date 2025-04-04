#' Create formatted area request string from bounding box
#'
#' This internal helper function converts a bounding box or coordinates vector
#' into a formatted string for ERA5 API area requests.
#'
#' @param x A bounding box object or numeric vector of length 4 (xmin, xmax, ymin, ymax)
#' @param ext Numeric extension factor to expand the bounding box (default: 0.01)
#'
#' @return Character string in format "north/west/south/east" for ERA5 API
#' @keywords internal
.AreaRequest <- function(x, ext = 0.01) {
  #check if the input is a bbox
  if(is(x, "bbox")){
    bounds <- x
  }
  #check whether the input is a vector of length 4 
  if(is(x, "numeric") && length(x) == 4){
    bounds <- data.frame(xmin = x[1], xmax = x[2],  ymin = x[3], ymax = x[4])
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

#' Generate request table from date range
#'
#' This internal helper function creates a structured request table from timestamps,
#' organizing them by the specified time unit.
#'
#' @param x Vector of POSIXct timestamps
#' @param unit Time unit for organizing requests, one of: "day", "week", or "month"
#'
#' @return Data frame with structured request information by time unit
#' @keywords internal
.DateRequest <- function(x, unit) {
  if(!unit %in% c("day", "week", "month")){stop("Unit has to be either 'day', 'week', or 'month'")}
  DateTime <- sort(unique(c(floor_date(x, "hour"), floor_date(x+3600, "hour"))))
  reqTable <- data.frame(year=year(DateTime), month=sprintf("%02d", month(DateTime)), week=sprintf("%02d", week(DateTime)), days=sprintf("%02d", day(DateTime)), hours=sprintf("%02d",hour(DateTime)))
  if(unit=="day"){
    #split the table into a list with all the observations with the same year, month, day
    reqTable <- split(reqTable, apply(reqTable[,c("year", "month", "days")], 1, paste0, collapse=""))
    #collapse to one line per day
    reqTable <- do.call(rbind, lapply(reqTable, function(x) cbind(x[1,c("year", "month", "days")], hours= paste0(x[,"hours"], collapse=" "))))
  }
  if(unit=="week"){
    #split the table into a list with all the observations with the same year, month, week
    reqTable <- split(reqTable, apply(reqTable[,c("year", "month", "week")], 1, paste0, collapse=""))
    #collapse to one line per day
    reqTable <- do.call(rbind, lapply(reqTable, function(x) cbind(x[1,c("year", "month", "week")], days=  paste0(unique(x[,"days"]), collapse=" "), hours= paste0(unique(x[,"hours"]), collapse=" "))))
    row.names(reqTable) <- unlist(apply(reqTable, 1, function(x) paste0(x[1], x[2], "CW", x[3], collapse="")))
    reqTable <- reqTable[,c("year", "month", "days", "hours")]
  }
  if(unit == "month"){
    #split the table into a list with all the observations with the same year and month
    reqTable <- split(reqTable, apply(reqTable[,1:2], 1, paste0, collapse=""))
    #collapse to one line per month
    reqTable <- do.call(rbind, lapply(reqTable, function(x) cbind(x[1,c("year", "month")], days = paste0(unique(x[,"days"]), collapse=" "), hours = paste0(unique(x[,"hours"]), collapse=" "))))
  }
  return(reqTable)
}

#' Create ERA5 data request table from tracking data
#'
#' This function processes tracking data to create a structured request table
#' for ERA5 climate data. It divides the data according to the specified time unit
#' and handles spatial extents based on the tracking locations.
#'
#' @param track An sf object containing tracking data with timestamps
#' @param timeUnit Character string specifying the time unit for data organization.
#'                 One of: "day", "week", "month". Default: c("day", "week", "month")
#' @param area Character string specifying how to determine spatial extent.
#'             One of: "complete" (one area for all data) or "byTimeUnit" 
#'             (separate area per time unit). Default: c("complete", "byTimeUnit")
#' @param area_ext Numeric extension factor for the bounding box. Default: 0.01
#'
#' @return A data frame with ERA5 request information organized by specified time unit
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a request table with daily time units and complete spatial extent
#' req_table <- ERA5request(bird_track, timeUnit = "day", area = "complete")
#' }
ERA5request <- function(track, 
                        timeUnit = c("day", "week", "month"), 
                        area = c("complete", "byTimeUnit"),
                        area_ext=0.01) {
  if(!"sf" %in% class(track)){stop("This function requires 'track' to be a 'sf' object. You passed something else to it.")}
  if(any(st_is_empty(track$geometry))){stop("Your 'track' has missing coordinates! Remove rows with missing coordinates.")}
  if(grep("+proj=longlat", st_crs(track)$proj4string)!=1){stop("The projection of your track is not '+proj=longlat +datum=WGS84'. Please reproject!")}
  ts <- mt_time(track)
  #Split data by timeUnit
  reqTable <- .DateRequest(x=ts, unit=timeUnit)
  if(area=="complete"){
    reqTable$bbox <- .AreaRequest(x=st_bbox(track), ext=area_ext)
  }
  if(area=="byTimeUnit"){
    if(timeUnit=="day"){
      reqDT <- unlist(lapply(ts, function(x) paste0(year(x), sprintf("%02d", month(x)), sprintf("%02d", day(x)), collapse="")))
      reqTable$bbox <- lapply(rownames(reqTable), function(x) .AreaRequest(st_bbox(track[reqDT==x,]), ext=area_ext))
    }
    if(timeUnit=="week"){
      reqDT <- unlist(lapply(ts, function(x) paste0(year(x), sprintf("%02d", month(x)), "CW", sprintf("%02d", week(x)), collapse="")))
      reqTable$bbox <- lapply(rownames(reqTable), function(x) .AreaRequest(st_bbox(track[reqDT==x,]), ext=area_ext))
    }
    if(timeUnit=="month"){
      reqDT <- unlist(lapply(ts, function(x) paste0(year(x), sprintf("%02d", month(x)), collapse="")))
      reqTable$bbox <- lapply(rownames(reqTable), function(x) .AreaRequest(st_bbox(track[reqDT==x,]), ext=area_ext))
    }   
  }
  return(reqTable)
}


#' Format ERA5 pressure level data requests
#'
#' This internal helper function formats request tables into the specific structure
#' required for ERA5 pressure level data requests.
#'
#' @param reqTable Request table generated by ERA5request
#' @param vars Character vector of variable names to request
#' @param levels Character vector of pressure levels to request
#'
#' @return List of formatted ERA5 pressure level data requests
#' @keywords internal
.request_ERA5_pressure_levels <- function(reqTable,
                                          vars = c("geopotential", "u_component_of_wind", "v_component_of_wind"),
                                          levels = c("500", "550", "600", "650", "700", "750", "775", "800", "825",
                                                     "850", "875", "900", "925", "950", "975", "1000")) {
  reqList <- lapply(seq_len(nrow(reqTable)), function(i) {
    filename <- paste0("ERA5_pl_", rownames(reqTable[i,]), ".nc")
    list(
      product_type = "reanalysis",
      variable = vars,
      pressure_level = levels,
      year = reqTable$year[i],
      month = reqTable$month[i],
      day = unlist(strsplit(reqTable$days[i], " ")),
      time = sort(unlist(lapply(strsplit(reqTable$hours[i], " "), paste, ":00", sep = ""))),
      area = unlist(reqTable$bbox[i]),
      format = "netcdf",
      dataset_short_name =  "reanalysis-era5-pressure-levels",
      target = filename
    )})
  return(reqList)
}

#' Format ERA5 single level data requests
#'
#' This internal helper function formats request tables into the specific structure
#' required for ERA5 single level data requests.
#'
#' @param reqTable Request table generated by ERA5request
#' @param vars Character vector of variable names to request
#'
#' @return List of formatted ERA5 single level data requests
#' @keywords internal
.request_ERA5_single_level <- function(reqTable,
                                       vars = c('10m_u_component_of_wind', '10m_v_component_of_wind', '2m_temperature')) {
  reqList <- lapply(seq_len(nrow(reqTable)), function(i) {
    filename <- paste0("ERA5_sl_", rownames(reqTable[i,]), ".nc")
    list(
      product_type = "reanalysis",
      variable = vars,
      year = reqTable$year[i],
      month = reqTable$month[i],
      day = sort(unlist(strsplit(reqTable$days[i], " "))),
      time = sort(unlist(lapply(strsplit(reqTable$hours[i], " "), paste, ":00", sep = ""))),
      area = unlist(reqTable$bbox[i]),
      format = "netcdf",
      dataset_short_name =  "reanalysis-era5-single-levels",
      target = filename
    )})
  return(reqList)
}

#' Create a formatted list of ERA5 data requests
#'
#' This function converts a request table into properly formatted request lists
#' for the ERA5 API, supporting both single level and pressure level datasets.
#'
#' @param reqTable Request table generated by ERA5request
#' @param dataset Character string specifying the dataset type.
#'                One of: "ERA5 hourly data on single level" or 
#'                "ERA5 hourly data on pressure levels"
#' @param vars Character vector of variable names to request
#' @param levels Character vector of pressure levels (required for pressure level dataset)
#'
#' @return List of formatted ERA5 data requests ready for submission
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a request table
#' req_table <- ERA5request(bird_track, timeUnit = "day")
#' 
#' # Create single-level requests
#' single_reqs <- create_request_list(
#'   req_table, 
#'   dataset = "ERA5 hourly data on single level",
#'   vars = c('10m_u_component_of_wind', '10m_v_component_of_wind')
#' )
#' 
#' # Create pressure-level requests
#' pressure_reqs <- create_request_list(
#'   req_table,
#'   dataset = "ERA5 hourly data on pressure levels",
#'   vars = c("geopotential", "u_component_of_wind"),
#'   levels = c("500", "700", "850")
#' )
#' }
create_request_list <- function(reqTable, dataset = c("ERA5 hourly data on single level", "ERA5 hourly data on pressure levels"),
                                vars = NULL, levels = NULL) {
  # Check for weekly format explicitly (contains "CW")
  is_weekly <- any(grepl("CW", row.names(reqTable)))
  
  if(is_weekly) {
    message("Requested ERA5 data at a level of 1 week of data per request!")
  } else if(all(unique(nchar(row.names(reqTable))) == 6)) {
    message("Requested ERA5 data at a level of 1 month of data per request!")
  } else if(all(unique(nchar(row.names(reqTable))) == 8)) {
    message("Requested ERA5 data at a level of 1 day of data per request!")
  } else {
    stop("Your request table seems not to be correct. Row names should follow patterns for daily (YYYYMMDD), weekly (YYYYMMCWWW), or monthly (YYYYMM) requests.")
  }
  
  if(dataset == "ERA5 hourly data on single level") {
    message(paste0("Creating the request list for ", nrow(reqTable), " requests for single level ERA5 data."))
    reqList <- .request_ERA5_single_level(reqTable, vars)  
  }
  if(dataset == "ERA5 hourly data on pressure levels") {
    message(paste0("Creating the request list for ", nrow(reqTable), " requests for pressure level ERA5 data."))
    reqList <- .request_ERA5_pressure_levels(reqTable, vars, levels)  
  }
  return(reqList)
}

#' Send ERA5 data requests and download results
#'
#' This function submits ERA5 data requests to the Climate Data Store (CDS),
#' monitors their status, and downloads the resulting files. It includes
#' features for resuming interrupted downloads, error handling, and rate limiting.
#'
#' @param reqList List of request specifications created by create_request_list
#' @param localPath Character string specifying the directory to save downloaded files
#' @param batch_size Integer specifying the maximum number of concurrent requests (default: 25)
#' @param max_retries Integer specifying the maximum download retry attempts (default: 3)
#'
#' @return List with download statistics including total submitted, downloaded, and failed requests
#' @export
#'
#' @examples
#' \dontrun{
#' # Create and send requests
#' req_table <- ERA5request(bird_track, timeUnit = "day")
#' reqs <- create_request_list(
#'   req_table, 
#'   dataset = "ERA5 hourly data on single level",
#'   vars = c('10m_u_component_of_wind', '2m_temperature')
#' )
#' 
#' # Send requests and download data
#' download_stats <- send_requests(reqs, localPath = "data/ERA5")
#' }
send_requests <- function(reqList, localPath, batch_size = 25, max_retries = 3) {
  # Initialize consecutive error tracking for progressive backoff
  consecutive_errors <- 0
  max_consecutive_errors <- 10
  base_wait_time <- 30  # seconds
  current_wait_time <- base_wait_time
  
  # Remove the requests that have already been downloaded
  pattern <- "^ERA5_.*\\.nc$"  # You might need to adjust this pattern
  existing_files <- list.files(localPath, pattern = pattern, full.names = TRUE)
  reqList <- reqList[!vapply(reqList, function(req) file.path(localPath, req$target) %in% existing_files, FUN.VALUE = logical(1))]
  
  # Check for a previous request list 
  if(file.exists(paste(localPath, "all_requests.rds", sep="/"))) {
    all_requests <- readRDS(paste(localPath, "all_requests.rds", sep="/"))
    # Take as live requests only those that have not yet completed with a local downloaded file
    live_requests <- all_requests[!unlist(lapply(lapply(all_requests, function(x) x$get_request()$target), 
                                                 function(y) y %in% list.files(localPath, pattern = pattern, full.names = FALSE)))]
    # Clear the all requests lists only for those that are still required to be downloaded
    all_requests <- live_requests
  } else {
    all_requests <- list()
    live_requests <- list()
  }
  
  # Initialize failed_transfers list to track download attempts
  failed_transfers <- list()
  
  dld_requests <- list()
  inheritedJobs <- length(all_requests)
  # Remove already requested but not yet downloaded requests from the list of pending requests
  reqList <- reqList[!unlist(lapply(reqList, function(x) x$target)) %in% lapply(live_requests, function(y) y$get_request()$target)]
  nJobs <- inheritedJobs + length(reqList)
  message(paste("Total of ", nJobs, " pending requests remain after removing previously completed jobs and adding still pending requests on CDS!", sep=""))
  
  # Variables for rate limiting
  request_interval <- 5  # Seconds between submitting new requests
  download_interval <- 10 # Seconds between downloading files
  backoff_time <- 60     # Initial backoff time for rate limit errors (1 minute)
  max_backoff <- 900     # Maximum backoff time (15 minutes)
  
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
              message(paste("Rate limit reached. Waiting ", backoff_time, " seconds before retrying..."))
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
    
    # Update the status of the live requests - WITH ENHANCED ERROR HANDLING
    live_requests <- lapply(live_requests, function(x) {
      tryCatch({
        # First try to update the status
        updated_x <- tryCatch({
          x$update_status()
        }, error = function(e) {
          message(paste("Inner update error:", e$message))
          return(NULL)  # Return NULL if inner update fails
        })
        
        # Check if the result is NULL or has length zero before returning
        if (is.null(updated_x) || length(updated_x) == 0) {
          message("Received empty response from update_status(), maintaining previous state")
          return(x)  # Return unchanged object
        }
        
        return(updated_x)
      }, error = function(e) {
        if (grepl("rate limit", e$message, ignore.case = TRUE)) {
          message(paste("Rate limit reached during status check. Waiting ", backoff_time, " seconds..."))
          Sys.sleep(backoff_time)
          backoff_time <- min(backoff_time * 2, max_backoff)
        } else {
          message(paste("Error updating status:", e$message))
        }
        return(x)  # Return unchanged request object if update fails
      })
    })
    
    # Print current status values with improved error handling
    current_statuses <- unlist(lapply(live_requests, function(x) {
      tryCatch({
        status <- x$get_status()
        if (is.null(status) || length(status) == 0) {
          return("unknown")  # Return a default status when empty
        }
        return(status)
      }, error = function(e) {
        return("unknown")
      })
    }))
    
    # Apply progressive backoff for connection issues
    if (any(current_statuses == "unknown")) {
      consecutive_errors <- consecutive_errors + 1
      current_wait_time <- min(base_wait_time * 2^(consecutive_errors/3), 300)  # Cap at 5 minutes
      message(paste("Consecutive errors:", consecutive_errors, "- Waiting ", current_wait_time, " seconds"))
    } else {
      consecutive_errors <- max(0, consecutive_errors - 1)  # Gradually reduce count on success
      current_wait_time <- base_wait_time
    }
    
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
      # Check if there are any failed transfers to retry
      if(length(failed_transfers) > 0) {
        message(paste("Retrying", length(failed_transfers), "failed transfers..."))
        retry_list <- names(failed_transfers)[failed_transfers < max_retries]
        
        if(length(retry_list) > 0) {
          # Find the corresponding requests in live_requests
          for(req_id in retry_list) {
            retry_idx <- which(sapply(live_requests, function(x) {
              tryCatch({
                return(x$get_request()$target == req_id)
              }, error = function(e) {
                return(FALSE)
              })
            }))
            
            if(length(retry_idx) > 0) {
              message(paste("Retrying download of:", req_id))
              tryCatch({
                live_requests[[retry_idx]]$transfer()
                # Update retry count
                failed_transfers[[req_id]] <- failed_transfers[[req_id]] + 1
                Sys.sleep(download_interval)
              }, error = function(e) {
                message(paste("Error retrying download:", e$message))
              })
            }
          }
        }
      }
      
      message("\nWaiting for CDS for ", current_wait_time, " seconds.")
      Sys.sleep(current_wait_time)
      message("------------------------------------------------\n")
    } else {
      message(paste("Found", length(dldList), "completed requests. Downloading..."))
      
      # Download completed requests one by one with delays between them
      for (idx in dldList) {
        req_target <- tryCatch({
          live_requests[[idx]]$get_request()$target
        }, error = function(e) {
          return(NULL)
        })
        
        if(!is.null(req_target)) {
          message(paste("Downloading:", req_target))
          download_success <- tryCatch({
            live_requests[[idx]]$transfer()
            # Verify the file actually downloaded
            if(file.exists(file.path(localPath, req_target))) {
              # Success - delete the request
              live_requests[[idx]]$delete()
              TRUE
            } else {
              # File didn't appear - mark as failed
              FALSE
            }
          }, error = function(e) {
            message(paste("Error downloading file:", e$message))
            # Add to failed transfers for retry
            return(FALSE)
          })
          
          if(!download_success) {
            # Track failed transfers to retry later
            if(is.null(failed_transfers[[req_target]])) {
              failed_transfers[[req_target]] <- 1
            } else {
              failed_transfers[[req_target]] <- failed_transfers[[req_target]] + 1
            }
            message(paste("Download failed for", req_target, "- will retry later (attempt", failed_transfers[[req_target]], "of", max_retries, ")"))
          }
          
          # Add delay between downloads to avoid rate limiting
          Sys.sleep(download_interval)
        }
      }
      
      # Remove successfully downloaded requests from the active list
      downloaded_files <- list.files(localPath, pattern = pattern, full.names = FALSE)
      to_remove <- which(unlist(lapply(live_requests, function(x) {
        target <- tryCatch(x$get_request()$target, error = function(e) return(""))
        return(target %in% downloaded_files)
      })))
      
      if(length(to_remove) > 0) {
        live_requests <- live_requests[-to_remove]
      }
      
      # Reset backoff time after successful downloads
      consecutive_errors <- 0
      current_wait_time <- base_wait_time
    }
    
    message(paste("Number of requests alive on CDS: ", length(live_requests)))
    # Get current list of downloaded files
    dld_requests <- all_requests[unlist(lapply(all_requests, function(x) {
      target <- tryCatch(x$get_request()$target, error = function(e) return(""))
      return(target %in% list.files(localPath, pattern = pattern, full.names = FALSE))
    }))]
    message(paste("Downloaded ", length(dld_requests), " of ", nJobs, " pending requests."))
    
    # Reset backoff time if we've made progress
    if(length(dld_requests) > 0) {
      backoff_time <- 60  # Reset to initial value
    }
  }
  
  # Final check for any completed jobs that weren't downloaded
  message("Performing final check for completed jobs...")
  # Query CDS directly for any completed jobs that haven't been downloaded
  # This requires a call to the CDS API to list jobs - not directly possible with ecmwfr
  # So we rely on our tracking within this function
  
  remaining_requests <- all_requests[!unlist(lapply(all_requests, function(x) {
    target <- tryCatch(x$get_request()$target, error = function(e) return(""))
    return(target %in% list.files(localPath, pattern = pattern, full.names = FALSE))
  }))]
  
  if(length(remaining_requests) > 0) {
    message(paste("Found", length(remaining_requests), "remaining requests - checking status..."))
    for(req in remaining_requests) {
      status <- tryCatch(req$get_status(), error = function(e) return("unknown"))
      if(tolower(status) %in% tolower(completed_statuses)) {
        message(paste("Found completed job still waiting for download:", req$get_request()$target))
        tryCatch({
          req$transfer()
          message("Successfully downloaded.")
        }, error = function(e) {
          message(paste("Failed to download:", e$message))
        })
        Sys.sleep(download_interval)
      }
    }
  }
  
  message("FINISHED!")
  message(paste("Total requests submitted:", length(all_requests)))
  message(paste("Total files downloaded:", length(list.files(localPath, pattern = pattern))))
  unlink(paste(localPath, "all_requests.rds", sep="/"))
  
  # Return statistics
  return(list(
    total_submitted = length(all_requests),
    total_downloaded = length(list.files(localPath, pattern = pattern)),
    failed_downloads = names(failed_transfers)[failed_transfers >= max_retries]
  ))
}


#' Extract location data for specific dates from ERA5 files
#'
#' This function extracts matching data from tracking data and ERA5 climate files
#' based on date. It supports both move2 objects and data frames as input.
#'
#' @param track A move2 object or data frame containing tracking data
#' @param era5_file File name and path to the ERA5 file (the idea is to to loop through all the files)
#' @param eventCol Column name containing event IDs (default: NULL)
#'                 For move2 objects, will automatically check for "event_id" if "event.id" is not found
#' @param timeCol Column name containing timestamps (as POSIXct) in the data.frame (default: NULL)
#'                For move2 objects, will automatically extract "timestamp"
#' @param coordsCol Column names containing coordinates (in EPSG 4326) in the data.frame, vector of two strings (default: NULL)
#'                For move2 objects, will automatically extract the coordinates from the geometry column
#' @param heightCol Column name containing height data  in the data.frame (default: NULL)
#'                  For move2 objects, will automatically check for "height.above.ellipsoid" if default not found
#'
#' @return A list containing:
#' \itemize{
#' \item \code{era5_file_name}: Name of the ERA5 file
#' \item \code{dataset}: Dataset name extracted from file name
#' \item \code{era5_date_start}: Start date of the ERA5 file
#' \item \code{era5_date_end}: End date of the ERA5 file
#' \item \code{file_type}: Type of ERA5 file (daily/weekly/monthly)
#' \item \code{location_data}: Filtered tracking data with columns:
#'    * event_id
#'    * Long (longitude)
#'    * Lat (latitude)
#'    * timestamp
#'    * heights (optional)
#' }
extract_locations_and_era5_data <- function(track, era5_file, 
                                            eventCol = "event.id",
                                            timeCol = "timestamp", 
                                            coordsCol = c("location.long","location.lat"), 
                                            heightCol = "height_above_ellipsoid") {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  
  # Extract the date from the ERA5 file name
  era5_filename <- basename(era5_file)
  date_part <- strsplit(era5_filename, "_")[[1]][3]
  date_part <- sub("\\.nc$", "", date_part)  # Remove .nc extension if present
  
  # Determine file type and extract date range
  if (nchar(date_part) == 8) {
    # Daily file (YYYYMMDD)
    file_type <- "daily"
    era5_date <- lubridate::ymd(date_part)
    date_start <- era5_date
    date_end <- era5_date
  } else if (grepl("CW", date_part)) {
    # Weekly file (YYYYMMCWWW)
    file_type <- "weekly"
    year_month <- substr(date_part, 1, 6)
    week_part <- regmatches(date_part, regexpr("CW\\d{2}", date_part))
    week_num <- as.numeric(substr(week_part, 3, 4))
    
    year <- as.numeric(substr(year_month, 1, 4))
    month <- as.numeric(substr(year_month, 5, 6))
    
    # Calculate the date range using ISO week date system
    first_day_of_month <- lubridate::ymd(paste0(year, "-", month, "-01"))
    days_in_month <- seq(first_day_of_month, 
                         lubridate::ceiling_date(first_day_of_month, "month") - lubridate::days(1), 
                         by = "day")
    
    # Filter days for the specified week number
    week_days <- days_in_month[lubridate::week(days_in_month) == week_num]
    
    # Handle weeks spanning multiple months if needed
    if (length(week_days) == 0) {
      days_before <- seq(first_day_of_month - lubridate::days(7), 
                         first_day_of_month - lubridate::days(1), 
                         by = "day")
      days_after <- seq(lubridate::ceiling_date(first_day_of_month, "month"), 
                        lubridate::ceiling_date(first_day_of_month, "month") + lubridate::days(7), 
                        by = "day")
      
      week_days <- c(days_before[lubridate::week(days_before) == week_num],
                     days_after[lubridate::week(days_after) == week_num])
    }
    
    if (length(week_days) == 0) {
      stop(paste0("Could not determine date range for week ", week_num, " in ", year, "-", month))
    }
    
    date_start <- min(week_days)
    date_end <- max(week_days)
  } else if (nchar(date_part) == 6) {
    # Monthly file (YYYYMM)
    file_type <- "monthly"
    year <- as.numeric(substr(date_part, 1, 4))
    month <- as.numeric(substr(date_part, 5, 6))
    
    date_start <- lubridate::ymd(paste0(year, "-", month, "-01"))
    date_end <- lubridate::ceiling_date(date_start, "month") - lubridate::days(1)
  } else {
    stop(paste0("Unknown ERA5 file format: ", era5_filename))
  }
  
  # Filter tracking data based on the identified date range
  if (inherits(track, "move2")) {
    daily_track <- track %>%
      dplyr::filter(as.Date(timestamp) >= date_start & as.Date(timestamp) <= date_end)
    
    if (nrow(daily_track) == 0) {
      warning(paste0("No data found for date range: ", date_start, " to ", date_end))
      return(NULL)
    }
    
    available_cols <- names(daily_track)
    
    # Handle different event ID column naming conventions in move2 objects
    event_column <- eventCol
    if (!event_column %in% available_cols) {
      alt_event_cols <- c("event.id", "event_id")
      found_event_cols <- alt_event_cols[alt_event_cols %in% available_cols]
      
      if (length(found_event_cols) > 0) {
        event_column <- found_event_cols[1]
        message(paste0("Using '", event_column, "' as event ID column instead of '", eventCol, "'"))
        event_id <- daily_track %>% dplyr::pull(!!event_column)
      } else {
        warning(paste0("No event ID column found. Checked: ", paste(c(eventCol, alt_event_cols), collapse = ", ")))
        event_id <- seq_len(nrow(daily_track))  # Fallback to row numbers if no event ID found
      }
    } else {
      event_id <- daily_track %>% dplyr::pull(!!event_column)
    }
    
    height_column <- heightCol
    if (!height_column %in% available_cols) {
      alt_height_cols <- c("height_above_ellipsoid", "height.above.ellipsoid")
      found_height_cols <- alt_height_cols[alt_height_cols %in% available_cols]
      
      if (length(found_height_cols) > 0) {
        height_column <- found_height_cols[1]
        message(paste0("Using '", height_column, "' as height column instead of '", heightCol, "'"))
        heights <- daily_track %>% dplyr::pull(!!height_column)
      } else {
        warning(paste0("No height data found. Checked columns: ", paste(c(heightCol, alt_height_cols), collapse = ", ")))
        heights <- rep(NA, nrow(daily_track))
      }
    } else {
      heights <- daily_track %>% dplyr::pull(!!height_column)
    }
    
    coords <- sf::st_coordinates(daily_track)
    times <- mt_time(daily_track)
    
  } else if (inherits(track, "data.frame")) {
    daily_track <- track %>%
      dplyr::filter(as.Date(!!dplyr::sym(timeCol)) >= date_start & 
                      as.Date(!!dplyr::sym(timeCol)) <= date_end)
    
    if (nrow(daily_track) == 0) {
      warning(paste0("No data found for date range: ", date_start, " to ", date_end))
      return(NULL)
    }
    
    if (!eventCol %in% names(daily_track)) {
      warning(paste0("Event ID column '", eventCol, "' not found in data frame."))
      event_id <- seq_len(nrow(daily_track))  
    } else {
      event_id <- daily_track[[eventCol]]
    }
    
    coords <- as.matrix(daily_track[, coordsCol])
    times <- daily_track[[timeCol]]
    
    if (!heightCol %in% names(daily_track)) {
      warning(paste0("No height data found. Height column '", heightCol, "' not found in data frame."))
      heights <- rep(NA, nrow(daily_track))
    } else {
      heights <- daily_track[[heightCol]]
    }
  } else {
    stop(paste0("Track must be a move2 object or a data.frame"))
  }
  
  location_data <- data.frame(
    event_id = event_id,
    Long = coords[, 1],
    Lat = coords[, 2],
    timestamp = times,
    heights = heights
  )
  
  location_data_required <- location_data %>%
    dplyr::select(-heights)
  
  complete_indices <- complete.cases(location_data_required)
  
  if (!all(complete_indices)) {
    warning(paste0("Removed ", sum(!complete_indices), " rows with missing required values (excluding heights)."))
  }
  
  location_data <- location_data[complete_indices, ]
  
  extracted_data <- list(
    era5_file_name = era5_file,
    dataset = era5_filename,
    era5_date_start = date_start,
    era5_date_end = date_end,
    file_type = file_type,
    location_data = location_data
  )
  
  return(extracted_data)
}

#' Interpolate ERA5 data in time
#'
#' Helper function to perform temporal interpolation for ERA5 variables
#'
#' @param interpolated_in_space Data frame with spatially interpolated ERA5 variables
#' @param extracted_data List containing location data and timestamps
#'
#' @return Data frame with temporally interpolated values
.interpolate_in_time <- function(interpolated_inSpace, extracted_data) {
  variable_names <- gsub("_valid_time=.*", "", names(interpolated_inSpace))
  times <- as.POSIXct(
    as.numeric(gsub(".*valid_time=([0-9]+).*", "\\1", names(interpolated_inSpace))),
    origin = "1970-01-01",
    tz = "UTC"
  )
  df <- data.frame(vars = variable_names, time=times)
  df_sorted <- df[order(df$vars, df$time), ]
  layers <- unique(variable_names)
  tInterpolate <-   lapply(layers, function(x) {
    tmp <- interpolated_inSpace[, as.numeric(row.names(df_sorted[df_sorted$vars == x, ]))]
    vals <- lapply(1:nrow(tmp), function(i){
      layer_times <- as.POSIXct(
        as.numeric(gsub(".*valid_time=([0-9]+).*", "\\1", names(tmp[i,]))),
        origin = "1970-01-01",
        tz = "UTC"
      )
      layer_values <- as.numeric(tmp[i,])
      est <- approx(layer_times, layer_values, xout = extracted_data$location_data$timestamp[i])$y
      return(est)
      })
    return(unlist(vals))
    })
  tInterpolate <- do.call(cbind, tInterpolate)
  tInterpolate <- as.data.frame(tInterpolate)
  names(tInterpolate) <- layers
  return(tInterpolate)
}


#' Interpolate ERA5 pressure level data in height dimension
#'
#' This helper function performs vertical interpolation for ERA5 variables
#' based on geopotential height and location height.
#'
#' @param interpolated_in_time Data frame with temporally interpolated ERA5 variables
#' @param variable_name Base name of the variable to interpolate (e.g., "u", "v", "t")
#' @param location_heights Vector of heights for each location in meters
#' @param variable_longname Long name for the output column (for labeling)
#'
#' @return Data frame with vertically interpolated values
.interpolate_in_height <- function(interpolated_in_time) {
  var_names <- unique(gsub("_pressure.*", "", names(interpolated_inTime)))
  var_names <- var_names[!grepl("^z", var_names)]
  location_heights <- interpolated_in_time$heights
  # For each location, interpolate the variable at its specific height
  interpolated_var <- lapply(1:nrow(interpolated_in_time), function(x) {
    tryCatch({
      # Get geopotential heights by dividing by gravity constant (9.80665 m/s²)
      geopotential_heights <- interpolated_in_time[x, grep("^z_pressure_level=", names(interpolated_in_time))] / 9.80665
      
      # Get variable values at each pressure level - match the exact pattern in column names
      ints <- lapply(var_names, function(var) {
        var_values <- interpolated_in_time[x, grep(paste0("^", var, "_pressure_level="), names(interpolated_in_time))]
        # Ensure values are sorted by height (important for interpolation)
        pressure_levels <- as.numeric(gsub(".*pressure_level=([0-9]+)$", "\\1", names(interpolated_in_time)[grep(paste0("^", var, "_pressure_level="), names(interpolated_in_time))]))
        
        # Skip interpolation if any values are NA
        if (any(is.na(geopotential_heights)) || any(is.na(var_values)) || is.na(target_height)) {
          return(NA)
        }
        
        # Interpolate to the exact height
        ret <- approx(
          x = geopotential_heights,
          y = var_values,
          xout = location_heights[x],
          rule = 2  # rule=2 allows extrapolation if needed
        )$y
        return(ret)
      })
      df <- data.frame(t(unlist(ints)))
      names(df) <- var_names
      return(df)
      })
      })
  return(rbindlist(interpolated_var))
}


#' Annotate bird track with ERA5 data
#'
#' This function annotates bird tracking data with environmental variables from ERA5 
#' reanalysis data. It performs spatial and temporal interpolation of ERA5 variables 
#' to match the exact locations and timestamps in the bird tracking data. For pressure 
#' level data, it also performs vertical interpolation based on location heights.
#'
#' @param extracted_data A list containing location data and ERA5 file information. Must include:
#'   \itemize{
#'     \item location_data: Data frame with bird tracking data including Lat, Long, timestamp columns
#'     \item era5_file_name: Path to the ERA5 NetCDF file
#'     \item era5_date: Date string for the ERA5 data
#'     \item dataset: String indicating dataset type (includes "pl" for pressure level data)
#'   }
#'
#' @return A data frame containing the original location data augmented with interpolated 
#'   environmental variables from ERA5 data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Prepare input data
#' data <- list(
#'   location_data = bird_tracks,
#'   era5_file_name = "path/to/ERA5_pl_20230101.nc",
#'   era5_date = "20230101",
#'   dataset = "pl"
#' )
#' 
#' # Annotate with ERA5 data
#' annotated_tracks <- annotate_era5_data(data, "results/annotated")
#' }
annotate_era5_data <- function(extracted_data) {
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
  
  # temporal interpolation
  interpolated_inTime <- .interpolate_in_time(interpolated_inSpace, extracted_data)
  
  # Combine location data with interpolated results
  annotated_data <- cbind(extracted_data$location_data, interpolated_inTime)
  
  ### For pressure level variables, interpolate in height using helper function (if needed)
  if (grepl("pl", extracted_data$dataset)) {
    # Convert heights if they contain units in string format
    if (is.character(annotated_data$heights)) {
      annotated_data$heights <- as.numeric(gsub("\\s*\\[.*\\]", "", annotated_data$heights))
    }
    #interpolate to the height
    interpolated_inHeight <- .interpolate_in_height(annotated_data)
    #combine the data 
    annotated_data <- cbind(extracted_data$location_data, interpolated_inHeight)
  } 
  #return the interpolated data
  return(annotated_data)
}
