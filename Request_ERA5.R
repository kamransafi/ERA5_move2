# Main execution
# Load required packages
library(move2)
library(sf)
library(lubridate)
library(ecmwfr)  # Make sure this is version 2.0.0+
library(terra)
library(dplyr)
library(data.table)
library(lubridate)
library(plyr)
library(doMC)

source("./ERA5_function_libs.r")

# No need to specify CDS_USER_ID as the authentication is now handled through PAT
# Make sure you've set your Personal Access Token with:
# wf_set_key(key = "...")

#choose the key ring name that you want to use
keyring::key_list()
options("move2_movebank_key_name" = "KamiMovebank")


# ID of the study in movebank or a study name needed for movebank_download_study but also for naming the data folder
studyID <- 24442409 
# ID of the local tag in movebank needed for the move2 function
localID <- "3029" 
#Local folder where all the ERA5 data and the track as Track.rds will be stored
Local_path <- paste(getwd(), studyID, sep="")

#if the directory hasn't been created yet, create it
if (!dir.exists(Local_path)) dir.create(Local_path)

### This part can/should be adjusted to your needs in regard to the movement data that you want to annotate ###
# What is important that at the end there is a Track.rds file stored in the Local_path
# This Track object can be either a move2 object according to the movebank standard, or
# a data.frame with columns "event_id", "Long", "Lat", "timestamp", and "height_above_ellipsoid".

# check if movement data has been stored in the working directory and skip downloading from movebank if it has
if (!file.exists(paste(Local_path, "Track.rds", sep="/"))) {
  # Load data from a study from movebank
  Track <- movebank_download_study(
    study_id = studyID,
    tag_local_identifier = localID,
    sensor_type_id = c("gps"),
    attributes = "all"
    
  ) #,'license-md5'='3584fcbaab31e6a2f9e1dccaeeb0b97f'
  #store the track, which will be needed for the annotations later
  saveRDS(Track, file = file.path(Local_path, "Track.rds"))
} else {
  Track <- readRDS(file = file.path(Local_path, "Track.rds"))
}
### End of the part that can/should be adjusted ###
# Remove empty geometries
Traj <- Track[!st_is_empty(Track$geometry),]

# Create request table
t0 <- Sys.time()
reqTable <- ERA5request(Traj, timeUnit = "week", area="byTimeUnit", area_ext = 0.01)
difftime(Sys.time(), t0, units = "secs")

print(paste("Number of requests:", nrow(reqTable)))
if(!file.exists(paste(Local_path, "requestTable.csv", sep="/"))) {
  fwrite(reqTable, paste(Local_path, "requestTable.csv", sep="/"))
}

# Create request list - structure remains the same
requests <- create_request_list(reqTable, 
                                dataset="ERA5 hourly data on pressure levels", 
                                vars=c("geopotential", "u_component_of_wind", "v_component_of_wind"), 
                                levels = c("500", "550", "600", "650", "700", "750", "775", 
                                           "800", "825", "850", "875", "900", "925", "950", "975", "1000"))

# Check for existing files and remove corresponding existing requests from the request list
existing_files <- list.files(Local_path, pattern = "^ERA5_.*\\.nc$", full.names = TRUE)
reqTable$file_exists <- vapply(requests, function(req) file.path(Local_path, req$target) %in% existing_files, FUN.VALUE = logical(1))
fwrite(reqTable, paste(Local_path, "requestTable.csv", sep="/"))

# Check for which are already downloaded
Requests2Send <- requests[!reqTable$file_exists]
print(paste("Number of total requests to be submitted:", length(Requests2Send)))

# Remove partial downloads
unlink(list.files(Local_path, pattern="ecmwf", full.names = TRUE))

  # Submit requests to CDS API - no need for user parameter anymore
  t0 <- Sys.time()
  send_requests(Requests2Send, localPath=Local_path, batch_size = 25)
  difftime(Sys.time(), t0, units = "hours")
