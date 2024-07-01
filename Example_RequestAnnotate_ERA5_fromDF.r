
#____________________________________
# Download/Import movement data ####
#____________________________________

### This part can/should be adjusted to your needs in regard to the movement data that you want to annotate ###
# What is important that at the end there is a Track.rds file stored in the Local_path
# This Track object can be either a move2 object according to the movebank standard, or a data.frame
# here I use the movebank_retrieve function to download the movement data from movebank with no further adjustments

### Load data from a study from movebank
library(move2)
Track <- movebank_retrieve(
  entity_type = "event",
  study_id = 24442409,
  tag_local_identifier = "3029",
  sensor_type_id = c("gps"),
  attributes = "all"
)

### Or import movement data from a local folder
# Track <- readRDS(file = "...Track.rds")


#________________________
# Download era5 data ####
#________________________

### Source necessary functions
source("ERA5_functions.r")

### Store credentials for the Copernicus Climate Data Store
CDS_USER_ID <- "..."
wf_set_key(user = CDS_USER_ID,
           key = "",
           service = "cds")

### Define the local folder where all the ERA5 data will be stored
localPath <- "ERA5_downloads"
if (!dir.exists()) dir.create(localPath)

### Set the request area
# this function takes either bbox object or a vector with 
# optional modification on the function. Add an optional argument coords to specify long and lat columns, that we can use to extract the bbox from a dataframe
areaS <- area2request(c(min(Track$location.long, na.rm=T), 
                        max(Track$location.long, na.rm=T), 
                        min(Track$location.lat, na.rm=T), 
                        max(Track$location.lat, na.rm=T)))

### Create the requests for the track
# this function takes the timestamp column
reqTable <- date2request(Track$timestamp)
print(paste("Number of days to be requested:", nrow(reqTable)))

### With this function we can download either single level products or pressure level products, e.g.
# Single level:
vars_singleLev <- c("2m_temperature", "boundary_layer_height", "instantaneous_moisture_flux",
                    "instantaneous_surface_sensible_heat_flux", "surface_pressure",
                    "10m_u_component_of_wind", "10m_v_component_of_wind")
# Pressure level:
# The var geopotential should always be downloaded among the press lev variables for correct estimation of the pressure level height
vars_pressLev <- c("Geopotential", "Specific humidity", "Temperature", "u_component_of_wind" , "v_component_of_wind")
levels <- c("650", "750", "850", "900", "950", "1000")

### Create two separate requests for the surface variables and the pressure levels variables
reqList_pressLev <- create_request_list(reqTable, areaS, datasetName = "reanalysis-era5-pressure-levels",
                                        vars = vars_pressLev, levels = levels)
reqList_singleLev <- create_request_list(reqTable, areaS, datasetName = "reanalysis-era5-single-levels",
                                         vars = vars_singleLev)

# remove partial downloads
unlink(list.files(localPath, pattern="ecmwf", full.names = TRUE))
# check for existing files and remove corresponding existing requests from the request list
existing_files <- list.files(localPath, pattern = "^download_ERA5_.*\\.nc$", full.names = TRUE)
reqList_pressLev <- reqList_pressLev[!vapply(reqList_pressLev, function(req) file.path(localPath, req$target) %in% existing_files, FUN.VALUE = logical(1))]
reqList_singleLev <- reqList_singleLev[!vapply(reqList_singleLev, function(req) file.path(localPath, req$target) %in% existing_files, FUN.VALUE = logical(1))]

print(paste("Number of total requests to be submitted:", length(reqList_singleLev)))

### Adjust the timeout option of curl in case of internet connection problems
options(timeout = 20000)
library(httr)
set_config(config(timeout = 300))
library(curl)
handle <- new_handle() ## curl::curl_options()
handle_setopt(handle, .list = list(connecttimeout = 300, timeout = 900))

### Submit requests to CDS API based on the list of requests
send_requests(reqList_pressLev, localPath, user=CDS_USER_ID,
              batch_size = 5, requestFileName = "all_requests_pressLev")
send_requests(reqList_singleLev, localPath, user=CDS_USER_ID, 
              batch_size = 15, requestFileName = "all_requests_singLev")



#___________________
# Annotate data ####
#___________________

### Import movement data
Track <- readRDS(file = "...Track.rds")

### Define height to download (only needed for pressure level products):
# We could either vertically interpolate the era5 values to the animal height
# or decide on a specific height at which we want data to be annotated
# in either case we can specify the column containing height (either a vector of one repeated value or a vector of values that are different at each location) to the annotation function
# Height should be expressed in metres above the ellipsoid
# Track$height.above.ellipsoid <- 1000

### List ERA5 files in the directory, separately for single levels and pressure levels
era5_files <- list.files(localPath, pattern = "^download_ERA5_.*singleLev.nc$", full.names = TRUE)
#era5_files <- list.files(localPath, pattern = "^download_ERA5_.*pressLev.nc$", full.names = TRUE)

### Extract data, here there is flexibility in specifying which columns contain the required information for annotation
# timestamp should be of class POSIXct and time zone UTC
# coordinates should be in decimal degrees, wgs84
# height should be in metres above the ellipsoid
# eventCol can be any columns with a unique row identifier

# Register a parallel back-end
registerDoMC(cores = parallel::detectCores() - 1)
# Extract track data for each ERA5 file in parallel
start_time <- Sys.time()
extracted_data_list <- llply(era5_files, function(x) {
  extract_track_and_era5_data(track=finalDf, 
                              era5_file=x,
                              eventCol = "mergingCol",
                              timeCol = "timestamp", 
                              coordsCol = c("location.long","location.lat"), 
                              heightCol = "height.above.ellipsoid")
}, .parallel = TRUE)
end_time <- Sys.time()
print(paste("Time taken for extracting track data:", round(difftime(end_time, start_time, units="mins"), 2), "minutes"))

# For every timestamp, we downloaded era5 data for the hour before and after to make time interpolation possible
# This means that we have some era5 data that do not have a corresponding section in the trajectory?
# Or why do we have elements with 0 rows in track data?
# Remove NULL elements from the list
extracted_data_list <- extracted_data_list[!sapply(extracted_data_list, is.null)]
# Also do not keep list elements with empty data frames
extracted_data_list <- extracted_data_list[sapply(extracted_data_list, function(x) nrow(x$track_data) > 0)]

### Annotate era5 data in parallel
annotationPath <- "..AnnotatedData/"
if (!dir.exists()) dir.create(localPath)

start_time <- Sys.time()
era5_data_list <- llply(extracted_data_list, 
                        annotate_era5_data, 
                        #pathToFolder = annotationPath, #only needed for really big datasets
                        .parallel = TRUE)
end_time <- Sys.time()
print(paste("Time taken for annotating era5 data:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes"))

# Combine era5 data of each subfile into a single data frame
era5_data <- rbindlist(era5_data_list)
summary(era5_data)

### Write the annotated data 
#fwrite(era5_data, paste(annotationPath, "era5_data_singleLev.csv", sep = "/"), row.names = FALSE) # as csv
saveRDS(era5_data, file = paste0(annotationPath,"era5_data_singleLev.rds")) # or as rds
saveRDS(era5_data, file = paste0(annotationPath,"era5_data_pressLev.rds"))


