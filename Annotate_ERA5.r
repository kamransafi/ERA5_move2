library(move2)
library(sf)
library(lubridate)
library(ecmwfr)
library(terra)
library(dplyr)
library(plyr)
library(doMC)
library(data.table)

# Main execution
source("/home/kami/Documents/Research/Projects/ERA5_move2/ERA5_function_libs.r")

# Set the directory path for the ERA5 files
era5_dir <- "/home/kami/Documents/Research/Projects/ERA5_move224442409"

# Load the tracking data
# Can be a move2 object or a data.frame with columns "eventID", "Long", "Lat", "timestamp", and "height_above_ellipsoid"
track_data <- readRDS(paste(era5_dir, "Track.rds", sep = "/"))

# Register a parallel back-end
registerDoMC(cores = parallel::detectCores() - 1)

# List all ERA5 files in the directory
era5_files <- list.files(era5_dir, pattern = "ERA5_.*\\.nc$", full.names = TRUE)

# Extract location data for each ERA5 file in parallel
start_time <- Sys.time()
extracted_data_list <- llply(era5_files, function(x) extract_locations_and_era5_data(
  track = track_data, 
  era5_file = x
), .parallel = TRUE)
end_time <- Sys.time()
print(paste("Time taken for extracting location data:", round(difftime(end_time, start_time, units="mins"), 2), "minutes"))

# Remove NULL elements from the list
extracted_data_list <- extracted_data_list[!sapply(extracted_data_list, is.null)]

# Remove list elements with empty data frames
extracted_data_list <- extracted_data_list[sapply(extracted_data_list, function(x) nrow(x$location_data) > 0)]

# Annotate with ERA5 environmental data in parallel
start_time <- Sys.time()
annotated_data_list <- llply(extracted_data_list, annotate_era5_data, .parallel = F)
end_time <- Sys.time()
print(paste("Time taken for annotating with ERA5 data:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes"))

# Combine annotated data into a single data frame
annotated_data <- data.table::rbindlist(annotated_data_list)

# Write the annotated data to a CSV file
data.table::fwrite(annotated_data, paste(era5_dir, "annotated_era5_data.csv", sep = "/"), row.names = FALSE)

# Alternatively, save as RDS for better preservation of data types
saveRDS(annotated_data, paste(era5_dir, "annotated_era5_data.rds", sep = "/"))
