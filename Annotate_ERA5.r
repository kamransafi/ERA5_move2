# Main execution
source(".../ERA5_functions.r")

# Set the directory path for the ERA5 files
era5_dir <- ".../Study24442409"

# Load the bird track data
bird_track <- readRDS(paste(era5_dir, "Track.rds", sep = "/"))

# Register a parallel back-end
registerDoMC(cores = parallel::detectCores() - 1)

# List all ERA5 files in the directory
era5_files <- list.files(era5_dir, pattern = "^download_ERA5_.*\\.nc$", full.names = TRUE)

# Extract track data for each ERA5 file in parallel
start_time <- Sys.time()
extracted_data_list <- llply(era5_files, function(x) extract_track_and_era5_data(bird_track, x), .parallel = TRUE)
end_time <- Sys.time()
print(paste("Time taken for extracting track data:", round(difftime(end_time, start_time, units="mins"), 2), "minutes"))
# Remove NULL elements from the list)
extracted_data_list <- extracted_data_list[!sapply(extracted_data_list, is.null)]
# Also do not keep list elements with empty data frames
extracted_data_list <- extracted_data_list[sapply(extracted_data_list, function(x) nrow(x$bird_track_data) > 0)]
# Annotate wind data in parallel
start_time <- Sys.time()
wind_data_list <- llply(extracted_data_list, annotate_wind_data, .parallel = TRUE)
end_time <- Sys.time()
print(paste("Time taken for annotating wind data:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes"))

# Combine wind data into a single data frame
wind_data <- rbindlist(wind_data_list)

# Write the wind data to a CSV file using fwrite
fwrite(wind_data, paste(era5_dir, "wind_data.csv", sep = "/"), row.names = FALSE)
