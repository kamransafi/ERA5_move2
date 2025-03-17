# Main execution
library(move2)
library(sf)
library(lubridate)
library(ecmwfr)
library(terra)
library(dplyr)
library(data.table)

# Make sure your CDS credentials are set up
# wf_set_key(key = "your-personal-access-token")

# Study or track ID
studyID <- 24442409 
localID <- "3029" 
Local_path <- paste(getwd(), studyID, sep="")

# Create directory if needed
if (!dir.exists(Local_path)) dir.create(Local_path)

# Load or download track data
if (!file.exists(paste(Local_path, "Track.rds", sep="/"))) {
  Track <- movebank_download_study(
    study_id = studyID,
    tag_local_identifier = localID,
    sensor_type_id = c("gps"),
    attributes = "all"
  )
  saveRDS(Track, file = file.path(Local_path, "Track.rds"))
} else {
  Track <- readRDS(file = file.path(Local_path, "Track.rds"))
}

# Remove empty geometries
Traj <- Track[!st_is_empty(Track$geometry),]

# Original use case remains unchanged
req_table_move2 <- sentinel_create_request_table(
  track = movement_data,  # move2 object
  timeUnit = "week",
  area = "byTimeUnit",
  cloud_coverage = 20,
  area_ext = 0.05
)

# SF object with explicit time column
req_table_sf <- sentinel_create_request_table(
  track = sf_data,
  timeCol = "timestamp",  # Specify which column contains time information
  timeUnit = "week",
  area = "byTimeUnit",
  cloud_coverage = 20
)

# Dataframe with coordinates and time
req_table_df <- sentinel_create_request_table(
  track = my_dataframe,
  timeCol = "time",
  lonCol = "longitude", 
  latCol = "latitude",
  timeUnit = "day",
  area = "complete",
  cloud_coverage = 30
)

# Numeric vector extent [xmin, ymin, xmax, ymax] (west, south, east, north)
req_table_extent <- sentinel_create_request_table(
  track = c(-10, 35, 5, 45),  # [west, south, east, north]
  start_time = as.POSIXct("2025-01-01", tz = "UTC"),
  end_time = as.POSIXct("2025-01-31", tz = "UTC"),
  time_res = "1 day",     # Generate daily timestamps
  timeUnit = "week",
  area = "complete",
  cloud_coverage = 15
)

# Alternative with an sf bbox object
bbox <- st_bbox(c(xmin = -10, ymin = 35, xmax = 5, ymax = 45), crs = 4326)
req_table_bbox <- sentinel_create_request_table(
  track = bbox,
  start_time = as.POSIXct("2025-01-01", tz = "UTC"),
  end_time = as.POSIXct("2025-01-31", tz = "UTC"),
  timeUnit = "week",
  area = "complete"
)

