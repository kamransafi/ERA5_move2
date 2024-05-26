# Main execution
source("/home/kami/Documents/Research/Data_Scripts/Scripts/ERA5/ERA5_functions.r")

# Load one individual stork from a study from movebank
Track <- movebank_download_study(
  study_id = 24442409,
  tag_local_identifier = "3029",
  sensor_type_id = c("gps", "orientation"),
  attributes = "all",
  'license-md5' = '65bc6ae51d40c3a3a555ec6d0b49f221'
)

# Set the local path for data storage
mkdir("/home/kami/Documents/Research/Data_Scripts/Data/Study24442409")
localPath <- "/home/kami/Documents/Research/Data_Scripts/Data/Study24442409"
saveRDS(Track, file = file.path(localPath, "Track.rds"))

#plot(Track, max.plot = 1)
areaS <- area2request(Track)

#create the requests for the track
reqTable <- date2request(Track)
print(paste("Number of days to be requested:", nrow(reqTable)))

# Check for existing files and remove corresponding existing requests
existing_files <- list.files(localPath, pattern = "^download_ERA5_.*\\.nc$", full.names = TRUE)
reqList <- create_request_list(reqTable, areaS)
reqList <- reqList[!vapply(reqList, function(req) file.path(localPath, req$target) %in% existing_files, FUN.VALUE = logical(1))]
print(paste("Number of total requests to be submitted:", length(reqList)))
#remove partial downloads
unlink(list.files(localPath, pattern="ecmwf", full.names = TRUE))

# Submit requests to CDS API
send_requests(reqList, localPath, user="310914", batch_size = 20)


