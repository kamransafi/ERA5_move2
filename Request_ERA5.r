# Main execution
source(".../ERA5_functions.r")
#Your userID for the Copernicus Climate Data Store
#This assumes that you have stored your credentials with the wf_set_key() function (see readme).
CDS_USER_ID <- "..."

# ID of the study in movebank or a study name needed for movebank_download_study but also for naming the data folder
studyID <- 24442409 
# ID of the local tag in movebank needed for the move2 function
localID <- "3029" 
#Local folder where all the ERA5 data and the track as Track.rds will be stored
Local_path <- paste(".../Data/Study", studyID, sep="")

#if the directory hasn't been created yet, create it
if (!dir.exists()) dir.create(Local_path)

### This part can/should be adjusted to your needs in regard to the movement data that you want to annotate ###
# What is important that at the end there is a Track.rds file stored in the Local_path and 
# that the very same movement data is contained in the object "Track"!
# here I use the movebank_download_study function to download the movement data from movebank with no further adjustments

# check if movement data has been stored in the working directory and skip downloading from movebank if it has
if (!file.exists(paste(Local_path, Track.rds, sep="/"))) {
  # Load data from a study from movebank
  Track <- movebank_download_study(
    study_id = studyID,
    tag_local_identifier = localID,
    sensor_type_id = c("gps"),
    attributes = "all"
    )
  #store the track, which will be needed for the annotations later
  saveRDS(Track, file = file.path(Local_path, "Track.rds"))
} else {
  Track <- readRDS(file = file.path(Local_path, "Track.rds"))
}
### End of the part that can/should be adjusted ###


# set the request area based on the movement data
areaS <- area2request(Track)

# create the requests for the track
reqTable <- date2request(Track)
print(paste("Number of days to be requested:", nrow(reqTable)))

# check for existing files and remove corresponding existing requests from the request list
existing_files <- list.files(localPath, pattern = "^download_ERA5_.*\\.nc$", full.names = TRUE)
reqList <- create_request_list(reqTable, areaS)
reqList <- reqList[!vapply(reqList, function(req) file.path(localPath, req$target) %in% existing_files, FUN.VALUE = logical(1))]
print(paste("Number of total requests to be submitted:", length(reqList)))
# remove partial downloads
unlink(list.files(localPath, pattern="ecmwf", full.names = TRUE))

# Submit requests to CDS API based on the list of requests
send_requests(reqList, localPath, user=CDS_USER_ID, batch_size = 20)


