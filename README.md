# ERA5_move2
Code to download and annotate U and V Wind for move2 object class

Before running the code, please check: https://cran.r-project.org/web/packages/ecmwfr/vignettes/ads_vignette.html

It is important to know that the code requires a registration with the Copernicus Climate Data Store (CDS) to download the data. The registration is free and can be done here: https://cds.climate.copernicus.eu/user/register
The data is requested through the ADS (atmospheric data store) API, which requires an API key. The API key can be obtained after registration and is used to authenticate the user. The API key should be stored using the keyring packages and the ecmwfr package using the wf_set_key() function. 

Likewise, the move2 package stores the credentials to movebank.org using the keyring package. The credentials are used to authenticate the user and allow the user to upload data to Movebank. See movebank_store_credentials("username") and the following vignette: https://bartk.gitlab.io/move2/articles/movebank.html

The code is divided into two parts:
1. Download the data based on a move2 object.
2. Annotate the data stored locally in a folder based on a move2 object stored in the same folder locally.

Part One: Download.
The downloads from CDS can and probably will fail. The CDS API is finicky, the internet is so too, and it will take some time. What the request ERA5 code does is to send a maximum of 20 concurrent requests (can be set as option 'batch_size' to the function 'send_requests') to CDS.  Each request can be either requesting data for a 'day', a 'week', or a 'month' and can either be for single layers or for various pressure levels. 

The download begins with starting a request table based on the spatial data for which the requests is supposed to be made, and based on the choice of how to split the spatial-temporal structure of the requests. For that, the function 'ERA5request' is used. It takes the options 'track', which must be a spatial object, 'timeUnit', which can be "day", "week", or "month" and 'area' which can be "byTimeUnit" or "complete". The option 'area_ext' sets the increase in area where a value of 0.01 would increase the bounding boxes by 1% in each direction. 

The next step is to create the CDS request table based on the previous table and choosing what data should be requested for those dates and areas. The 'create_request_list' function takes a list made by the function 'ERA5request' and allows to choose between the 'dataset' being 'ERA5 hourly data on single level' and 'ERA5 hourly data on pressure levels' (currently). The options are 'vars' and 'levels' which depend on the choice of 'dataset'. This nomenclature is taken from CDS to make it easier to see what variables and levels either of the two datasets provide for download. 

Depending on the track, there might be many requests to send. When a request was completed in CDS, a ncdf file is downloaded. The files will be downloaded with a specific file naming convention. The files are called ERA5_ followed by 'pl' or 'sl' for pressure level or single level and then the date conveys the time requets. For daily data the format of the date will be 8 digits for %Y%m%d, for weekly data the format will be 10 digits/characters %Y%mCW%W, and finally for monthly data the format would be 6 digits %Y%m. These files will be stored locally. 

The function that handles the sending and downloading completed requests to CDS is 'send_requests'. This function takes as options 'localPath', which is the folder path where all the data will be stored and also expected to be found when reinitiating requests. Furthermore 'user' refers to the user ID for CDS and finally 'batch_size' refers to how many requests can CDS handle for you at a time. Here, the recommendation is not to go over 40, better stay at somewhere like 20. Queueing more requests on CDS will not speed up your downloads, it will only make CDS lower your max number of requests you can send. 

If many requests have to be sent, chance are that the connection dies, or something else fails in CDS. Thus it is possible to rerun the code to resume the download of ERA 5 ncdf files. The code will check which raster files are already downloaded and skip the download of already obtained data if the corresponding file is already present in the folder indicated in localPath. The code will also check if a file is not fully downloaded and re-download the file if it is only partially downloaded. 

In the example script 'Request_ERA5.r' there is data downloaded from movebank. The idea is that prior to requesting data from CDS, the movement data can also be manipulated, filtered, subset etc. What is important is that there is a folder where all the requested daily wind data and a file called 'Track.rds' as well as 'requestTable.csv' are stored. The Track.rds file is a move2 object (we are working on sf and timestamp) that contains the spatio-temporal data that will eventually be annotated with the wind data. 

The downloaded ncdf files for a 'complete' bounding box can be used for visualisations of the movement data in the larger context given by the bounding box of the movement data. The ncdf files can be read using the raster package and visualised using the rasterVis package. Or used for animations. Note that the days and hours requested and obtained are set by the tracking data. For animations maybe the requests should also include days and hours that are not covered by the tracking data.

Part Two: Annotate.
The code starts with reading the Track.rds file and then reads the wind data from the folder. The wind data is then annotated to the coordinates contained in the move2 object using time and height above ellipsoid. The annotated data is then exported in a csv file in the same folder as the wind data. The output will contain event_id (the ID of the move2 object or the data frame), time, longitude, latitude, altitude, U and V wind. This code uses parallel processing to speed up the annotation process. The number of cores used can be set in the code (currently it is number of cores - 1). The annotation does a bilinear interpolation in horizontal space, then annotate linearly in time and finally also do a linear interpolation in height based on the geopotential height at the specified place and time. 

Future: 
1. *DONE* The code could be made more generic by allowing to submit non move2 objects most likely a data frame with EventID, X and Y coordinates, time and altitude, for which the area2request, date2request and extract_track_and_era5_data would require adjustment to allow distinguishing between receiving a move2 or a data frame object. 
2. What could be done in the future is to allow for more ERA5 products to be processed. The code is currently set up to download U, V and Z. The Z is used to interpolate the height above ellipsoid to height above sea level. The code could be extended to download other variables such as temperature, humidity, etc. and interpolate these as well. 
3. The code could also be extended to allow for more complex interpolation methods. 
4. The code could also be extended to allow for more complex filtering of the movement data. 