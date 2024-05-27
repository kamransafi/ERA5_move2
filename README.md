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
The downloads from CDS can fail. The CDS API is finicky, the internet is so too, and it will take some time. What the request ERA5 code does is to send a maximum of 20 concurrent requests to CDS where each request will be for a day of U, V and Z (geopotential height) for all the hours that there is movement data for in that particular day (+1 hour to the max hour of the movement data time stamps) over the bounding box (extended by 1%) of the entire movement track. 
So depending on the track, there might be many requests to send. When a request was completed in CDS, a ncdf file is downloaded (name of the file will be download_ERA5_ and added the date of the data). Then the request on cds is deleted and a new request queued in CDS (but never more than 20), until all requests have been worked off. If many requests have to be sent, chance are that the connection dies, or something else fails in CDS. Thus it is possible to rerun the code to resume the download of ERA 5 ncdf files (you require only the last command, but you can also source the whole Request_ERA5.r code). The code will check which raster files are already downloaded and skip the download of already obtained data if the corresponding file is already present in the folder indicated. The code will also check if a file is not fully downloaded and re-download the file if it is only partially downloaded. 

In the example there is data downloaded from movebank. The idea is that prior to requesting data from CDS, the movement data can also be manipulated, filtered, subset etc. What is important is that there is a folder where all the requested daily wind data and a file called Track.rds are stored. The Track.rds file is a move2 object that contains the movement data that will eventually be annotated with the wind data. If you want to work with move2 objects of your own choice, you can replace the Track.rds file with your own move2 object and start the code with setting the area2request after you created a folder to contain your data and the Track.rds file.
The downloaded ncdf files can be used for visualisations of the movement data in the larger context given by the bounding box of the movement data. The ncdf files can be read using the raster package and visualised using the rasterVis package. Or used for animations. Note that the days and hours requested and obtained are set by the tracking data. For animations maybe the requests should also include days and hours that are not covered by the tracking data.

Part Two: Annotate.
The code starts with reading the Track.rds file and then reads the wind data from the folder. The wind data is then annotated to the coordinates contained in the move2 object using time and height above ellipsoid. The annotated data is then exported in a csv file in the same folder as the wind data. The output will contain eventID (the ID of the move2 object), time, latitude, longitude, altitude, U and V wind. This code uses parallel processing to speed up the annotation process. The number of cores used can be set in the code (currently it is number of cores - 1). The annotation does a bilinear interpolation in horizontal space, then annotate linearly in time and finally also do a linear interpolation in height based on the geopotential height at the specified place and time. 

Future: 
1. The code could be made more generic by allowing to submit non move2 objects most likely a data frame with EventID, X and Y coordinates, time and altitude, for which the area2request, date2request and extract_track_and_era5_data would require adjustment to allow distinguishing between receiveing a move2 or a data frame object. 
2. What could be done in the future is to allow for more ERA5 products to be processed. The code is currently set up to download U, V and Z. The Z is used to interpolate the height above ellipsoid to height above sea level. The code could be extended to download other variables such as temperature, humidity, etc. and interpolate these as well. 
3. The code could also be extended to allow for more complex interpolation methods. 
4. The code could also be extended to allow for more complex filtering of the movement data. 