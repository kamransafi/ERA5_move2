# ERA5_move2 new CDS API

## Code to Download and Annotate ERA5 Reanalysis Data for move2 Object Class

This package provides a framework for downloading environmental data from ERA5 reanalysis through the Copernicus Climate Data Store (CDS) API and annotating movement data with these variables. It supports both pressure level data (e.g., wind at different altitudes) and single level data (e.g., surface wind, temperature).

## Prerequisites

Before using this package, you need to:

1. Register with the Copernicus Climate Data Store (CDS): https://cds.climate.copernicus.eu
2. Set up your CDS API key in CDS under "Your Profile".
3. Store your API key using the ecmwfr package version > 2.0.0 (see documentation)
4. Note that you need to agree to the user agreement and licences. Go to the CDS website and log in to accept the terms under "Your Profile", where you find the "Licences" tab

For working with Movebank data, the move2 package uses keyring to store credentials. See https://bartk.gitlab.io/move2/articles/movebank.html for details on storing credentials.

## Workflow Overview

The package workflow is divided into two main parts:
1. **Download**: Request and download ERA5 data based on your tracking data's spatial and temporal extent (Request_ERA5.r)
2. **Annotate**: Process the downloaded ERA5 data to extract environmental variables at each tracking location (Annotate_ERA5.r)

## Part One: Downloading ERA5 Data (Request_ERA5.r)

The download process uses a robust framework that handles API rate limits, automatically resumes interrupted downloads, and organizes requests efficiently.

### Step 1: Create a Request Table

The ERA5request function examines your tracking data and creates a structured request table based on:
- Temporal units: How to divide the temporal extent (daily, weekly, or monthly files). Note that the data requested are hourly data, however the files contain data for a specific spatio-temporal extent (the extent of your data and in batches of one day, one week, or one month)
- Area handling: Whether to use a single bounding box or separate ones for each time unit
- Area extension: How much to extend the bounding box beyond tracking locations
- Variables: Which ERA5 variables to download
#### Pressure level variables
- `geopotential` - Geopotential height (default in the function)
- `u_component_of_wind` - Eastward/zonal wind component (default in the function)
- `v_component_of_wind` - Northward/meridional wind component (default in the function)
- `temperature` - Air temperature
- `specific_humidity` - Specific humidity
- `relative_humidity` - Relative humidity
- `divergence` - Wind divergence
- `vorticity` - Vorticity (rotation of the wind field)
- `potential_vorticity` - Potential vorticity
- `vertical_velocity` - Vertical velocity (omega)
- `ozone_mass_mixing_ratio` - Ozone concentration
- `fraction_of_cloud_cover` - Cloud cover fraction
- `specific_cloud_liquid_water_content` - Liquid water content in clouds
- `specific_cloud_ice_water_content` - Ice content in clouds

#### Default Pressure Levels
The default pressure levels in the code are:
```
c("500", "550", "600", "650", "700", "750", "775", "800", "825", "850", "875", "900", "925", "950", "975", "1000")
```
However they can be requested from 1000hPa to 1hPa.

#### Single Level Dataset Variables
- `10m_u_component_of_wind` - Eastward wind component at 10m height (default in the function)
- `10m_v_component_of_wind` - Northward wind component at 10m height (default in the function)
- `2m_temperature` - Air temperature at 2m height (default in the function)
- `2m_dewpoint_temperature` - Dewpoint temperature at 2m height
- `mean_sea_level_pressure` - Atmospheric pressure at sea level
- `surface_pressure` - Pressure at the surface
- `total_precipitation` - Accumulated liquid and frozen water falling to Earth's surface
- `convective_precipitation` - Accumulated convective precipitation
- `snow_depth` - Depth of snow
- `snowfall` - Accumulated snow (water equivalent)
- `boundary_layer_height` - Height of the planetary boundary layer
- `total_cloud_cover` - Fraction of sky covered by clouds
- `low_cloud_cover` - Fraction of sky covered by low clouds
- `medium_cloud_cover` - Fraction of sky covered by medium clouds
- `high_cloud_cover` - Fraction of sky covered by high clouds
- `surface_solar_radiation_downwards` - Solar radiation reaching the surface
- `surface_thermal_radiation_downwards` - Thermal radiation reaching the surface
- `surface_sensible_heat_flux` - Transfer of heat through air movement
- `surface_latent_heat_flux` - Heat transfer through evaporation
- `soil_temperature_level_1` - Soil temperature near surface (0-7cm)
- `volumetric_soil_water_layer_1` - Soil moisture near surface (0-7cm)


### Step 2: Create Formatted Request List

The create_request_list function converts the request table into properly formatted API requests for:
- Dataset type: "ERA5 hourly data on pressure levels" or "ERA5 hourly data on single level"
- Variables: Wind components, geopotential height, temperature, etc.
- Pressure levels: For pressure level data, specify which levels to download

### Step 3: Send Requests and Download Data

The send_requests function handles:
- Submitting requests to the CDS API
- Monitoring request status
- Downloading completed files
- Handling rate limits with exponential backoff
- Automatic retries for failed downloads
- Tracking progress and resuming interrupted downloads

Files are downloaded with a naming convention:
- ERA5_pl_YYYYMMDD.nc for daily pressure level data
- ERA5_sl_YYYYMMDD.nc for daily single level data
- ERA5_pl_YYYYMMCWWW.nc for weekly pressure level data (WW = week number)
- ERA5_pl_YYYYMM.nc for monthly pressure level data

#### Interrupting and resuming requests

The download part is the most time consuming. You can interrupt the request_ERA5.r script and resume it later, just rerun the script. The script will check which files are already downloaded and continue with the missing files. It will resume the process, checking for completed jobs and download them, and accordingly submit new jobs, until all the jobs have been completed. You can interrupt as needed, resume when back etc., just source the Request_ERA5.r script on the same machine again. 

## Part Two: Annotating Tracking Data (Annotate_ERA5.r)

The annotation process interpolates ERA5 variables to your tracking locations in three dimensions and in time:
1. Spatial interpolation (horizontal)
2. Temporal interpolation
3. Vertical interpolation (for pressure level data)

### Running the Annotation Process

The Annotate_ERA5.r script:
- Loads tracking data from a move2 object or data frame
- Sets up parallel processing to speed up annotation
- Loads ERA5 files and extracts relevant data for each tracking location
- Performs multi-dimensional interpolation to match environmental data to exact tracking coordinates, timestamps, and heights
- Combines results and saves them to file

This process adds environmental variables to each tracking location, handling:
- Tracking data from move2 objects or data frames
- Multiple file formats (daily, weekly, or monthly batches)
- Wind components and other variables
- Vertical interpolation based on the **altitude of the location data** as well as the **pressure level data** 
  for the appropriate heights and based on the **geopotential height**. 

## Key Features

1. **Flexible Data Handling**: Works with move2 objects or data frames with columns for EventID, coordinates, timestamp, and height
2. **Robust Download Management**: Handles API rate limits, resumes interrupted downloads, and tracks progress
3. **Efficient Processing**: Uses parallel processing to speed up annotation
4. **Multi-dimensional Interpolation**: Interpolates variables in space, time, and height
5. **Different Time Scales**: Supports daily, weekly, or monthly data organization

## Future Development

1. ✅ **Support for Generic Data Types**: The code now accepts both move2 objects and data.frames
2. 🔄 **Additional ERA5 Variables**: Check support for all ERA5 products beyond wind components (temperature, humidity, etc.)
3. 🔄 **Advanced Interpolation Methods**: Implement more sophisticated interpolation algorithms
4. Try Terra::vrt for tiling instead of terra::merge.

## Acknowledgments

This package leverages the ecmwfr package for CDS API access and the move2 package for animal movement data handling.
