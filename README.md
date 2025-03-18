# ERA5_move2 new CDS API

## Code to Download and Annotate ERA5 Reanalysis Data for move2 Object Class

This package provides a framework for downloading environmental data from ERA5 reanalysis through the Copernicus Climate Data Store (CDS) API and annotating movement data with these variables. It supports both pressure level data (e.g., wind at different altitudes) and single level data (e.g., surface wind, temperature).

## Prerequisites

Before using this package, you need to:

1. Register with the Copernicus Climate Data Store (CDS): https://cds.climate.copernicus.eu/user/register
2. Set up your CDS API key following the guidelines at: https://cran.r-project.org/web/packages/ecmwfr/vignettes/cds_vignette.html
3. Store your API key using the ecmwfr package (see documentation)
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
- Temporal units: How to divide the temporal extent (daily, weekly, or monthly files)
- Area handling: Whether to use a single bounding box or separate ones for each time unit
- Area extension: How much to extend the bounding box beyond tracking locations

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

## Part Two: Annotating Tracking Data (Annotate_ERA5.r)

The annotation process interpolates ERA5 variables to your tracking locations in three dimensions:
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
- Multiple file formats (daily, weekly, or monthly)
- Wind components and other variables
- Vertical interpolation based on tracking altitude

## Key Features

1. **Flexible Data Handling**: Works with move2 objects or data frames with columns for EventID, coordinates, timestamp, and height
2. **Robust Download Management**: Handles API rate limits, resumes interrupted downloads, and tracks progress
3. **Efficient Processing**: Uses parallel processing to speed up annotation
4. **Multi-dimensional Interpolation**: Interpolates variables in space, time, and height
5. **Different Time Scales**: Supports daily, weekly, or monthly data organization

## Future Development

1. ✅ **Support for Generic Data Types**: The code now accepts both move2 objects and data.frames
2. 🔄 **Additional ERA5 Variables**: Extend support for more ERA5 products beyond wind components (temperature, humidity, etc.)
3. 🔄 **Advanced Interpolation Methods**: Implement more sophisticated interpolation algorithms
4. 🔄 **Enhanced Data Filtering**: Develop more options for pre-processing movement data

## Acknowledgments

This package leverages the ecmwfr package for CDS API access and the move2 package for animal movement data handling.
