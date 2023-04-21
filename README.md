# Map_CA_fields
Source code for "A new framework for mapping conservation agricultural fields using time-series optical and radar imagery"， you could find the detailed description from (add DOI later).

Please cite as: 

The entire workflow consists of these steps:

1. Obtain satellite data from Google Earth Engine platform. script path: (https://code.earthengine.google.com/?scriptPath=users%2Fyuez9466%2FCApractice%3ANDVI). You need to download the NDVI, NBR2, Sentinel-1 Radar dataset and Precipitation data for your research area and seltected time interval. Download data from Google Cloud, then convert the format of the data for following calculations.(see 1_import_transfer_data.R)

2. Obtain the annual crop types in your study area, either through agricultural census data or remote sensing predictions (not mentioned in this paper), calculate organic carbon input based on the crop types. 

3. Extracting seasons and phenology based on time-series NDVI values using phenofit package. (see 2_NDVI_Smooth_Divide_seasons.R)

4. Calculating the length of the cover crop growing season and periods of bare soil. (see 3_CC_BS_length.R)

5. Create covarites and build a tillage model. (see 4.Tillage.R)

6. Build your own conservation agriculture fields model!
