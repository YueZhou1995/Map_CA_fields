# Map_CA_fields
Source code for "A new framework for mapping conservation agricultural fields using time-series optical and radar imagery"， you could find the detailed description from (add DOI later).

Please cite as: 

The entire workflow consists of these steps:

1. Obtain satellite data from Google Earth Engine platform. script path: https://code.earthengine.google.com/8fea476933a1238e3c20afad6c4251d5. You need to download the NDVI, NBR2, Sentinel-1 Radar dataset and Precipitation data for your research area and seltected time interval. Download data from Google Cloud, then convert the format of the data for following calculations.(see import_transfer_data.R)

2. Obtain the annual crop types in your study area, either through agricultural census data or remote sensing predictions (not mentioned in this paper), calculate organic carbon input based on the crop types. (see code OCinput_crop.R)

3.
