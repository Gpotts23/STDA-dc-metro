# STDA-dc-metro

IMPORTANT:
	1. The R files in the 'Stadia_Key' folder are the raw files used for this analysis. Reproduction of the code requires the user to register a Stadia Maps API Key, which is free. Instructions to register a Stadia Maps API key can be found at: https://docs.stadiamaps.com/authentication/#api-keys.

	2. If the user wants to reproduce the code without registering a Stadia Maps API key, the files in the 'No_key' folder can be ran without a Stadia Maps API key. These files will produce the same result, but plots will not display a basemap. 

	3. For either option, the DataPrep script should be ran prior to the K-means or DBSCAN scripts. After the DataPrep script is ran, the K-means and DBSCAN scripts can be ran interchangeably.


Abstract:
As is the case with any public transportation network, ridership experiences variation at different stations throughout the year. This analysis focuses on the Washington DC Metropolitan Area metro system in the United States, using two spatial-temporal clustering methods to compare ridership differences throughout 2024. The functionality and performance of these two clustering methods with the metro data is also compared. The two spatial-temporal clustering methods used to perform this data analysis and performance comparison are K-means and Density Based Spatial Clustering of Applications with Noise (DBSCAN).

Useful clustering of metro stations can be utilised to predict future ridership trends, adjust fare pricing across stations, target regions for expansion, and guide infrastructure investments in the metro network. 