# MixoMaps
## Mapping mixoplankton in the global ocean using OBIS and metaPR2 datasets.

The code presented here can be applied to map the distribution of any species within the OBIS database as well as to any dataset that contains georeferenced data (i.e. latitude and longitude coordinates). We also developed a function that links georeferenced data to the classification of the ocean by Longhurst into 54 biogeographic provinces (Longhurst 2010). 

Data manipulation and analyses were performed in R:
1) retrieve_data.R contains code to  retrieve data from OBIS (input file requires species name) and to define a function that can transform georeferenced data into counts by Longhurst provinces (input file requires species name and georeferenced data). Here the input files were extended to also contain information on mixoplankton functional type and size for posterior analyses. 
2) heatmap_type_size.R generates global heatmaps based on Longhurst provinces for mixoplankton based on functional type and size class.
3) nmds.R performs a NMDS analysis to compare the global distribution of mixoplankton species between the OBIS and the metaPR2 databases.
4) map_obis_meta.R generates global maps with georeferenced data for example species to compare their distribution between the OBIS and the metaPR2 databases.


### Bibliography
Longhurst, Alan R. Ecological geography of the sea. Elsevier, 2010.


