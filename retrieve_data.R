#########################
# OBIS data compilation #
#########################

# To run this script, it is necessary to provide a file with a list of taxa and 
# to have in the directory the shapefiles with the Longhurst provinces
# The shapefiles used here were obtained at http://www.marineregions.org

rm(list=ls())

# Loading libraries

#install.packages("devtools")
#install_github("iobis/robis")
library(devtools)  # to import data from OBIS
library(robis)     # to import data from OBIS
library(rgdal)     # to read shapefiles
library(plyr)      # to join 
library(dplyr)     # to perform loops
library(tidyr)     # to manipulate data
library(writexl)   # to export data to an excel file

#---------------------------------------#
# Importing lat and long data from OBIS #
#---------------------------------------#

# Species name control: species names have been matched with the World Register of Marine Species
# For future analyses using this script, this can be updated directly in R using the obistools package

# Importing species list
data_sp <- read.csv('data_sample.csv', header = T)
# Inspecting variable types
str(data_sp)
# Retrieving occurrence records from OBIS
species <- occurrence(data_sp$species)
# Transforming species name column to factor
species$scientificName <- as.factor(species$scientificName)
# Clean up to retrieve data only for the species within our input file
species <- species[species$scientificName %in% data_sp$species,]

# Saving lat and long data by species
mixo <- data.frame(species = species$scientificName, 
                   long = species$decimalLongitude, 
                   lat = species$decimalLatitude)

# Adding functional grouping
mixo <- merge(mixo, data_sp[, c("species", "mft", "size")], by = "species")

# Export data to an excel file (add your own directory here)
write_xlsx(mixo,"C:\\Users\\leles\\Desktop\\mixo_coor.xlsx")

################################
# Count by Longhurst Province  #
################################

# Creating a function to extract data for each Longhurst biogeographic province
# Data quality control: this function also removes any data points on land 
count_obs <- function(species){
  # retrieving longitude and latitute:
  long <- species$decimalLongitude
  lat <- species$decimalLatitude
  # building dataframe:
  data <- data.frame(long, lat)
  # transforming to SpatialPointsDataFrame
  p <- SpatialPointsDataFrame(data, data.frame(id=1:nrow(data)))
  # setting the CRS of polygon-data to point-data
  proj4string(p)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  # read shapefile for Longhurst provinces 
  polygons <- readOGR("Longhurst_world_v4_2010.shp")
  # counting number of observations within each province
  res <- over(p, polygons)
  res$ProvCode <- as.factor(res$ProvCode)
  n <- as.numeric(with(res, tapply(res$ProvCode, list(res$ProvCode), length)))
  # get province names
  provinces <- levels(res$ProvCode)
  # merge counts and province names in a dataframe
  obs <- data.frame(provinces, n)
  # make sure to include provinces for which no data were found
  nodata_prov <- setdiff(polygons$ProvCode, res$ProvCode)
  add_nodata <- data.frame(provinces = nodata_prov, n = rep(0,length(nodata_prov)))
  obs <- rbind(obs, add_nodata)
  # replace NA values with zeros
  obs[is.na(obs)] <- 0
  # end function
  return(obs) 
}

# Looping: apply the count_obs function for each species in the dataset 
loop <- species %>%
  group_by(scientificName) %>%
  do(count_obs(.))

loop$provinces <- as.factor(loop$provinces)

# Check species for which no records were found (loop will not include these)
sp_zeros <- data.frame(scientificName = as.factor(setdiff(data_sp$species, loop$scientificName)))

# if interested on these species, we need to add them back 
zero_df <- function(sp_zeros){
  n_pr <- length(levels(loop$provinces))
  scientificName <- rep(sp_zeros$scientificName, n_pr)
  provinces <- levels(loop$provinces)
  n <- rep(0, n_pr)
  sp_zeros_df <- data.frame(scientificName, provinces, n)
}

if(nrow(sp_zeros) > 0){
  sp_zeros_df <- sp_zeros %>%
    group_by(scientificName) %>%
    do(zero_df(.))
  update_loop <- rbind(loop, sp_zeros_df)
} else {
  update_loop <- loop
}

# Order species according to input dataset
update_loop$scientificName <- factor(update_loop$scientificName, levels = unique(data_sp$species))

# Manipulating data to get a matrix format
final_data <- spread(data = update_loop, key = provinces, value = n, fill = NA, convert = FALSE, drop = TRUE)
names(final_data)[names(final_data) == 'scientificName'] <- 'species'

# Adding functional grouping
final_data <- merge(final_data, data_sp[, c("species", "mft", "size")], by = "species")

# Export data to an excel file
write_xlsx(final_data,"C:\\Users\\leles\\Desktop\\mix_prov.xlsx")
