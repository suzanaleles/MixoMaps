######################################################################
# Plotting global maps : OBIS data by functional type and size class #
######################################################################

rm(list=ls())

library(ggplot2)   # plot
library(viridis)   # color
library(tidyr)     # data structure
library(rgdal)     # to read shapefiles
library(plyr)      # join function
library(gridExtra) # save plot

size <- read.csv('data_final_obis_size.csv', header = T, fileEncoding="latin1")
size$class <- size$size
size$class[size$size <= 10] <- "<10"
size$class[size$size > 10 & size$size <= 20] <- "10-20"
size$class[size$size > 20 & size$size <= 200] <- "20-200"
size$class[size$size > 200 & size$size <= 300] <- "200-300"
size$class[size$size > 300] <- ">300"
size$class <- factor(size$class, level = c("<10", "10-20", "20-200", "200-300",">300"))
size$type <- factor(size$type, level = c("CM", "GNCM", "pSNCM", "eSNCM"))

# restructure data by type and size class
dataFINAL <- gather(size, province, n, -c(scientificName, type, size, class))

# For plotting, replace zero values with NA
dataFINAL$n[dataFINAL$n == 0] <- NA
dataFINAL$province <- factor(dataFINAL$province)
dataFINAL <- aggregate(n ~ type + class + province, data = dataFINAL, FUN = sum)

# Importing shapefile for the Longhurst provinces
datalong <- readOGR("Longhurst_world_v4_2010.shp")
# Getting province id names
datalong@data$id <- rownames(datalong@data)
# Bringing together data from shapefile and data of number of records 
colnames(dataFINAL) <- c("type", "class", "ProvCode", "n")
data_obis <- as.data.frame(dataFINAL)
datalong@data <- join(datalong@data, data_obis, by = "ProvCode")
# Make shapefile a dataframe
datalong_df <- fortify(datalong)
# Bringing all together in a single dataframe to plot with ggplot
datalong_df <- join(datalong_df, datalong@data, by = 'id')
# Data to plot world map
world <- map_data("world")

# Plotting
ptype <- ggplot() +
  geom_polygon(data = datalong_df, aes(x = long, y = lat, group = group, fill = log(n+1)),
               colour = 'gray') +
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = region),
           colour = "darkgray", fill = "darkgray") +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white", size = 0.5, colour = 'darkgray'),
        legend.position = "right",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.key.width = unit(.5, "cm"),
        legend.key.height = unit(.95, "cm"),
        strip.background = element_rect(colour="darkgray", fill="white"),
        strip.text = element_text(size = 19),
        plot.title = element_text(hjust = 0, face = "bold", size = 22)
  ) +
  coord_cartesian(ylim = c(-79,90), xlim = c(-180, 180)) +
  scale_fill_viridis(na.value = "white") +
  facet_grid(class ~ type, scales="free")

png('map_type_size.png', height = 3520, width = 2800*2, res = 300)
grid.arrange(ptype)
dev.off()