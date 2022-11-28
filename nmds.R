#############################
# NMDS: OBIS versus MetaPR2 #
#############################

library(ggplot2)
library(dplyr)
library(vegan)        
library(dplyr)
library(viridis)
library(ggpubr)
library(writexl)

rm(list=ls())

# Import data from OBIS
size1 <- read.csv('data_final_obis_size.csv', header = T, fileEncoding="latin1")
size1$database <- rep("OBIS", nrow(size1))
size1 <- size1[rowSums(size1[4:57])>0,]


# Import data from metaPR2
size2 <- read.csv('data_final_meta_size.csv', header = T, fileEncoding="latin1")
size2$database <- rep("metaPR2", nrow(size2))
size2 <- size2[rowSums(size2[4:57])>0,]

# Make sure we just include species that are present in OBIS and MetaPR2
size1 <- subset(size1, scientificName %in% size2$scientificName)
size2 <- subset(size2, scientificName %in% size1$scientificName)
size <- rbind(size1, size2)

# Add size classes
size$class <- size$size
size$class[size$size <= 10] <- "<10"
size$class[size$size > 10 & size$size <= 20] <- "10-20"
size$class[size$size > 20 & size$size <= 200] <- "20-200"
size$class[size$size > 200 & size$size <= 300] <- "200-300"
size$class[size$size > 300] <- ">300"
size$class <- factor(size$class, level = c("<10", "10-20", "20-200", "200-300",">300"))
size$type <- factor(size$type, level = c("CM", "GNCM", "pSNCM", "eSNCM"))

# Transform the data into presence/absence
size_pa <- size[,4:57] %>% mutate_if(is.numeric, ~1 * (. != 0))
size_pa <- cbind(size[,1:3], size_pa)
size_pa2 <- cbind(size_pa, size[,58:59])
size <- size_pa2

# Remove rows that only contain 0 values to perform NMDS
dnmds <- size[,4:57] %>% 
  filter_all(any_vars(. != 0))

# Run NMDS
nMDS <- metaMDS(dnmds, distance = "jaccard", k = 4, trymax=1000) # stress = 0.07
names(nMDS)
species <- nMDS$points
datamds <-data.frame(speciesy = species[,2], speciesx = species[,1])

# Merge back with functional type and database information
ha <- merge(size[,c(1:3,58:59)], datamds, by.x = 0, by.y = 0)

# Plotting settings
mytheme <-   theme(panel.grid.minor = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.background = element_rect(colour = "black", fill = "white", size=1),
                    axis.text=element_text(size=16, colour ="black"),
                    axis.title=element_text(size=17, colour ="black"),
                    axis.ticks.length = unit(.25, "cm"),
                    plot.margin = unit(c(1, 1.5, 0.5, 0.5), "lines"),
                    legend.title = element_blank(),
                    legend.text = element_text(size=15, colour ="black"),
                    legend.key = element_rect(fill = "white"),
                    legend.position = c(0.15,0.1),
                    legend.background = element_rect(linetype = 'solid', color = 'black'),
                    plot.title = element_text(hjust = -0.65, face = "bold", size = 25)
)

cols <- viridis(5)

# Plot NMDS results

p1 <- ggplot(data = ha, aes(x = speciesx, y = speciesy, col = database)) +
  geom_point(cex = 3) +
  xlab('NMDS 1') + ylab('NMDS 2') +
  scale_color_manual(values = c(cols[1],cols[4])) +
  annotate("text", x = -1.5, y = 1.8, label = "stress = 0.07", cex = 4.5) +
  mytheme +
  theme(legend.position = c(0.8,0.87))
  
p2 <- ggplot(data = ha, aes(x = speciesx, y = speciesy, col = database, shape = type)) +
  geom_point(cex = 3) +
  xlab('NMDS 1') + ylab('NMDS 2') +
  scale_color_manual(values = c(cols[1],cols[4])) +
  annotate("text", x = -1.5, y = 1.8, label = "stress = 0.07", cex = 4.5) +
  mytheme +
  theme(legend.position = "right")

# remove data from species for which we do not have information on cell size
ha2 <- na.omit(ha)

p3 <- ggplot(data = ha2, aes(x = speciesx, y = speciesy, col = database, shape = class)) +
  geom_point(cex = 3) +
  xlab('NMDS 1') + ylab('NMDS 2') +
  scale_color_manual(values = c(cols[1],cols[4])) +
  annotate("text", x = -1.5, y = 1.8, label = "stress = 0.07", cex = 4.5) +
  mytheme +
  theme(legend.position = "right")

# Save figures
png('nmds1.png', res = 300, height = 1500, width = 1600)
ggarrange(p1, font.label = list(size = 22))
#grid.arrange(p1, ncol=1)
dev.off()

png('nmds2.png', res = 300, height = 1500, width = 2000*2)
ggarrange(p2, p3, ncol=2, labels = c("a","b"), font.label = list(size = 22))
dev.off()
