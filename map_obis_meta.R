##################################################################
# Global maps coordinates OBIS versus MetaPR2 by functional type #
##################################################################

rm(list=ls())

library(ggplot2)
library(gridExtra)
library(viridis)

# Importing data from MetaPR2
d <- read.table("data_obis_meta_coor.txt", header = T)

# Selected esncm species
esncm <- d[d$species == "Acanthometra pellucida" | 
           d$species == "Globigerinita glutinata" |
           d$species == "Orbulina universa" |
           d$species == "Collozoum inerme", ]

esncm$species <- factor(esncm$species, levels = c("Acanthometra pellucida", "Globigerinita glutinata",
                                                  "Orbulina universa", "Collozoum inerme"))
# Selected cm species
cm <- d[d$species == "Emiliania huxleyi" |
        d$species == "Phaeocystis globosa" |
        d$species == "Chrysochromulina leadbeateri" |
        d$species == "Tripos furca", ]

cm$species <- factor(cm$species, levels = c("Emiliania huxleyi", "Phaeocystis globosa",
                                            "Chrysochromulina leadbeateri", "Tripos furca"),
                     labels = c("Emiliania huxleyi", "Phaeocystis globosa",
                                "Chrysochromulina leadb.", "Tripos furca"))
# Selected psncm and gncm species
psncm <- d[d$species == "Mesodinium rubrum" |
           d$species == "Dinophysis acuminata" | 
           d$species == "Dinophysis acuta" |
           d$species == "Laboea strobila", ]

psncm$species <- factor(psncm$species, levels = c("Mesodinium rubrum", "Dinophysis acuminata", 
                                                  "Dinophysis acuta", "Laboea strobila"))

# Plotting
col <- viridis(5)

# Data to plot world map
world <- map_data("world")

pesncm <- ggplot() +
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
  geom_point(data = esncm, aes(x = long, y = lat, col = database), alpha = 0.5, size = 2) +
  scale_color_manual(values = c(col[1], col[3])) +
  facet_grid(species ~ database) +
  theme(legend.position = "none",
        strip.text.y = element_text(size = 14, face = "italic"))

pcm <- ggplot() +
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
  geom_point(data = cm, aes(x = long, y = lat, col = database), alpha = 0.5, size = 2) +
  scale_color_manual(values = c(col[1], col[3])) +
  facet_grid(species ~ database) +
  theme(legend.position = "none",
        strip.text.y = element_text(size = 14, face = "italic"))

ppsncm <- ggplot() +
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
  geom_point(data = psncm, aes(x = long, y = lat, col = database), alpha = 0.5, size = 2) +
  scale_color_manual(values = c(col[1], col[3])) +
  facet_grid(species ~ database) +
  theme(legend.position = "none",
        strip.text.y = element_text(size = 14, face = "italic"))

png('map_obis_metapr2_esncm.png', height = 2800, width = 2700, res = 300)
grid.arrange(pesncm, ncol = 1)
dev.off()

png('map_obis_metapr2_cm.png', height = 2800, width = 2700, res = 300)
grid.arrange(pcm, ncol = 1)
dev.off()

png('map_obis_metapr2_psncm.png', height = 2800, width = 2700, res = 300)
grid.arrange(ppsncm, ncol = 1)
dev.off()

png('map_obis_metapr2.png', height = 3500, width = 2700*3, res = 300)
grid.arrange(pcm, ppsncm, pesncm, ncol = 3)
dev.off()

# Map for Noctiluca scintillans 
ns <- d[d$species == "Noctiluca scintillans", ]

pns <- ggplot() +
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
  geom_point(data = ns, aes(x = long, y = lat, col = database), alpha = 0.5, size = 2) +
  scale_color_manual(values = c(col[1], col[3])) +
  facet_grid(species ~ database) +
  theme(legend.position = "none",
        strip.text.y = element_text(size = 14, face = "italic"))

png('map_obis_metapr2_pns.png', height = 800, width = 2700, res = 300)
grid.arrange(pns, ncol = 1)
dev.off()