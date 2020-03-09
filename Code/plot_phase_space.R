# plot_phase_space.R: Plots the phase of plague cases across space.

# Clear the workspace.
rm(list=ls())

# Load the necessary libraries.
library(ggplot2)
library(tidyr)
library(dplyr)
library(rgdal)
library(maptools)
library(ggmap)
library(ggpolypath)

# Load and modify the ggplot theme.
source("gg_theme.R")
gg_theme <- gg_theme +
  theme(axis.line = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(legend.position="right")

# Load in the plague data.
phase <- read.csv("Data/india_plague_phase.csv") %>%
  mutate(PROVINCE_STATE = ifelse((PROVINCE_STATE == "Bengal") & (YEAR <= 1911), "Bihar And Orissa", as.character(PROVINCE_STATE))) %>%
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Bihar", "Orissa"), "Bihar And Orissa", as.character(PROVINCE_STATE))) %>%
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Central Province", "Berar"), "Central Province And Berar", as.character(PROVINCE_STATE))) %>%
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE == "United Province", "United Province Of Agra And Oudh", as.character(PROVINCE_STATE)))

# Desaturate colours.
colours <- rgb2hsv(col2rgb(rainbow(11, end=1)))
#colours <- cbind(colours[,6:11], colours[,2:6])
colours["s",] <- 0.25*colours["s",]
colours <- hsv(colours[1,], colours[2,], colours[3,])

# Take the average phase over the years.
phase_filtered <- phase %>%
  group_by(PROVINCE_STATE, NATIVE) %>%
  summarise(PHASE = atan2(mean(sin(PHASE)), mean(cos(PHASE)))) %>%
  mutate(PHASE = (-PHASE/2/pi*12 - 6) %% 12) %>%
  mutate(NATIVE = as.factor(NATIVE))

# Use the shape file for 1921 to plot the phase.
shapefileYear <- 1921
  
# Load in the shapefile.
plague_shape <- readOGR("Data/Shapefiles", paste("india_", shapefileYear, sep=""))
plague_shape <- rgeos::gBuffer(plague_shape, byid=TRUE, width=0)
  
# Convert into the shapefile data into a data frame.
plague_shape_df <- broom::tidy(plague_shape, region="POLY_ID") %>%
  left_join(plague_shape@data, by = c("id" = "POLY_ID")) %>%
  mutate(STATE_NAME = as.character(STATE_NAME))
  
# Join the plague data to the shapefile data.
plague_shape_df <- left_join(plague_shape_df, phase_filtered, by = c("STATE_NAME" = "PROVINCE_STATE",
                                                                     "NATIVE" = "NATIVE"))
  
# Create rectangles for the four separate (non-provincial) locations.
rectangles <- data.frame("xmin" = c(89, 83, 71, 68), "ymin" = c(18, 10, 10, 18),
                         "x" = c(88.36, 80.27, 77.52, 72.87), "y" = c(23.85, 13.27, 13.37, 19.94),
                         "STATE_NAME" = c("Calcutta City",
                                          "Madras City",
                                          "Bangalore Civil And Military Station",
                                          "Bombay City")) %>%
  mutate(xmax = xmin + 2, ymax = ymin + 2) %>%
  mutate(STATE_NAME = as.character(STATE_NAME)) %>%
  left_join(phase_filtered, by = c("STATE_NAME" = "PROVINCE_STATE"))
  
# Plot and save the map.
gg_map <- ggplot(plague_shape_df) +
  geom_polypath(aes(x = long, y = lat, group = group, fill = PHASE), color = "black", size = .2) +
  geom_segment(data=rectangles, aes(x=x, y=y, xend = xmin+1, yend=ymin+1), colour="black", size=.2) +
  geom_rect(data=rectangles, aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, fill = PHASE), colour="black", size = .2) +
  scale_fill_gradientn("Timing of\noutbreaks", colors=colours, limits = c(0, 12), na.value = "grey60",
                       breaks=c(0, 2, 4, 6, 8, 10, 12),
                       labels=c("July", "September", "November", "January", "March", "May", "July")) +
  scale_x_continuous(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  gg_theme +
  theme(legend.key.height=unit(15,"mm"),
        legend.title=element_text(margin=margin(0,0,5,0,"mm"))) +
  xlab("") +
  ylab("") +
  coord_fixed() +
  guides(fill = guide_colorbar(ticks.colour="black", frame.colour="black"))

# Save the map.
ggsave(filename=paste("india_plague_phase.pdf", sep=""), plot=gg_map,
       dpi=150, height=9, width=16, path="Figures")
system("bash -c \"pdfcrop --margin '14.22638' Figures/india_plague_phase.pdf Figures/india_plague_phase.pdf\"")
