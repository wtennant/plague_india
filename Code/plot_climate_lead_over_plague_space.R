# plot_climate_lead_over_plague_space.R: Plots the lead of climate over plague cases over space.

# Clear the workspace.
rm(list=ls())

# Load in the necessary libraries.
library(ggplot2)
library(ggforce)
library(tidyr)
library(dplyr)
library(rgdal)
#library(ggmap)
library(ggpolypath)

# Load in the default ggplot theme.
source("gg_theme.R")
gg_theme <- gg_theme +
  theme(axis.line = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank())

# Load in the phase lead data.
phase <- read.csv("Data/india_plague_climate_phase.csv", header=TRUE) %>%
  mutate(DATE = as.Date(DATE, format="%Y-%m-%d"))

# Duplicate the Burma data.
phase <- phase %>%
  mutate(ID = ifelse((PROVINCE_STATE == "Burma") & (CLIMATE == "Humidity"), 14, ID))
burma <- filter(phase, (PROVINCE_STATE == "Burma") & (CLIMATE == "Humidity")) %>%
  mutate(ID = 15)
phase <- bind_rows(phase, burma)

# Extract years where outbreaks actually occurred.
phase <- phase %>%
  group_by(PROVINCE_STATE, YEAR, CLIMATE) %>%
  mutate(INCIDENCE = mean(INCIDENCE)) %>%
  filter(INCIDENCE > 0)

# Reformat to join with the shapefile data.
phase_filtered <- select(phase, CLIMATE, ID, ANGLE) %>%
  group_by(ID, CLIMATE) %>%
  summarise(ANGLE = atan2(mean(sin(ANGLE)), mean(cos(ANGLE)))) %>%
  ungroup() %>%
  mutate(ANGLE = (ANGLE/2/pi*12) %% 12) %>%
  mutate(ID = as.character(ID)) %>%
  spread(CLIMATE, ANGLE)

# Load in the 1921 climate shape file.
climate_shape <- readOGR("Data/Shapefiles", "climate_1921")
climate_shape <- rgeos::gBuffer(climate_shape, byid=TRUE, width=0)

# Convert into the shapefile data into a data frame.
climate_shape_df <- broom::tidy(climate_shape, region="POLY_ID") %>%
  left_join(climate_shape@data, by = c("id" = "POLY_ID")) %>%
  mutate(STATE_NAME = as.character(STATE_NAME))

# Join the climate data to the shapefile data.
climate_shape_df <- left_join(climate_shape_df, phase_filtered, by = c("id"="ID")) %>%
  gather(key=CLIMATE, value=ANGLE, grep("Rainfall|Temperature|Humidity", names(.)))

# Re-order climate varaibles.
climate_shape_df$CLIMATE <- factor(climate_shape_df$CLIMATE, levels=c("Temperature", "Rainfall", "Humidity"))

# Desaturate colours.
colours <- rgb2hsv(col2rgb(rainbow(10, end=1)))
colours["s",] <- 0.25*colours["s",]
colours <- hsv(colours[1,], colours[2,], colours[3,])

# Construct the map.
gg_lag_space <- ggplot(climate_shape_df) +
  geom_polypath(aes(x = long, y = lat, group = group, fill = ANGLE), colour = "black", size = .2) +
  geom_text(data=data.frame("CLIMATE" = c("Temperature", "Rainfall", "Humidity")),
            x=0.68*(max(climate_shape_df$long) - min(climate_shape_df$long)) + min(climate_shape_df$long),
            y=0.78*(max(climate_shape_df$lat) - min(climate_shape_df$lat)) + min(climate_shape_df$lat),
            aes(label=CLIMATE), hjust=0.5, vjust=0.5, fontface="bold", size=6
            ) +
  scale_fill_gradientn("Plague outbreak lag time (months)", colours=colours, limits=c(0,12), na.value="grey75") +
  scale_x_continuous(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  facet_wrap(~CLIMATE) +
  gg_theme +
  theme(legend.key.width=unit(20, "mm"),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  xlab("") +
  ylab("") +
  coord_fixed() +
  guides(fill = guide_colorbar(frame.colour="black", ticks.colour="black",
                               title.hjust=0.5, title.position="top"))

# Print the map.
print(gg_lag_space)

# Save the figure
filename <- "fig_plague_climate_lag_space.pdf"
ggsave(filename, plot=gg_lag_space, height=8.27, width=11.69, path="Figures", scale=1.5)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename ,"\""))
