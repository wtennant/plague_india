# plot_climate_offset.R: Plot the provinicial climate affects on
# outbreak timing on a map.

# Clear the workspace.
rm(list=ls())

# Load data manipulation and visualisation libraries.
library(ggplot2)
library(tidyr)
library(dplyr)
library(rgdal)
library(ggpolypath)

# Load the ggplot theme.
source("gg_theme.R")

# Load in plague-climate phase data.
plague <- read.csv("Data/india_plague_climate_phase.csv")

# Define an outbreak incidence threshold.
# And define a threshold for how many outbreaks.
outbreak = 100
outbreaks = 2

# Join select provinces.
plague <- plague %>%
  # Combine Bihar and Orissa with Bengal and Bihar, and Orissa.
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Bihar", "Orissa"), "Bihar And Orissa", as.character(PROVINCE_STATE))) %>%
  # Combine Eastern Bengal and Assam with Assam.
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Eastern Bengal And Assam"), "Assam", as.character(PROVINCE_STATE))) %>%
  group_by(PROVINCE_STATE) %>%
  mutate(ID = min(ID)) %>%
  group_by(PROVINCE_STATE, CLIMATE, YEAR, MONTH) %>%
  summarise(PHASE_CLIMATE = atan2(mean(sin(PHASE_CLIMATE)), mean(cos(PHASE_CLIMATE))),
            PHASE_PLAGUE = atan2(mean(sin(PHASE_PLAGUE)), mean(cos(PHASE_PLAGUE))),
            INCIDENCE = mean(INCIDENCE),
            ID = mean(ID)) %>%
  ungroup()

# Calculate the number of outbreaks per province and then filter out data
# which does not have enough outbreaks.
plague <- plague %>%
  group_by(PROVINCE_STATE, CLIMATE, YEAR) %>%
  mutate(OUTBREAK = sum(INCIDENCE) >= outbreak) %>%
  filter(OUTBREAK) %>%
  group_by(PROVINCE_STATE, CLIMATE) %>%
  mutate(OUTBREAK = sum(OUTBREAK)) %>%
  filter(OUTBREAK >= outbreaks)

# For each location, calculate the overall mean phase for climate and plague.
# Then calculate the annual change from the mean phase.
plague <- plague %>%
  select(PROVINCE_STATE, YEAR, CLIMATE, PHASE_CLIMATE, PHASE_PLAGUE, INCIDENCE, ID) %>%
  group_by(PROVINCE_STATE, CLIMATE) %>%
  mutate(PHASE_CLIMATE_AVG = atan2(mean(sin(PHASE_CLIMATE)), mean(cos(PHASE_CLIMATE)))) %>%
  mutate(PHASE_PLAGUE_AVG = atan2(mean(sin(PHASE_PLAGUE)), mean(cos(PHASE_PLAGUE)))) %>%
  group_by(PROVINCE_STATE, CLIMATE, YEAR) %>%
  mutate(PHASE_CLIMATE = atan2(mean(sin(PHASE_CLIMATE)), mean(cos(PHASE_CLIMATE))),
         PHASE_PLAGUE = atan2(mean(sin(PHASE_PLAGUE)), mean(cos(PHASE_PLAGUE)))) %>%
  summarise(PHASE_CLIMATE = mean(PHASE_CLIMATE - PHASE_CLIMATE_AVG),
            PHASE_PLAGUE = mean(PHASE_PLAGUE - PHASE_PLAGUE_AVG),
            INCIDENCE = sum(INCIDENCE),
            ID = mean(ID)) %>%
  mutate(PHASE_CLIMATE = atan2(sin(PHASE_CLIMATE), cos(PHASE_CLIMATE)),
         PHASE_PLAGUE = atan2(sin(PHASE_PLAGUE), cos(PHASE_PLAGUE))) %>%
  mutate(PHASE_CLIMATE = PHASE_CLIMATE/2/pi*12,
         PHASE_PLAGUE = PHASE_PLAGUE/2/pi*12)

# Duplicate Burmese humidity data for plotting.
burma <- filter(plague, CLIMATE == "Humidity", PROVINCE_STATE == "Burma") %>%
  mutate(ID = 15)
plague <- plague %>%
  mutate(ID = ifelse(CLIMATE == "Humidity" & PROVINCE_STATE == "Burma", 14, ID)) %>%
  bind_rows(burma)

# Some linear model with location as a mixed effect.
plague_model <- plague %>%
  group_by(ID, CLIMATE) %>%
  summarise(GRAD = lm(PHASE_PLAGUE ~ PHASE_CLIMATE)$coefficients[2],
            PROB = summary(lm(PHASE_PLAGUE ~ PHASE_CLIMATE))$coefficients[2,4])
plague_model$ID <- as.character(plague_model$ID)

# Spread out the climate data ready for joining.
plague_model <- plague_model %>%
  select(-PROB) %>%
  spread(key="CLIMATE", value="GRAD")

# Load in the 1921 climate shape file.
india_shape <- readOGR("Data/Shapefiles", "climate_1921")
india_shape <- rgeos::gBuffer(india_shape, byid=TRUE, width=0)

# Convert into the shapefile data into a data frame.
india_shape_df <- broom::tidy(india_shape, region="POLY_ID") %>%
  left_join(india_shape@data, by = c("id" = "POLY_ID")) %>%
  mutate(STATE_NAME = as.character(STATE_NAME))

# Join the india plague data to the shapefile data and re-gather climate variables.
india_shape_df <- left_join(india_shape_df, plague_model, by = c("id"="ID")) %>%
  gather(key="CLIMATE", value="GRAD", c("Temperature", "Rainfall", "Humidity"))

# Re-order climate varaibles.
india_shape_df$CLIMATE <- factor(india_shape_df$CLIMATE, levels=c("Temperature", "Rainfall", "Humidity"))

# Contain phase shifts between -6 and 6.
india_shape_df <- india_shape_df %>%
  mutate(GRAD = (GRAD + 6) %% 12 - 6)

# Plot by climate type and location.
gg_offset <- ggplot(india_shape_df) +
  geom_polypath(aes(x = long, y = lat, group = group, fill = GRAD), colour = "black", size = .2) +
  geom_text(data=data.frame("CLIMATE" = c("Temperature", "Rainfall", "Humidity")),
            x=0.68*(max(india_shape_df$long) - min(india_shape_df$long)) + min(india_shape_df$long),
            y=0.78*(max(india_shape_df$lat) - min(india_shape_df$lat)) + min(india_shape_df$lat),
            aes(label=CLIMATE), hjust=0.5, vjust=0.5, fontface="bold", size=5) +
  scale_x_continuous(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  scale_fill_distiller("Delay in outbreak timing per one month\ndelay in seasonal change", palette="RdBu",
                       limits=c(-max(abs(india_shape_df$GRAD), na.rm=TRUE),
                                max(abs(india_shape_df$GRAD), na.rm=TRUE)),
                       na.value = ) +
  facet_wrap(~CLIMATE) +
  gg_theme +
  theme(legend.key.width=unit(20, "mm"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()) +
  xlab("") +
  ylab("") +
  coord_fixed() +
  guides(fill = guide_colorbar(frame.colour="black", ticks.colour="black",
                               title.hjust=0.5, title.position="top"))

# Print the figure.
print(gg_offset)

# Save the figures.
filename <- "fig_india_climate_offset_space.pdf"
ggsave(filename, plot = gg_offset,
       path="Figures", dpi=600, height=8.67, width=11.69)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename, "\""))
