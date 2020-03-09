# plot_cases_annual_space.R: Plots cases across space facetted by year.

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
library(scales)

# Load and modify the ggplot theme.
source("gg_theme.R")

# Define the years being tested.
years = seq(1898, 1949)

# Create empty data frames for storing all the data.
all_rectangles <- data.frame()
all_plague_shape_df <- data.frame()

# Create a text progress bar.
cat("Joining data...\n")
pb <- txtProgressBar(max=length(years), style=3)

# For each year.
for (year in years)
{
  # Define list relating data years to shapefile years.
  if (year < 1901){
    shapefileYear <- 1891
  } else if (year < 1903){
    shapefileYear <- 1901
  } else if (year < 1905){
    shapefileYear <- year
  } else if (year < 1912){
    shapefileYear <- 1911
  } else if (year < 1923){
    shapefileYear <- 1921
  } else if (year < 1931){
    shapefileYear <- 1923
  } else if (year < 1935){
    shapefileYear <- 1931
  } else if (year < 1938){
    shapefileYear <- min(1936, year)
  } else {
    shapefileYear <- 1941
  }
  
  # Load in the shapefile.
  plague_shape <- readOGR("Data/Shapefiles", paste("india_", shapefileYear, sep=""), verbose=FALSE)
  plague_shape <- rgeos::gBuffer(plague_shape, byid=TRUE, width=0)
  
  # Convert into a data frame.
  plague_shape_df <- broom::tidy(plague_shape, region="POLY_ID") %>%
    left_join(plague_shape@data, by = c("id" = "POLY_ID")) %>%
    mutate(STATE_NAME = as.character(STATE_NAME))
  
  # Load in the plague data.
  plague <- read.csv("Data/india_plague.csv")
  
  # Filter out date.
  plague_filtered <- filter(plague, YEAR == year) %>%
    mutate(NATIVE = as.factor(NATIVE)) %>%
    mutate(PROVINCE_STATE = as.character(PROVINCE_STATE)) %>%
    mutate(CASES = INCIDENCE) %>%
    select(-INCIDENCE, -POPULATION) %>%
    group_by(PROVINCE_STATE, NATIVE, YEAR) %>%
    summarise(CASES = sum(CASES)) %>%
    ungroup() %>%
    mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE == "Bengal" & YEAR <= 1911, "Bengal Province", PROVINCE_STATE))
  
  # Get the data which has no space to be plotted.
  missing_locations <- anti_join(plague_filtered, plague_shape_df, by = c("PROVINCE_STATE" = "STATE_NAME", "NATIVE" = "NATIVE")) %>%
    select(PROVINCE_STATE, NATIVE) %>%
    unique() %>%
    as_tibble()
  
  # Get the locations which do not have any data.
  missing_data <- anti_join(plague_shape_df, plague_filtered, by = c("STATE_NAME" = "PROVINCE_STATE", "NATIVE" = "NATIVE")) %>%
    select(STATE_NAME, NATIVE) %>%
    unique()
  
  # Join the data to the shape data.
  plague_shape_df <- left_join(plague_shape_df, plague_filtered, by = c("STATE_NAME" = "PROVINCE_STATE", "NATIVE" = "NATIVE")) %>%
    mutate(YEAR = year)

  # Create the rectangle map.
  rectangles <- data.frame("xmin" = c(89, 83, 71, 68), "ymin" = c(18, 10, 10, 18),
                           "x" = c(88.36, 80.27, 77.52, 72.87), "y" = c(23.85, 13.27, 13.37, 19.94),
                           "STATE_NAME" = c("Calcutta City",
                                            "Madras City",
                                            "Bangalore Civil And Military Station",
                                            "Bombay City")) %>%
    mutate(xmax = xmin + 2, ymax = ymin + 2) %>%
    mutate(STATE_NAME = as.character(STATE_NAME)) %>%
    left_join(plague_filtered, by = c("STATE_NAME" = "PROVINCE_STATE")) %>%
    mutate(YEAR = year)
  
  # Join all rectangles and shape data.
  all_plague_shape_df <- rbind(all_plague_shape_df, plague_shape_df)
  all_rectangles <- rbind(all_rectangles, rectangles)
  
  # Update the progress bar.
  setTxtProgressBar(pb, which(year == years))
}

# Close the progress bar.
close(pb)

# Unique labels for each year.
all_year_labels <- all_plague_shape_df %>%
  group_by(YEAR) %>%
  summarise(X = min(long) + 0.7*(max(long) - min(long)),
            Y = min(lat) + 0.8*(max(lat) - min(lat)))

# Plot the map.
gg_map <- ggplot(all_plague_shape_df) +
  geom_polypath(aes(x = long, y = lat, group = group, fill = CASES + 1), color = "black", size = .2) +
  geom_text(data=all_year_labels, aes(label=YEAR, x=X, y=Y), hjust=0.5, vjust=0.5) +
  geom_segment(data=all_rectangles, aes(x=x, y=y, xend = xmin+1, yend=ymin+1), colour="black", size=.2) +
  geom_rect(data=all_rectangles, aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, fill = CASES + 1), colour="black", size = .2) +
  scale_fill_distiller("Plague deaths per 1,000,000", type="seq",
                       palette="YlOrRd", trans=pseudo_log_trans(base=10), direction=1,
                       breaks=c(1, 10, 100, 1000, 10000)) +
  scale_x_continuous(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  facet_wrap(~YEAR, labeller = label_wrap_gen(multi_line=FALSE), ncol=5) +
  gg_theme +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title = element_blank(),
        legend.key.width = unit(15, "mm")) +
  theme(axis.line = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  coord_fixed() +
  guides(fill=guide_colorbar(title.hjust=0.5, title.position="top", frame.colour="black", ticks.colour="black"))
print(gg_map)

# Save the map.
ggsave(filename="india_plague_annual_space.pdf", plot=gg_map, dpi=600, height=11.69, width=8.27, path="Figures")
