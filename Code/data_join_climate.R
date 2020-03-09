# data_join_climate.R: Joins the climate and plague incidence data files.

# Clear the workspace.
rm(list = ls())

# Clear the workspace.
rm(list=ls())

# Load the libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Load in the climate data.
climate <- read.csv("Data/india_climate.csv")

# Load in the plague data.
plague <- read.csv("Data/india_plague.csv")

# Aggragate the data by merging native and non-native states.
plague <- filter(plague, YEAR>="1907" & YEAR<="1936") %>%
  group_by(PROVINCE_STATE, MONTH, YEAR) %>%
  summarise(INCIDENCE = mean(INCIDENCE)) %>%
  mutate(ID = as.numeric(PROVINCE_STATE)) %>%
  mutate(ID = ifelse(PROVINCE_STATE == "Ajmer Merwara", which("Rajputana Agency" == levels(PROVINCE_STATE)), ID)) %>%
  mutate(ID = ifelse(PROVINCE_STATE == "Baroda", which("Bombay Presidency" == levels(PROVINCE_STATE)), ID)) %>%
  mutate(ID = ifelse((PROVINCE_STATE == "Burma") & (YEAR <= 1922), which("Upper Burma" == levels(PROVINCE_STATE)), ID)) %>%
  mutate(ID = ifelse(PROVINCE_STATE == "Coorg", which("Mysore" == levels(PROVINCE_STATE)), ID)) %>%
  mutate(ID = ifelse(PROVINCE_STATE == "Delhi", which("Punjab" == levels(PROVINCE_STATE)), ID)) %>%
  mutate(ID = ifelse(PROVINCE_STATE == "Cambay", which("Bombay Presidency" == levels(PROVINCE_STATE)), ID)) %>%
  mutate(ID = ifelse(PROVINCE_STATE == "Deccan State Agency", which("Bombay Presidency" == levels(PROVINCE_STATE)), ID)) %>%
  group_by(ID, MONTH, YEAR) %>%
  mutate(INCIDENCE = mean(INCIDENCE)) %>%
  filter(PROVINCE_STATE != "Madras City",
         PROVINCE_STATE != "Bombay City",
         PROVINCE_STATE != "Calcutta City",
         PROVINCE_STATE != "Bangalore Civil And Military Station",
         PROVINCE_STATE != "Ajmer Merwara",
         PROVINCE_STATE != "Baroda",
         (PROVINCE_STATE != "Burma") | (YEAR > 1922),
         PROVINCE_STATE != "Delhi",
         PROVINCE_STATE != "Coorg",
         PROVINCE_STATE != "Cambay",
         PROVINCE_STATE != "Deccan State Agency",
         PROVINCE_STATE != "Eastern States Agency") %>%
  ungroup() %>%
  select(-ID)

# Create Burma out of Upper and Lower Burma where appropriate (to join humidity data).
burma <- filter(plague, PROVINCE_STATE %in% c("Lower Burma", "Upper Burma")) %>%
  group_by(MONTH, YEAR) %>%
  summarise(INCIDENCE = mean(INCIDENCE),
            PROVINCE_STATE = "Burma")

# Bind the aggregated Burmese plague data to all plague data.
plague <- suppressWarnings(bind_rows(plague, burma))

# Rename some states to have a more cohesive data set.
plague$PROVINCE_STATE[which(plague$PROVINCE_STATE == "United Province")] <- "United Province Of Agra And Oudh"

# Add Burma to the climate data as well.
#burma <- filter(climate, METEROLOGICAL_PROVINCE %in% c("Lower Burma", "Upper Burma")) %>%
#  group_by(MONTH, YEAR, CLIMATE, UNIT) %>%
#  summarise(VALUE = mean(VALUE, na.rm=TRUE),
#            METEROLOGICAL_PROVINCE = "Burma",
#            ID=19)

# Bind the aggregated Burmese climate data to all climate data.
#climate <- suppressWarnings(bind_rows(climate, burma))

# Join the plague data with the climate data.
plague <- suppressWarnings(left_join(plague, climate, by=c("MONTH" = "MONTH", "PROVINCE_STATE" = "METEROLOGICAL_PROVINCE", "YEAR" = "YEAR")))

# Write data to file.
write.csv(plague, file="Data/india_plague_climate.csv", row.names=FALSE)
