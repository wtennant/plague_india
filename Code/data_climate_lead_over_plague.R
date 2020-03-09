# data_climate_lead_over_plague.R: Calculate the phase lead between epidemic and climate time series
# in different regions of India.

# Clear the workspace.
rm(list = ls())

# Load libraries.
library(tidyr)
library(dplyr)
library(WaveletComp)

# Load the joint plague and climate data.
plague <- read.csv("Data/india_plague_climate.csv")

# Construct a data frame for the time series of each variable.
plague_timeSeries <- plague %>%
  group_by(CLIMATE, ID) %>%
  mutate(VALUE = (VALUE - min(VALUE, na.rm=TRUE)) / (max(VALUE, na.rm=TRUE) - min(VALUE, na.rm=TRUE))*max(INCIDENCE, 1, na.rm=TRUE)) %>%
  select(-UNIT) %>%
  spread(CLIMATE, VALUE) %>%
  gather(TYPE, VALUE, Temperature, Rainfall, Humidity, INCIDENCE) %>%
  mutate(DATE = as.Date(paste("01", MONTH, YEAR, sep="-"), format="%d-%B-%Y"))

# Get a list of unique states.
states <- select(plague, PROVINCE_STATE, CLIMATE) %>%
  unique()
phase <- data.frame()
for (i in seq(1, nrow(states)))
{
  # Define the state and climate variable.
  state <- states$PROVINCE_STATE[i]
  clim_var <- states$CLIMATE[i]
  
  one_plague <- filter(plague_timeSeries, PROVINCE_STATE == state, TYPE == "INCIDENCE") %>%
    mutate(TIME = order(order(DATE)) - 1) %>%
    group_by(YEAR) %>%
    arrange(TIME) %>%
    ungroup()
  one_climate <- filter(plague_timeSeries, PROVINCE_STATE == state, TYPE == clim_var) %>%
    mutate(TIME = order(order(DATE)) - 1) %>%
    group_by(YEAR) %>%
    arrange(TIME) %>%
    ungroup()
  
  # Discard missing values.
  one_plague <- one_plague[!is.na(one_climate$VALUE),]
  one_climate <- one_climate[!is.na(one_climate$VALUE),]
  
  if (sum(one_plague$VALUE) == 0) {
    
    # Print out some key information.
    cat(paste("\nProvince:", state, "\tClimate:", clim_var))
    cat(paste("\nSkipping because there is no incidence\n"))
    
  } else {
    # Construct the time series.
    timeSeries <- data.frame("t1" = log10(one_climate$VALUE + 1),
                             "t2" = log10(one_plague$VALUE + 1),
                             "t3" = cos(2*pi*seq(0, nrow(one_plague) - 1) / 12) /2 + 0.5)
    
    # Analyse the coherency between the climate and oscillations peaking in January.
    wc_climate <- analyze.coherency(timeSeries, my.pair=c(1,3), make.pval = FALSE, verbose = FALSE, dt=1, dj=1/100,
                            upperPeriod = max(12, floor(nrow(timeSeries)/3)))
    
    # Analyse the coherency between the plague and oscillations peaking in January.
    wc_plague <- analyze.coherency(timeSeries, my.pair=c(2,3), make.pval = FALSE, verbose = FALSE, dt=1, dj=1/100,
                                    upperPeriod = max(12, floor(nrow(timeSeries)/3)))
    
    # Analyse the coherency between the climate and plague.
    wc <- analyze.coherency(timeSeries, my.pair=c(1,2), make.pval = FALSE, verbose = FALSE, dt=1, dj=1/100,
                            upperPeriod = max(12, floor(nrow(timeSeries)/3)))
    
    # Print out some key information.
    cat(paste("\nProvince:", state, "\tClimate:", clim_var))
    cat(paste("\nMean climate lead time:", mean(wc$Angle[which.min(abs(wc$Period - 12)),])/pi/2*12, "months\n"))
    
    # Add the phases of the annual signal to the data set.
    one_plague <- one_plague %>%
      mutate(ANGLE = wc$Angle[which.min(abs(wc$Period - 12)),],
             PHASE_CLIMATE = wc_climate$Angle[which.min(abs(wc_climate$Period - 12)),],
             PHASE_PLAGUE = wc_plague$Angle[which.min(abs(wc_plague$Period - 12)),],
             CLIMATE = clim_var,
             INCIDENCE = VALUE) %>%
    select(-TYPE, -VALUE)
    
    # Bind to the data.
    phase <- bind_rows(phase, one_plague)
  }
}

# Save the output to file.
write.csv(phase, file="Data/india_plague_climate_phase.csv", row.names=FALSE)
