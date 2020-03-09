# plot_wavelet.R: Plots the wavelet power spectrum of several provinces.

# Clear the workspace.
#rm(list = ls())

# Library.
library(ggplot2)
library(tidyr)
library(dplyr)
library(WaveletComp)

# Load the default ggplot theme.
source("gg_theme.R")

# Load the data.
plague <- read.csv("Data/india_plague.csv") %>%
  mutate(PROVINCE_STATE = ifelse((PROVINCE_STATE == "Bengal") & (YEAR <= 1911), "Bihar And Orissa", as.character(PROVINCE_STATE))) %>%
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Bihar", "Orissa"), "Bihar And Orissa", as.character(PROVINCE_STATE))) %>%
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Central Province", "Berar"), "Central Province And Berar", as.character(PROVINCE_STATE))) %>%
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE == "United Province", "United Province Of Agra And Oudh", as.character(PROVINCE_STATE)))

# Get the most endemic states.
endemic <- plague %>%
  group_by(PROVINCE_STATE) %>%
  summarise(CASES = sum(CASES)) %>%
  arrange(desc(CASES)) %>%
  .[1:6,]

# Create an empty data frame for saving the wavelet data.
all_wave_df <- data.frame()

# For each state
for (state in endemic$PROVINCE_STATE)
{
  # Filter province/state, create dates and sort by time.
  plague_filtered <- filter(plague, PROVINCE_STATE == state) %>%
    group_by(MONTH, YEAR, PROVINCE_STATE) %>%
    summarise(INCIDENCE = mean(INCIDENCE, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(DATE = as.Date(paste("01", MONTH, YEAR), format = "%d %B %Y")) %>%
    mutate(TIME = order(order(DATE)) - 1) %>%
    group_by(YEAR) %>%
    arrange(TIME) %>%
    mutate(INCIDENCE = log(INCIDENCE + 1, base=1000))
  
  # Do wavelet analysis on the plague time series.
  wave <- analyze.wavelet(plague_filtered, my.series="INCIDENCE", make.pval=FALSE, verbose=FALSE,
                          upperPeriod=5*12)
  
  # Construct the data frame containing the power spectrum information.
  wave_df <- expand.grid("TIME"=plague_filtered$DATE, "PERIOD"=wave$Period) %>%
    mutate(POWER=as.numeric(t(wave$Power))) %>%
    group_by(PERIOD) %>%
    mutate(WIDTH=as.numeric(diff(c(TIME, max(TIME) + 31)))) %>%
    ungroup() %>%
    mutate(PROVINCE_STATE = state)
  
  # Add to the overall power spectrum data.
  all_wave_df <- rbind(all_wave_df, wave_df)
}

# Set look up table for facet labels.
labels <- c(
  "Bihar And Orissa" = "11. Bihar and Orissa",
  "Bombay Presidency" = "15. Bombay Presidency",
  "Central Province And Berar" = "16. Central Province and Berar",
  "Hyderabad" = "17. Hyderabad",
  "Punjab" = "8. Punjab",
  "United Province Of Agra And Oudh" = "6. United Province of Agra and Oudh"
)

# Reorder of provinces.
all_wave_df <- all_wave_df %>%
  mutate(ID=as.character(labels[as.character(PROVINCE_STATE)])) %>%
  mutate(ID=as.numeric(gsub("[[:punct:]][A-z ]*", "", ID)))
all_wave_df$PROVINCE_STATE <- factor(all_wave_df$PROVINCE_STATE,
                                     levels=unique(all_wave_df$PROVINCE_STATE)[order(unique(all_wave_df$ID))])
  
# Plot the wavelet power spectrum.
gg_wavelet <- all_wave_df %>%
  #filter(PROVINCE_STATE == state) %>%
  ggplot(aes(x=TIME, y=PERIOD/12, fill=POWER, width=WIDTH)) +
  geom_tile() +
  scale_x_date("Year", expand=c(0,0)) +
  scale_y_continuous("Period (years)", trans="log2", expand=c(0,0), breaks=c(0.25, 1, 4),
                     labels=c(0.25, 1, 4)) +
  scale_fill_distiller("Wavelet power", palette="YlOrRd", direction = 1, breaks=c(0, 1, 2),
                       limits=c(0, max(all_wave_df$POWER))) +
  gg_theme +
  theme(aspect.ratio=1,
        legend.position="right",
        panel.background=element_rect(fill="grey60"),
        panel.border=element_rect(fill=NA, colour="black")) +
  facet_wrap(~PROVINCE_STATE, labeller=as_labeller(labels), ncol=2) +
  guides(fill=guide_colorbar(frame.colour="black", ticks.colour="black"))

# Draw and save the figure
print(gg_wavelet)
filename <- "fig_plague_wavelet_endemic.pdf"
ggsave(filename, plot=gg_wavelet, height=11.69, width=8.27, path="Figures", scale=1.05)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename ,"\""))
