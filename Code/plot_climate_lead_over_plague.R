# plot_climate_lead_over_plague.R: Plots the lead time of rainfall and temperature over
# outbreaks of plague in different regions of india.

# Clear the workspace.
rm(list=ls())

# Load in the necessary libraries.
library(ggplot2)
library(ggforce)
library(tidyr)
library(dplyr)

# Load in the default ggplot theme.
source("gg_theme.R")

# Define an outbreak incidence threshold.
# And define a threshold for how many outbreaks.
outbreak = 100
outbreaks = 2

# Load in the phase lead data.
phase <- read.csv("Data/india_plague_climate_phase.csv", header=TRUE) %>%
  mutate(DATE = as.Date(DATE, format="%Y-%m-%d"))

# Calculate the number of outbreaks per province and then filter out data
# which does not have enough outbreaks.
phase <- phase %>%
  group_by(PROVINCE_STATE, CLIMATE, YEAR) %>%
  mutate(OUTBREAK = sum(INCIDENCE) >= outbreak) %>%
  filter(OUTBREAK) %>%
  group_by(PROVINCE_STATE, CLIMATE) %>%
  mutate(OUTBREAK = sum(OUTBREAK)) %>%
  filter(OUTBREAK >= outbreaks)

# Filter out years where outbreaks occurred.
phase_filtered <- phase %>%
  group_by(PROVINCE_STATE, YEAR, CLIMATE) %>%
  summarise(ANGLE = atan2(mean(sin(ANGLE)), mean(cos(ANGLE))),
            INCIDENCE = sum(INCIDENCE)) %>%
  ungroup()

# Calculate the most endemic provinces.
endemic <- group_by(phase, PROVINCE_STATE) %>%
  summarise(INCIDENCE = mean(INCIDENCE)) %>%
  arrange(desc(INCIDENCE)) %>%
  ungroup()
most_endemic <- endemic$PROVINCE_STATE[1:10]

# Filter out endemic data.
phase_filtered <- phase_filtered %>%
  filter(PROVINCE_STATE %in% most_endemic)

# Set labels for provinces.
labels <- c("North West Frontier Province" = "1. North-West Frontier Province",
            "Punjab" = "4. Punjab",
            "United Province Of Agra And Oudh" = "6. United Province of Agra and Oudh",
            "Sindh" = "7. Sindh",
            "Rajputana Agency" = "8. Rajputana Agency",
            "Central India Agency" = "10. Central India Agency",
            "Bihar And Orissa" = "11. Bihar and Orissa",
            "Bengal" = "12. Bengal",
            "Assam" = "13. Assam",
            "Upper Burma" = "14. Upper Burma",
            "Bombay Presidency" = "15. Bombay Presidency",
            "Central Province And Berar" = "16. Central Province and Berar",
            "Hyderabad" = "17. Hyderabad",
            "Lower Burma" = "18. Lower Burma",
            "Mysore" = "20. Mysore",
            "Madras Presidency" = "21. Madras Presidency")
phase_filtered <- phase_filtered %>%
  mutate(ID=as.character(labels[as.character(PROVINCE_STATE)])) %>%
  mutate(ID=as.numeric(substr(gsub("[[:punct:]][A-z ]*", "", ID), start=1, stop=2))) %>%
  mutate(PROVINCE_STATE = as.character(labels[as.character(PROVINCE_STATE)]))
phase_filtered$PROVINCE_STATE <- factor(phase_filtered$PROVINCE_STATE,
                                           levels=unique(phase_filtered$PROVINCE_STATE)[order(unique(phase_filtered$ID))])

# Re order the climate factors and grouping.
phase_filtered$CLIMATE <- factor(phase_filtered$CLIMATE,
                                 levels=c("Temperature", "Rainfall", "Humidity"))
phase_filtered <- phase_filtered %>%
  mutate(GROUP = paste(CLIMATE, PROVINCE_STATE))
phase_filtered$GROUP <- factor(phase_filtered$GROUP,
                               levels=rev(unique(phase_filtered$GROUP)))

# Construct the time series of climate and incidence.
gg_lag <- ggplot(phase_filtered, aes(x = PROVINCE_STATE, y = (ANGLE/2/pi*12) %% 12)) +
  geom_violin(aes(fill=CLIMATE), scale="width", width=0.75, position=position_dodge(0.75)) +
  geom_boxplot(aes(group=GROUP), width=0.2, position=position_dodge(0.75)) +
  scale_y_continuous(limits=c(0, 12), breaks=c(0,3,6,9,12)) +
  scale_fill_brewer("Climate", type="qual", palette="Set1") +
  gg_theme + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("Province") +
  ylab("Plague outbreak lag time (months)") +
  guides(fill = guide_legend(title.hjust=0.5, title.position="top"))
print(gg_lag)

# Save the figure
filename <- "fig_plague_climate_lag_time.pdf"
ggsave(filename, plot=gg_lag, height=9.5, width=11.69, path="Figures", scale=1)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename ,"\""))
