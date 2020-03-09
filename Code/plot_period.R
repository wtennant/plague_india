# plot_period.R: Plots the periodicity of all provinces.

# Clear the workspace.
rm(list = ls())

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

# Get the selected states.
states <- c("North West Frontier Province",
            "Jammu And Kashmir",
            "Baluchistan Agency",
            "Punjab",
            "Delhi",
            "United Province Of Agra And Oudh",
            "Sindh",
            "Rajputana Agency",
            "Ajmer Merwara",
            "Central India Agency",
            "Bihar And Orissa",
            "Bengal",
            "Assam",
            "Upper Burma",
            "Bombay Presidency",
            "Central Province And Berar",
            "Hyderabad",
            "Lower Burma",
            "Coorg",
            "Mysore",
            "Madras Presidency",
            "Bombay City",
            "Bangalore Civil And Military Station",
            "Madras City",
            "Calcutta City")

# Create an empty data frame for saving the wavelet data.
all_wave_df <- data.frame()

# For each state
for (state in states)
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
    mutate(INCIDENCE = log(INCIDENCE + 1, base=10)) %>%
    filter(!is.na(INCIDENCE))
  
  # Only continue if there are cases.
  if (sum(plague_filtered$INCIDENCE, na.rm=TRUE) > 0)
  {
    # Do wavelet analysis on the plague time series.
    wave <- analyze.wavelet(plague_filtered, my.series="INCIDENCE", make.pval=FALSE, verbose=FALSE,
                            dj=1/100, upperPeriod=5*12)
    
    # Construct the data frame containing the power spectrum information.
    wave_df <- data.frame("PERIOD"=wave$Period, POWER=wave$Power.avg) %>%
      mutate(PROVINCE_STATE = state)
    
    # Add to the overall power spectrum data.
    all_wave_df <- rbind(all_wave_df, wave_df)
  }
}

# Make the weights fair between provinces (i.e. make the area under the curve equal).
all_wave_df %>%
  group_by(PROVINCE_STATE) %>%
  mutate()

# Create the labels for the legend.
state_labels <- c("1.   North-West Frontier Province",
            "2.   Jammu and Kashmir",
            "3.   Baluchistan Agency",
            "4.   Punjab",
            "5.   Delhi",
            "6.   United Province of Agra and Oudh",
            "7.   Sindh",
            "8.   Rajputana Agency",
            "9.   Ajmer Merwara",
            "10. Central India Agency",
            "11. Bihar and Orissa",
            "12. Bengal",
            "13. Assam",
            "14. Upper Burma",
            "15. Bombay Presidency",
            "16. Central Province and Berar",
            "17. Hyderabad",
            "18. Lower Burma",
            "19. Coorg",
            "20. Mysore",
            "21. Madras Presidency",
            "22. Bombay City (Mumbai)",
            "23. Bangalore Civil and Military Station",
            "24. Madras City (Chennai)",
            "25. Calcutta City (Kolkata)")

# Plot the evidence for each periodicity.
gg_period <- all_wave_df %>%
  ggplot(aes(x=PERIOD/12, y=POWER, colour=PROVINCE_STATE)) +
  geom_line(position="stack") +
  scale_y_continuous("Cumulative power", expand=c(0,0)) +
  scale_x_continuous("Period (years)", trans="log2", expand=c(0,0), breaks=c(0.25, 0.5, 1, 2, 4),
                     labels=c(0.25, 0.5, 1, 2, 4)) +
  scale_colour_discrete("Province", labels=state_labels) +
  coord_flip() +
  gg_theme +
  theme(aspect.ratio=1,
        legend.position="right",
        legend.margin=margin(5, 10, 5, 10, "mm"),
        legend.text=element_text(size=10),
        legend.key.height=unit(5.5, "mm")) +
  guides(colour=guide_legend(override.aes=list(size=1), ncol=1))

# Draw and save the figure
print(gg_period)
ggsave("plague_periodicity.pdf", plot=gg_period, dpi=600, path="Figures",
       height=8.27, width=8.27, scale=1.25)
system("bash -c \"pdfcrop --margin '14.22638' Figures/plague_periodicity.pdf Figures/plague_periodicity.pdf\"")
