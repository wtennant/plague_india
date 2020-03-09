# plot_climate_plague_time.R: Plots the time series of climate variables and incidence
# by location.

# Clear the workspace.
rm(list = ls())

# Load the necessary libraries.
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

# Load the ggplot theme.
source("gg_theme.R")

# Load in the joint plague-climate data.
plague <- read.csv("Data/india_plague_climate.csv")

# Combine Bihar and Orissa with Bengal and Bihar, and Orissa.
plague <- plague %>%
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Bengal", "Bihar", "Orissa"), "Bihar And Orissa", as.character(PROVINCE_STATE))) %>%
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Lower Burma", "Upper Burma"), "Burma", as.character(PROVINCE_STATE))) %>%
  group_by(PROVINCE_STATE, MONTH, YEAR, CLIMATE) %>%
  summarise(VALUE = mean(VALUE), INCIDENCE = mean(INCIDENCE)) %>%
  ungroup()

# Extract the temperature and rainfall data for Lower and Upper Burma, average
# and change to ID to Burma.
#burma <- plague %>%
#  filter(CLIMATE != "Humidity", PROVINCE_STATE %in% c("Lower Burma", "Upper Burma")) %>%
#  group_by(MONTH, YEAR, CLIMATE) %>%
#  summarise(VALUE = mean(VALUE), )

# Construct a data frame for the time series of each variable.
plague_timeSeries <- plague %>%
  mutate(INCIDENCE = round(INCIDENCE, digits=10)) %>% # Solves floating point error problems when spreading.
  group_by(CLIMATE, PROVINCE_STATE) %>%
  mutate(VALUE = (VALUE - min(VALUE, na.rm=TRUE)) / (max(VALUE, na.rm=TRUE) - min(VALUE, na.rm=TRUE))*max(INCIDENCE, 1, na.rm=TRUE)) %>%
  spread(CLIMATE, VALUE) %>%
  gather(TYPE, VALUE, Temperature, Rainfall, Humidity, INCIDENCE) %>%
  mutate(DATE = as.Date(paste("01", MONTH, YEAR, sep="-"), format="%d-%B-%Y")) %>%
  ungroup()

# Set labels of facets for provinces of interest.
labels <- c(
  "Burma" = "19. Burma",
  "Madras Presidency" = "22. Madras Presidency",
  "Bombay Presidency" = "15. Bombay Presidency",
  "Mysore" = "20. Mysore",
  "Central Province And Berar" = "16. Central Province and Berar",
  "Punjab" = "8. Punjab",
  "Bihar And Orissa" = "11. Bihar and Orissa"
)

# Select provinces of interest.
plague_timeSeries <- plague_timeSeries %>%
  filter(PROVINCE_STATE %in% names(labels))

# Reorder of provinces.
plague_timeSeries <- plague_timeSeries %>%
  mutate(ID=as.character(labels[as.character(PROVINCE_STATE)])) %>%
  mutate(ID=as.numeric(substr(gsub("[[:punct:]][A-z ]*", "", ID), start=1, stop=2))) %>%
  mutate(PROVINCE_STATE = as.character(labels[as.character(PROVINCE_STATE)]))
plague_timeSeries$PROVINCE_STATE <- factor(plague_timeSeries$PROVINCE_STATE,
                                        levels=unique(plague_timeSeries$PROVINCE_STATE)[order(unique(plague_timeSeries$ID))])

# Arrange the data into standardised monthly deaths.
plague_norm <- plague_timeSeries %>%
  mutate(DATE = as.Date(DATE, origin="1970-01-01")) %>%
  mutate(VALUE = ifelse(TYPE=="INCIDENCE", log10(VALUE + 1), VALUE)) %>%
  group_by(PROVINCE_STATE, YEAR, TYPE) %>%
  filter(sum(VALUE) > 0) %>%
  mutate(VALUE = log10(VALUE + 1)) %>%
  mutate(VALUE = (VALUE - mean(VALUE))/sd(VALUE)) %>%
  mutate(DATE = as.Date(gsub("[0-9]{4}", "1900", DATE)))

# Construct the time series of climate and incidence.
gg_timeSeries <- ggplot(plague_timeSeries, aes(x = DATE, y = VALUE, alpha = TYPE, size = TYPE)) +
  geom_col(data=filter(plague_timeSeries, TYPE == "Rainfall"), colour = brewer.pal(3, "Set1")[2], aes(fill = TYPE)) +
  geom_line(data=filter(plague_timeSeries, TYPE != "Rainfall"), aes(colour = TYPE)) +
  scale_x_date(expand=c(0.01,0)) +
  scale_colour_manual("Data", limits=c("INCIDENCE", "Temperature", "Humidity"),
                      labels=c("Incidence", "Temperature", "Humidity"),
                      values=c("black", brewer.pal(3, "Set1")[c(1,3)])) +
  scale_fill_manual("Data", limits=c("Rainfall"),
                      labels=c("Rainfall"),
                      values=c(brewer.pal(3, "Set1")[2])) +
  scale_alpha_manual(guide=FALSE,
                     limits=c("INCIDENCE", "Temperature", "Rainfall", "Humidity"),
                     values=c(1, 0.5, 0.3, 0.5)) +
  scale_size_manual(guide=FALSE,
                    limits=c("INCIDENCE", "Temperature", "Rainfall", "Humidity"),
                    values=c(0.75, 0.4, 0.4, 0.4)) +
  facet_wrap(~PROVINCE_STATE, ncol=1, scales="free_y") +
  gg_theme + 
  theme(panel.border = element_rect(colour="black", fill=NA),
        plot.margin = unit(c(5,0,5,10), "mm")) +
  xlab("Year") +
  ylab("Plague deaths per 1,000,000\nNoramlised climate value") +
  guides(colour=guide_legend(title.hjust=0.5, title.position="top"),
         fill=guide_legend(title.hjust=0.5, title.position="top"))

# Plot the cases per month.
gg_monthly <- ggplot(plague_norm, aes(x = DATE)) +
  geom_line(stat="smooth", method="lm", formula = y~splines::bs(x, degree=5),
            aes(y=VALUE, group=as.factor(paste(YEAR, PROVINCE_STATE, TYPE)), colour=TYPE),
            se=FALSE, alpha=0.1, size=1) +
  xlab("Month") +
  ylab("Standardised value") +
  scale_x_date(date_labels="%b", breaks=as.Date(c("01-01-1900",
                                                  "01-04-1900",
                                                  "01-07-1900",
                                                  "01-10-1900"), format="%d-%m-%Y")) +
  scale_y_continuous(position="right") +
  scale_colour_manual("Data", limits=c("INCIDENCE", "Temperature", "Rainfall", "Humidity"),
                      values=c("black", brewer.pal(3, "Set1"))) +
  gg_theme +
  theme(aspect.ratio = 1) +
  theme(axis.title.y=element_text(margin=margin(0,0,0,8,"mm"))) +
  theme(axis.text.y = element_text(margin=margin(0,0,0,4,"mm"))) +
  theme(panel.border = element_rect(colour="black", fill=NA)) +
  theme(plot.margin = unit(c(5,10,5,0), "mm")) +
  guides(colour=guide_legend(title.hjust=0.5, title.position="top")) +
  facet_wrap(.~PROVINCE_STATE, ncol=1)

# Combine in a single plot.
gg_combined <- grid.arrange(gg_timeSeries, gg_monthly,
                            layout_matrix=matrix(c(1,1,1,1,2), nrow=1, ncol=5))
filename <- "fig_plague_climate_time.pdf"
ggsave(filename=filename, plot=gg_combined,
       height=8.27, width=11.69, dpi=600, path="Figures", scale=1.2)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename ,"\""))
