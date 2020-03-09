# plot_cases_monthly.R: Plots plague incidence per month for every year.

# Clear the workspace.
rm(list = ls())

# Library.
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# Load the default ggplot theme.
source("gg_theme.R")

# Load the data.
plague <- read.csv("Data/india_plague.csv")

# Define the state.
states <- c("Madras Presidency", "Bombay Presidency")

# Filter province/state.
plague <- filter(plague, PROVINCE_STATE %in% states) %>%
  mutate(NATIVE = as.factor(NATIVE))

# Create dates.
plague <- mutate(plague, DATE = as.Date(paste("01", MONTH, YEAR), format = "%d %B %Y")) %>%
  group_by(PROVINCE_STATE, YEAR, MONTH) %>%
  summarise(INCIDENCE = mean(INCIDENCE), DATE = mean(DATE)) %>%
  ungroup()

# Set look up table for facet labels.
labels <- c(
  "Bombay Presidency" = "15. Bombay Presidency",
  "Madras Presidency" = "22. Madras Presidency",
  "Bombay City" = "23. Bombay City (Mumbai)"
)

# Reorder of provinces.
plague  <- plague %>%
  mutate(ID=as.character(labels[as.character(PROVINCE_STATE)])) %>%
  mutate(ID=as.numeric(gsub("[[:punct:]][A-z ]*", "", ID))) %>%
  mutate(PROVINCE_STATE = as.character(labels[as.character(PROVINCE_STATE)]))
plague$PROVINCE_STATE <- factor(plague$PROVINCE_STATE,
                                levels=unique(plague$PROVINCE_STATE)[order(unique(plague$ID))])

# Now normalise the incidence data.
plague_norm <- plague %>%
  mutate(DATE = as.Date(DATE, origin="1970-01-01")) %>%
  mutate(DATE = as.Date(gsub("[0-9]{4}", "1900", DATE))) %>%
  group_by(PROVINCE_STATE, YEAR) %>%
  mutate(INCIDENCE = (INCIDENCE - mean(INCIDENCE))/sd(INCIDENCE))

# Plot cases over time.
gg_plague <- ggplot(plague_norm, aes(x = DATE)) +
  #geom_point(aes(y=INCIDENCE, colour=PROVINCE_STATE), size=1) +
  geom_line(stat="smooth", method="lm", formula = y~splines::bs(x, 3),
            aes(y=INCIDENCE, group=as.factor(paste(YEAR, PROVINCE_STATE)), colour=PROVINCE_STATE),
            se=FALSE, alpha=0.25, size=1) +
  xlab("Month") +
  ylab("Standardised incidence") +
  scale_x_date(date_labels="%B", date_breaks="1 month") +
  scale_colour_manual("Location", guide=FALSE, values=hcl(360*sort(unique(plague_norm$ID))/25 + 15, 100, 65)) +
  gg_theme +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  guides(colour = guide_legend(title.hjust=0.5, title.position = "top",
                               override.aes=list(alpha=1)))

# Print the figure.
print(gg_plague)

# Save the figure
filename <- "fig_plague_bombay.pdf"
ggsave(filename, plot=gg_plague, height=8.27, width=11.69, path="Figures", scale=0.8)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename ,"\""))
