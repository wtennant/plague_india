# plot_cases_annual_time.R: Plot annual plague deaths over time.

# Clear the workspace.
rm(list=ls())

# Load in visualisation and data manipulation libraries.
library(ggplot2)
library(tidyr)
library(dplyr)

# Load in the default ggplot theme.
source("gg_theme.R")

# Load in the plague data.
plague <- read.csv("Data/india_plague.csv")

# Plot annual incidence per province over time.
plague_filtered <- plague %>%
  group_by(PROVINCE_STATE, YEAR) %>%
  summarise(CASES = sum(INCIDENCE, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE == "Bengal" & YEAR <= 1911, "Bengal Province", as.character(PROVINCE_STATE))) %>%
  group_by(YEAR) %>%
  mutate(MORE = sum(CASES > 100))

# Plot incidence over time.
gg_incidence <- ggplot(plague_filtered) +
  geom_boxplot(aes(x = YEAR, y=CASES, group=YEAR, fill=MORE)) +
  scale_y_continuous(trans=pseudo_log_trans(base=10), breaks=c(0, 1, 10, 100, 1000, 10000)) +
  scale_fill_distiller("Number of provinces with\nmore than 100 deaths",
                       palette="YlOrRd", direction=1, limits=c(0, max(plague_filtered$MORE))) +
  gg_theme +
  theme(legend.key.width=unit(15,"mm")) +
  xlab("Year") +
  ylab("Annual plague deaths per 1,000,000") +
  guides(fill=guide_colourbar(title.hjust=0.5, title.position="top",
                              ticks.colour="black", frame.colour="black"))
print(gg_incidence)

# Save the figure.
ggsave("india_plague_annual.pdf", plot=gg_incidence, dpi=600, height=8.27, width=11.69, path="Figures")
system("bash -c \"pdfcrop --margin '14.22638' Figures/india_plague_annual.pdf Figures/india_plague_annual.pdf\"")
