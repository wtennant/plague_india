# plot_climate_lead_offset.R: Plot the provincial climate effects.

# Clear the workspace.
rm(list=ls())

# Load data manipulation and visualisation libraries.
library(ggplot2)
library(tidyr)
library(dplyr)
library(rstanarm)
library(gridExtra)

# Load the ggplot theme.
source("gg_theme.R")

# Define an outbreak incidence threshold.
# And define a threshold for how many outbreaks.
outbreak = 10
outbreaks = 2

# Load in plague-climate phase data.
plague <- read.csv("Data/india_plague_climate_phase.csv")

# Join select provinces.
plague <- plague %>%
  # Combine Bihar and Orissa with Bengal and Bihar, and Orissa.
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Bihar", "Orissa"), "Bihar And Orissa", as.character(PROVINCE_STATE))) %>%
  # Combine Eastern Bengal and Assam with Assam.
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Eastern Bengal And Assam"), "Assam", as.character(PROVINCE_STATE))) %>%
  group_by(PROVINCE_STATE, CLIMATE, YEAR, MONTH) %>%
  summarise(PHASE_CLIMATE = atan2(mean(sin(PHASE_CLIMATE)), mean(cos(PHASE_CLIMATE))),
            PHASE_PLAGUE = atan2(mean(sin(PHASE_PLAGUE)), mean(cos(PHASE_PLAGUE))),
            INCIDENCE = mean(INCIDENCE)) %>%
  ungroup()

labels <- c("North West Frontier Province" = "1.   North-West Frontier Province",
            "Jammu And Kashmir" = "2.   Jammu and Kashmir",
            "Baluchistan Agency" = "3.   Baluchistan Agency",
            "Punjab" = "4.   Punjab",
            "United Province Of Agra And Oudh" = "6.   United Province of Agra and Oudh",
            "Sindh" = "7.   Sindh",
            "Rajputana Agency" = "8.   Rajputana Agency",
            "Central India Agency" = "10. Central India Agency",
            "Bihar And Orissa" = "11. Bihar and Orissa",
            "Bengal" = "12. Bengal",
            "Assam" = "13. Assam",
            "Upper Burma" = "14. Upper Burma",
            "Bombay Presidency" = "15. Bombay Presidency",
            "Central Province And Berar" = "16. Central Province and Berar",
            "Hyderabad" = "17. Hyderabad",
            "Lower Burma" = "18. Lower Burma",
            "Mysore" = "21. Mysore",
            "Madras Presidency" = "22. Madras Presidency",
            "Burma" = "19. Burma")
plague <- plague %>%
  mutate(ID=as.character(labels[as.character(PROVINCE_STATE)])) %>%
  mutate(ID=as.numeric(substr(gsub("[[:punct:]][A-z ]*", "", ID), start=1, stop=2))) %>%
  mutate(PROVINCE_STATE = as.character(labels[as.character(PROVINCE_STATE)]))
plague$PROVINCE_STATE <- factor(plague$PROVINCE_STATE,
                                levels=unique(plague$PROVINCE_STATE)[order(unique(plague$ID))])

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
  select(PROVINCE_STATE, YEAR, CLIMATE, PHASE_CLIMATE, PHASE_PLAGUE, INCIDENCE) %>%
  group_by(PROVINCE_STATE, CLIMATE) %>%
  mutate(PHASE_CLIMATE_AVG = atan2(mean(sin(PHASE_CLIMATE)), mean(cos(PHASE_CLIMATE)))) %>%
  mutate(PHASE_PLAGUE_AVG = atan2(mean(sin(PHASE_PLAGUE)), mean(cos(PHASE_PLAGUE)))) %>%
  group_by(PROVINCE_STATE, CLIMATE, YEAR) %>%
  mutate(PHASE_CLIMATE = atan2(mean(sin(PHASE_CLIMATE)), mean(cos(PHASE_CLIMATE))),
         PHASE_PLAGUE = atan2(mean(sin(PHASE_PLAGUE)), mean(cos(PHASE_PLAGUE)))) %>%
  summarise(PHASE_CLIMATE = mean(PHASE_CLIMATE - PHASE_CLIMATE_AVG),
            PHASE_PLAGUE = mean(PHASE_PLAGUE - PHASE_PLAGUE_AVG),
            INCIDENCE = sum(INCIDENCE)) %>%
  mutate(PHASE_CLIMATE = atan2(sin(PHASE_CLIMATE), cos(PHASE_CLIMATE)),
         PHASE_PLAGUE = atan2(sin(PHASE_PLAGUE), cos(PHASE_PLAGUE))) %>%
  mutate(PHASE_CLIMATE = PHASE_CLIMATE/2/pi*12,
         PHASE_PLAGUE = PHASE_PLAGUE/2/pi*12)

# Change the order of climate factors.
plague$CLIMATE <- factor(plague$CLIMATE, levels=c("Temperature", "Rainfall", "Humidity"))

# Fit a Bayesian model to the humidity data.
humidity <- filter(plague, CLIMATE == "Humidity")
model <- stan_glm(PHASE_PLAGUE ~ PHASE_CLIMATE, data=humidity, chains=1, family=gaussian,
                  prior=cauchy())
posterior <- as.data.frame(model)
posterior <- sample_n(posterior, size=1000)

# For each sample, calculate the line.
newdata <- data.frame(PHASE_CLIMATE = seq(min(humidity$PHASE_CLIMATE), max(humidity$PHASE_CLIMATE), length.out=500))
lines <- apply(posterior, MARGIN=1, FUN=function(x){rnorm(200*500, mean = x[1] + newdata$PHASE_CLIMATE*x[2], sd=x[3])})
lines <- matrix(lines, nrow=500, ncol=dim(lines)[2]*200)
intervals <- apply(lines, MARGIN=1, FUN=function(x){quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))})
rm(lines) # Get rid of it because it's a memory hog!
intervals <- t(intervals)
intervals <- as.data.frame(intervals)
intervals <- bind_cols(intervals, newdata)

# Calculate the mean gradient if data is agregated.
plague_grad <- plague %>%
  group_by(CLIMATE,PROVINCE_STATE) %>%
  summarise(GRAD = lm(PHASE_PLAGUE ~ PHASE_CLIMATE)$coefficients[2],
            x = max(PHASE_CLIMATE)*0.95,
            y = max(PHASE_PLAGUE)*0.95,
            COR_S = cor(PHASE_PLAGUE, PHASE_CLIMATE, method="spearman"),
            COR_P = cor(PHASE_PLAGUE, PHASE_CLIMATE, method="pearson"),
            GROUP_X = max(x), GROUP_Y = max(y)) %>%
  ungroup() %>%
  mutate(x = max(x), y = max(y))

# Plot by climate type and location.
plague_temp_rain <- filter(plague, CLIMATE != "Humidity")
gg_offset <- ggplot(plague_temp_rain, aes(x = PHASE_CLIMATE, y = PHASE_PLAGUE)) +
  geom_point(aes(colour=CLIMATE), alpha=0.5, shape=1) +
  geom_smooth(aes(group=CLIMATE), method="lm", se=FALSE, colour="black", size=1.5) +
  geom_text(data=filter(plague_grad, CLIMATE!="Humidity"),
            aes(label=paste0("Gradient: ", round(GRAD, 2))), hjust=1, vjust=1,
            x = 0.95*max(abs(plague_temp_rain$PHASE_PLAGUE)),
            y = 0.95*max(abs(plague_temp_rain$PHASE_PLAGUE))) +
  scale_x_continuous(limits=c(-max(abs(plague$PHASE_PLAGUE)), max(abs(plague$PHASE_PLAGUE)))) +
  scale_y_continuous(limits=c(-max(abs(plague$PHASE_PLAGUE)), max(abs(plague$PHASE_PLAGUE)))) +
  scale_colour_brewer(palette="Set1", guide=FALSE) +
  facet_wrap(~CLIMATE, nrow=2) +
  xlab("Climate oscillation phase shift\nfrom the mean (months)") +
  ylab("Outbreak phase shift from the mean (months)") +
  gg_theme +
  theme(panel.border = element_rect(colour="black", fill=NA)) +
  theme(strip.text=element_text(size=12)) +
  coord_fixed()

# Save the figures.
filename <- "fig_india_climate_offset_temperature_rainfall.pdf"
ggsave(filename, plot = gg_offset,
       path="Figures", dpi=600, height=11.69, width=8.27)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename, "\""))

# Plot humidity data by itself.
plague_humidity <- filter(plague, CLIMATE == "Humidity")
gg_offset <- ggplot(plague_humidity, aes(x = PHASE_CLIMATE)) +
  geom_point(aes(y = PHASE_PLAGUE), alpha=0.5, shape=1) +
  geom_ribbon(data=intervals, aes(ymin = `2.5%`, ymax = `97.5%`, alpha="3")) +
  geom_ribbon(data=intervals, aes(ymin = `25%`, ymax = `75%`, alpha="2")) +
  geom_line(data=intervals, aes(y = `50%`, alpha="1"), colour="black") +
  scale_x_continuous(limits=c(-max(abs(plague_humidity$PHASE_PLAGUE)), max(abs(plague_humidity$PHASE_PLAGUE)))) +
  scale_y_continuous(limits=c(-max(abs(plague_humidity$PHASE_PLAGUE)), max(abs(plague_humidity$PHASE_PLAGUE)))) +
  scale_alpha_manual("", labels=c("Median", "50% credible interval", "95% credible interval"),
                     values=c(0.75, 0.25, 0.1)) +
  xlab("Humidity oscillation phase shift\nfrom the mean (months)") +
  ylab("Outbreak phase shift from the mean (months)") +
  gg_theme +
  coord_fixed() +
  theme(aspect.ratio = 1) +
  guides(alpha=guide_legend(override.aes=list(colour="black"), order=0))

# Print the figure.
print(gg_offset)

# Calculate the posterior density and the quantiles.
dens <- data.frame("PHASE_CLIMATE" = density(posterior$PHASE_CLIMATE, adjust=1.5)$x,
                   "y" = density(posterior$PHASE_CLIMATE, adjust=1.5)$y)
quans <- quantile(posterior$PHASE_CLIMATE, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))

# Plot the posterior distribution of the gradient.
gg_posterior <- ggplot(posterior, aes(x = PHASE_CLIMATE)) +
  stat_density(geom="line", adjust=1.5) +
  geom_area(data=filter(dens, PHASE_CLIMATE >= quans[1], PHASE_CLIMATE <= quans[5]),
            aes(y = y, alpha="3"), fill="black") +
  geom_area(data=filter(dens, PHASE_CLIMATE >= quans[2], PHASE_CLIMATE <= quans[4]),
            aes(y = y, alpha="2"), fill="black") +
  geom_line(data=data.frame(PHASE_CLIMATE = as.numeric(rep(quans[3], 2)),
                            y = c(0, dens$y[which.min(abs(dens$PHASE_CLIMATE - quans[3]))])),
            aes(y = y, alpha="1")) +
  xlab("Time delay in outbreak given one month\ndelay in humidity (months)") +
  ylab("Density") +
  scale_y_continuous(expand = c(0,0)) +
  scale_alpha_manual("", labels=c("Median", "50% credible interval", "95% credible interval"),
                     values=c(0.75, 0.25, 0.1)) +
  gg_theme +
  theme(aspect.ratio = 1) +
  guides(alpha=guide_legend(override.aes=list(colour="black"), order=0))
print(gg_posterior)

# Save the figures.
gg_combined <- grid.arrange(gg_offset, gg_posterior, nrow = 1)
filename <- "fig_india_climate_offset_humidity.pdf"
ggsave(filename, plot = gg_combined,
       path="Figures", dpi=600, height=8.27, width=11.69)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename, "\""))
