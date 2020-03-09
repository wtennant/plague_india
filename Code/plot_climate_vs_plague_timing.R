# plot_climate_vs_plague_timing.R: Plot the timing of climate oscillations
# against plague outbreaks.

# Clear the workspace
rm(list = ls())

# Load data manipulation libraries.
library(tidyr)
library(dplyr)
library(ggplot2)
library(rstanarm)

# Load default ggplot theme.
source("gg_theme.R")

# Define an outbreak incidence threshold.
# And define a threshold for how many outbreaks.
outbreak = 10
outbreaks = 2

# Load in the phase data.
phase <- read.csv("Data/india_plague_climate_phase.csv") %>%
  mutate(DATE = as.Date(DATE, format="%Y-%m-%d"))

# Calculate the number of outbreaks per province and then filter out data
# which does not have enough outbreaks.
phase <- phase %>%
  group_by(PROVINCE_STATE, CLIMATE, YEAR) %>%
  mutate(OUTBREAK = sum(INCIDENCE) >= outbreak) %>%
  filter(OUTBREAK) %>%
  group_by(PROVINCE_STATE, CLIMATE) %>%
  mutate(OUTBREAK = sum(OUTBREAK)) %>%
  filter(OUTBREAK >= outbreaks) %>%
  ungroup()

# Calculate the phase difference of climate variables.
phase_filtered <- phase %>%
  select(-ANGLE, -TIME) %>%
  spread(CLIMATE, PHASE_CLIMATE) %>%
  # Combine Bihar and Orissa with Bengal and Bihar, and Orissa.
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Bihar", "Orissa"), "Bihar And Orissa", as.character(PROVINCE_STATE))) %>%
  # Combine Eastern Bengal and Assam with Assam.
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Eastern Bengal And Assam"), "Assam", as.character(PROVINCE_STATE))) %>%
  group_by(PROVINCE_STATE, YEAR) %>%
  summarise(PHASE = atan2(mean(sin(PHASE_PLAGUE)), mean(cos(PHASE_PLAGUE))), INCIDENCE = sum(INCIDENCE),
            Temperature = atan2(mean(sin(Temperature), na.rm=TRUE), mean(cos(Temperature), na.rm=TRUE)),
            Rainfall = atan2(mean(sin(Rainfall), na.rm=TRUE), mean(cos(Rainfall), na.rm=TRUE)),
            Humidity = atan2(mean(sin(Humidity), na.rm=TRUE), mean(cos(Humidity), na.rm=TRUE))) %>%
  gather(key=CLIMATE, value=PHASE_CLIMATE, c("Temperature", "Rainfall", "Humidity")) %>%
  ungroup()

# Change the level order of climate variables.
phase_filtered$CLIMATE <- factor(phase_filtered$CLIMATE,
                                 levels=c("Temperature", "Rainfall", "Humidity"))
  
# Set labels for provinces.
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
phase_filtered <- phase_filtered %>%
  mutate(ID=as.character(labels[as.character(PROVINCE_STATE)])) %>%
  mutate(ID=as.numeric(substr(gsub("[[:punct:]][A-z ]*", "", ID), start=1, stop=2))) %>%
  mutate(PROVINCE_STATE = as.character(labels[as.character(PROVINCE_STATE)]))
phase_filtered$PROVINCE_STATE <- factor(phase_filtered$PROVINCE_STATE,
                                        levels=unique(phase_filtered$PROVINCE_STATE)[order(unique(phase_filtered$ID))])

# Define guide lines.
guide_lines <- data.frame(PHASE_CLIMATE = c(6, 0),
                          PHASE = c(6, 0))

# Separate the temperature, rainfall and humidity data.
phase_temp_rain <- filter(phase_filtered, CLIMATE %in% c("Temperature", "Rainfall"),
                          !is.nan(PHASE_CLIMATE)) %>%
  mutate(PHASE_CLIMATE = (PHASE_CLIMATE/2/pi*12) %% 12) %>%
  mutate(PHASE = PHASE /2 / pi * 12)
phase_humidity <- filter(phase_filtered, CLIMATE %in% c("Humidity"),
                         !is.nan(PHASE_CLIMATE)) %>%
  mutate(PHASE_CLIMATE = (PHASE_CLIMATE/2/pi*12) %% 12) %>%
  mutate(PHASE = PHASE /2 / pi * 12)

# For humidity, fit a Bayesian model!
model_humidity <- stan_glm(PHASE ~ PHASE_CLIMATE, data=phase_humidity,
                           prior=cauchy(autoscale=FALSE, scale=2.5),
                           prior_intercept=cauchy(autoscale=FALSE, scale=10),
                           prior_aux=exponential(rate=1, autoscale=FALSE),
                           chains=4, iter=5000)
  
# Draw for the posterior predictive distribution for the mean.
newdata <- data.frame(PHASE_CLIMATE = c(min(phase_humidity$PHASE_CLIMATE), max(phase_humidity$PHASE_CLIMATE)))
posterior <- as.data.frame(model_humidity)
lines <- as.data.frame(apply(newdata, MARGIN=1, FUN=function(x){return(posterior[1] + posterior[2]*x)}))
intervals <- apply(lines, MARGIN=2, FUN=function(x){quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))}) %>%
  t() %>% as.data.frame() %>%
  bind_cols(newdata)

# Plot temperature timing versus plague timing.
gg_phase <- ggplot(phase_temp_rain, aes(x=PHASE_CLIMATE, y=PHASE)) +
  geom_point(aes(size=INCIDENCE, fill=PROVINCE_STATE), shape=21, alpha=0.5, na.rm=TRUE) +
  scale_x_reverse(limits=c(12, 0), breaks=c(12, 9, 6, 3, 0), labels=c("Jan", "Apr", "Jul", "Oct", "Jan")) +
  scale_y_reverse(limits=c(6, -6), labels=c("Jul", "Apr", "Jan", "Oct", "Jul")) +
  #scale_fill_distiller("Annual incidence", type="seq", palette="PuBu", direction=1, trans="log10") +
  scale_fill_discrete("Province") +
  scale_size_area("Annual deaths per 1,000,000", breaks=c(100, 1000, 10000), labels=c("100", "1,000", "10,000"), trans="log10") +
  xlab("Time of peak in climate variable") +
  ylab("Time of annual peak in plague deaths") +
  gg_theme +
  theme(aspect.ratio=1) +
  theme(legend.position="right",
        panel.border = element_rect(colour="black", fill=NA)) +
  theme(strip.text=element_text(size=12)) +
  guides(fill=guide_legend(title.position = "top", override.aes=list(size=4), order=1),
         size=guide_legend(title.position = "top"), order=0) +
  facet_wrap(~CLIMATE, nrow=2)
print(gg_phase)

# Save the figure.
filename <- "fig_timing_diff_temperature_rain.pdf"
ggsave(filename, plot=gg_phase, height=11.69, width=8.27, path="Figures", scale=1.25)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename ,"\""))

# Plot humidity timing versus plague timing.
gg_phase <- ggplot(phase_humidity, aes(x=PHASE_CLIMATE)) +
  geom_line(data=guide_lines, aes(y=PHASE, linetype="3"), size=0.5) +
  geom_point(aes(size=INCIDENCE, fill=PROVINCE_STATE, y=PHASE), shape=21, alpha=0.4, na.rm=TRUE) +
  geom_line(data=intervals, aes(y = `97.5%`, linetype="2"), size=0.6) +
  geom_line(data=intervals, aes(y = `2.5%`, linetype="2"), size=0.6) +
  geom_line(data=intervals, aes(y = `50%`, linetype="1"), size=1) +
  scale_x_reverse(limits=c(6, 0), breaks=c(6, 3, 0), labels=c("Jul", "Oct", "Jan"), expand=c(0,0.5)) +
  scale_y_reverse(limits=c(6, -6), labels=c("Jul", "Apr", "Jan", "Oct", "Jul"), expand=c(0,0.5)) +
  scale_fill_manual("Province", values=hcl(360*sort(unique(phase_humidity$ID))/26 + 15, 100, 65)) +
  scale_size_area(breaks=c(100,1000,10000), labels=c("100", "1,000", "10,000"), "Annual deaths per 1,000,000", trans="log10") +
  scale_linetype_manual("Summary statistics", values=c("solid", "longdash", "dotted"),
                        labels=c("Median", "95% credible interval of the mean", "Lag-time of zero months")) +
  xlab("Time of peak in humidity") +
  ylab("Time of annual peak in plague deaths") +
  gg_theme +
  coord_fixed() +
  theme(legend.position="right") +
  guides(fill=guide_legend(title.position = "top", override.aes=list(size=4), order=2),
         size=guide_legend(title.position = "top", order=0),
         linetype=guide_legend(title.position="top", override.aes=list(size=c(1,0.6,0.5)), order=1))
print(gg_phase)

# Save the figure.
filename <- "fig_timing_diff_humidity.pdf"
ggsave(filename, plot=gg_phase, height=8.27, width=11.69, path="Figures", scale=1.05)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename ,"\""))

# Quick correlation calculation.
correlation <- filter(phase_filtered, INCIDENCE >= 1, !is.nan(PHASE_CLIMATE), !is.na(PHASE_CLIMATE)) %>%
  mutate(PHASE_CLIMATE = (PHASE_CLIMATE/2/pi*12) %% 12) %>%
  mutate(PHASE = PHASE/2/pi*12) %>%
  group_by(CLIMATE) %>%
  summarise(SPEARMAN = cor(PHASE_CLIMATE, PHASE, method="spearman"),
            PEARSON = cor(PHASE_CLIMATE, PHASE, method="pearson"))
