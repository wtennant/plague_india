# plot_climate_vs_cases_annual.R: Plot the climate variables against plague cases.

# Clear the workspace.
rm(list = ls())

# Load the necessary libraries.
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(rstanarm)

# Load the ggplot theme.
source("gg_theme.R")
gg_theme <- gg_theme +
  theme(aspect.ratio = 1)

# Load in the plague-climate data.
plague <- read.csv("Data/india_plague_climate.csv")

# Join the plague data with the climate data.
plague <- mutate(plague, MONTH = as.character(MONTH)) %>%
  group_by(PROVINCE_STATE, YEAR, CLIMATE) %>%
  summarise(INCIDENCE = sum(INCIDENCE),
            VALUE = mean(VALUE)) %>%
  filter(INCIDENCE > 0)

# Change order of climate factors.
plague$CLIMATE <- factor(plague$CLIMATE,
                         levels=c("Temperature", "Rainfall", "Humidity"))

# Fit a Bayesian model to the humidity data.
humidity <- filter(plague, CLIMATE == "Humidity")
model <- stan_glm(log10(INCIDENCE) ~ poly(VALUE/100, 2, raw=TRUE), data=humidity, chains=1, family=gaussian,
                  iter=5000, adapt_delta=0.999,
                  prior=cauchy(autoscale=FALSE, scale=2.5),
                  prior_intercept=cauchy(autoscale=FALSE, scale=10),
                  prior_aux=exponential(rate=1, autoscale=FALSE),
                  QR=TRUE)
posterior <- as.data.frame(model)
posterior <- sample_n(posterior, size=1000)

# For each sample, calculate the line.
newdata <- data.frame(VALUE = seq(min(humidity$VALUE), max(humidity$VALUE), length.out=500)/100)
lines <- apply(posterior, MARGIN=1, FUN=function(x){rnorm(200*500, mean = x[1] + newdata$VALUE*x[2] + x[3]*newdata$VALUE^2, sd=x[4])})
lines <- matrix(lines, nrow=500, ncol=dim(lines)[2]*200)
lines <- 10^lines
intervals <- apply(lines, MARGIN=1, FUN=function(x){quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))})
rm(lines) # Get rid of it because it's a memory hog!
intervals <- t(intervals)
intervals <- as.data.frame(intervals)
intervals <- bind_cols(intervals, newdata)
intervals$VALUE <- intervals$VALUE*100

# Plot annual incidence against climate values.
gg_climate <- ggplot(filter(plague, CLIMATE!="Humidity"), 
                      aes(x = VALUE, y = INCIDENCE)) +
  geom_point(aes(colour=CLIMATE), alpha=0.5) +
  scale_y_continuous(trans=log_trans(base=10),
                     breaks=c(10^(seq(-2, 5, 1))),
                     label=c(10^(seq(-2, 5, 1)))) +
  scale_color_brewer(type="qual", palette = "Set1", guide=FALSE) +
  xlab("Mean annual value") +
  ylab("Annual plague deaths per 1,000,000") +
  gg_theme + 
  theme(panel.border=element_rect(colour="black", fill=NA)) +
  facet_wrap(~CLIMATE, scales="free_x")
print(gg_climate)

# Save figures
filename <- "fig_temp_rain_magnitude.pdf"
ggsave(filename, plot = gg_climate,
       path="Figures", dpi=600, height=8.67, width=11.69)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename, "\""))

# Plot annual incidence against climate values.
gg_humidity <- ggplot(filter(plague, CLIMATE=="Humidity"), 
                     aes(x = VALUE)) +
  geom_point(aes(y = INCIDENCE), alpha=0.5, shape=1) +
  geom_ribbon(data=intervals, aes(ymin = `2.5%`, ymax = `97.5%`, alpha="3")) +
  geom_ribbon(data=intervals, aes(ymin = `25%`, ymax = `75%`, alpha="2")) +
  geom_line(data=intervals, aes(y = `50%`, alpha="1"), colour="black") +
  scale_y_continuous(trans=log_trans(base=10),
                     breaks=c(10^(seq(-2, 5, 1))),
                     label=c(10^(seq(-2, 5, 1)))) +
  scale_alpha_manual("", labels=c("Median", "50% credible interval", "95% credible interval"),
                     values=c(0.75, 0.25, 0.1)) +
  xlab("Mean annual relative humidity (%)") +
  ylab("Annual plague deaths per 1,000,000") +
  gg_theme +
  theme(legend.position = "right") +
  guides(alpha=guide_legend(override.aes=list(colour="black"), order=0))
print(gg_humidity)

# Save figures
filename <- "fig_humidity_magnitude.pdf"
ggsave(filename, plot = gg_humidity,
       path="Figures", dpi=600, height=8.67, width=11.69)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename, "\""))
