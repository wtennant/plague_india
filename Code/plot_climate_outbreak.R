# plot_climate_outbreak.R: Plot climate values against probability of an outbreak
# for different outbreak thresholds.

# Clear the workspace
rm(list=ls())

# Load the data visualisation and manipulation libraries.
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstanarm)

# Load the default gg plot theme.
source("gg_theme.R")

# Define a vector of outbreak sizes.
outbreakSizes <- c(0, 1, 10, 100)

# Load in the plague-climate data.
plague <- read.csv("Data/india_plague_climate.csv")

# Aggregate the data by year.
plague <- mutate(plague, MONTH = as.character(MONTH)) %>%
  group_by(PROVINCE_STATE, YEAR, CLIMATE) %>%
  summarise(INCIDENCE = sum(INCIDENCE),
            VALUE = mean(VALUE))

# Define outbreak occurance at different thresholds.
outbreaks <- plague
outbreaks$OUTBREAK <- 1
for (i in seq(1, nrow(outbreaks))){
  outbreaks$OUTBREAK[i] = paste(as.numeric(plague$INCIDENCE[i] > outbreakSizes), collapse=",")
}
outbreaks <- outbreaks %>%
  separate(OUTBREAK, into=paste0("OUTBREAK_", outbreakSizes), sep=",") %>%
  gather(key="OUTBREAK_SIZE", value="OUTBREAK", contains("OUTBREAK")) %>%
  mutate(OUTBREAK = as.numeric(OUTBREAK))

# For each defined outbreak size, fit a bayesian model.
all_intervals <- data.frame()
stats <- list()
for (size in unique(outbreaks$OUTBREAK_SIZE)){
  one_outbreak <- filter(outbreaks, CLIMATE == "Humidity", OUTBREAK_SIZE == size)
  model <- stan_glm(OUTBREAK ~ poly(VALUE/100, 2, raw=TRUE), data=one_outbreak, chains=1, family=binomial,
                    prior=cauchy(autoscale=FALSE, scale=2.5),
                    prior_intercept=cauchy(autoscale=FALSE, scale=10), QR=TRUE)
  posterior <- as.data.frame(model)
  posterior <- sample_n(posterior, size=1000)

  # For each sample, calculate the line.
  newdata <- data.frame(VALUE = seq(min(one_outbreak$VALUE), max(one_outbreak$VALUE), length.out=500)/100)
  
  # Define the transformation function
  trans <- function(x){
    x <- as.numeric(x)
    y <- x[1] + x[2]*newdata$VALUE + x[3]*newdata$VALUE^2
    y <- exp(y) / (1 + exp(y))
    y <- matrix(y, nrow=nrow(newdata))
    y <- apply(y, MARGIN=1, FUN=function(z){rbinom(2000, 1, prob=z)})
    return(colMeans(y))
  }
  
  # Sample and calculate credible intervals of line.
  lines <- apply(posterior, MARGIN=1, FUN=trans)
  intervals <- apply(lines, MARGIN=1, FUN=function(x){quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))})
  intervals <- t(intervals)
  intervals <- as.data.frame(intervals)
  intervals <- bind_cols(intervals, newdata) %>%
    mutate(OUTBREAK_SIZE = size)
  intervals$VALUE <- intervals$VALUE*100
  
  # Calculate the odds ratio.
  p <- colMeans(lines[(newdata$VALUE >= 60/100) & (newdata$VALUE <= 80/100),])
  q_1 <- p / colMeans(lines[newdata$VALUE <= 60/100,])
  q_2 <- p / colMeans(lines[newdata$VALUE >= 80/100,])
  stats[[which(size == unique(outbreaks$OUTBREAK_SIZE))]] <- data.frame("q_1"=q_1, "q_2"=q_2)
  
  # Add to overall intervals data frame.
  all_intervals <- bind_rows(all_intervals, intervals)
}

# Define facet labels.
facet_labels <- paste("Outbreak size of more than", outbreakSizes, "deaths per 1,000,000")
names(facet_labels) <- paste0("OUTBREAK_", outbreakSizes)

# Bin the climate data.
binWidth = 2
outbreak_humidity <- filter(outbreaks, CLIMATE=="Humidity")
bins <- seq(floor(min(outbreak_humidity$VALUE)/binWidth)*binWidth,
            ceiling(max(outbreak_humidity$VALUE)/binWidth)*binWidth,
            by=binWidth)
binned_outbreaks <- data.frame()
for (i in seq(1, length(bins) - 1))
{
    binned_outbreak <- filter(outbreak_humidity, VALUE >= bins[i], VALUE < bins[i+1]) %>%
      group_by(OUTBREAK_SIZE) %>%
      summarise(PROB=mean(OUTBREAK),
                COUNT=n()) %>%
      mutate(VALUE = mean(c(bins[i+1], bins[i]))) %>%
      mutate(MIN_VALUE = bins[i]+0.5, MAX_VALUE = bins[i+1]-0.5) %>%
      mutate(MAX_PROB = PROB + 1.96*sqrt(PROB*(1 - PROB) / COUNT)) %>%
      mutate(MIN_PROB = PROB - 1.96*sqrt(PROB*(1 - PROB) / COUNT))
    binned_outbreaks <- rbind(binned_outbreaks, binned_outbreak)
}

# Plot the data.
gg_outbreak <- outbreak_humidity %>%
  ggplot(aes(x = VALUE)) +
  geom_point(aes(y = OUTBREAK, shape="1"), alpha=0.1) +
  geom_point(data = binned_outbreaks, aes(y = PROB, shape="2")) +
 # geom_errorbarh(data = binned_outbreaks, aes(xmin = MIN_VALUE, xmax = MAX_VALUE, y = PROB), height=0.025) +
  geom_ribbon(data = all_intervals, aes(ymin=`2.5%`, ymax=`97.5%`, alpha="3")) +
  geom_ribbon(data = all_intervals, aes(ymin=`25%`, ymax=`75%`, alpha="2")) +
  geom_line(data = all_intervals, aes(y=`50%`, colour="1")) +
  scale_shape_manual("Data", labels=c("Outbreak or not in each province for each year",
                                  "Average binned across climate"),
                     values=c(1, 4)) +
  scale_colour_manual("Summary statistics", values="black", labels="Median") +
  scale_alpha_manual("Summary statistics", labels=c("50% credible interval", "95% credible interval"),
                     values=c(0.25, 0.1)) +
  xlab("Mean annual relative humidity (%)") +
  ylab("Probability of outbreak occurring") +
  gg_theme +
  theme(strip.background = element_blank()) +
  guides(colour=guide_legend( title.hjust=0.5, title.position="top", order=0),
         alpha=guide_legend(override.aes=list(colour="black"), title.hjust=0.5, title.position="top", order=0),
         shape=guide_legend(title.hjust=0.5, title.position="top", order=4)) +
  facet_wrap(~OUTBREAK_SIZE, labeller=labeller(OUTBREAK_SIZE = facet_labels), scales="free")
print(gg_outbreak)

# Save the figure
filename <- "fig_outbreak_humidity.pdf"
ggsave(filename=filename, plot=gg_outbreak,
       height=8.27, width=11.69, dpi=600, path="Figures")
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename ,"\""))
