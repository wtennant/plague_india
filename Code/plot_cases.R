# plot_cases.R: Plots cases.

# Clear the workspace.
rm(list = ls())

# Library.
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggforce)
library(scales)
library(gridExtra)

# Load the default ggplot theme.
source("gg_theme.R")

# Load the data.
plague <- read.csv("Data/india_plague.csv")

# Filter province/state.
plague <- mutate(plague, NATIVE = as.factor(NATIVE))

# Create dates.
plague <- mutate(plague, DATE = as.Date(paste("01", MONTH, YEAR), format = "%d %B %Y"))

# Join data with similar names together.
plague <- plague %>%
  mutate(PROVINCE_STATE = ifelse((PROVINCE_STATE == "Bengal") & (YEAR <= 1911), "Bihar And Orissa", as.character(PROVINCE_STATE))) %>%
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Bihar", "Orissa"), "Bihar And Orissa", as.character(PROVINCE_STATE))) %>%
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE %in% c("Central Province", "Berar"), "Central Province And Berar", as.character(PROVINCE_STATE))) %>%
  mutate(PROVINCE_STATE = ifelse(PROVINCE_STATE == "United Province", "United Province Of Agra And Oudh", as.character(PROVINCE_STATE)))

# Combine incidence from native states.
plague <- plague %>%
  group_by(PROVINCE_STATE, DATE, YEAR, MONTH) %>%
  summarise(CASES=sum(CASES)) %>%
  ungroup()

# Get the most endemic states.
endemic <- plague %>%
  group_by(PROVINCE_STATE) %>%
  summarise(CASES=sum(CASES)) %>%
  arrange(desc(CASES)) %>%
  .[1:6,]

# Only plot the most endemic states.
plague <- plague %>%
  filter(PROVINCE_STATE %in% endemic$PROVINCE_STATE)

# Arrange the data into standardised monthly deaths.
plague_norm <- plague %>%
  mutate(DATE = as.Date(DATE, origin="1970-01-01")) %>%
  group_by(PROVINCE_STATE, YEAR) %>%
  filter(sum(CASES) > 0) %>%
  mutate(CASES = log10(CASES + 1)) %>%
  mutate(CASES = (CASES - mean(CASES))/sd(CASES)) %>%
  mutate(DATE = as.Date(gsub("[0-9]{4}", "1900", DATE)))

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
plague <- plague %>%
  mutate(ID=as.character(labels[as.character(PROVINCE_STATE)])) %>%
  mutate(ID=as.numeric(gsub("[[:punct:]][A-z ]*", "", ID))) %>%
  mutate(PROVINCE_STATE = as.character(labels[as.character(PROVINCE_STATE)]))
plague$PROVINCE_STATE <- factor(plague$PROVINCE_STATE,
                                levels=unique(plague$PROVINCE_STATE)[order(unique(plague$ID))])

# Plot cases over time.
gg_plague <- ggplot(plague, aes(x = DATE, y = CASES)) +
  geom_line(aes(colour=PROVINCE_STATE), alpha = 0.75, position="identity", size=0.75) +
  xlab("Year") +
  ylab("Monthly reported plague-related deaths") +
  scale_x_date(expand=c(0.025, 0)) +
  scale_y_continuous(trans=pseudo_log_trans(base=10), breaks=c(0, 1e2, 1e4), labels=scales::comma) +
  scale_colour_manual(guide=FALSE, values=hcl(360*sort(unique(plague$ID))/25 + 15, 100, 65)) +
  facet_wrap(.~PROVINCE_STATE, nrow=6, ncol=1) +
  gg_theme +
  theme(panel.border = element_rect(colour="black", fill=NA),
        plot.margin = unit(c(5,0,5,10), "mm"))

# Plot the cases per month.
gg_plague_month <- ggplot(plague_norm, aes(x = DATE)) +
  geom_line(stat="smooth", method="lm", formula = y~splines::bs(x, 5),
            aes(y=CASES, group=as.factor(paste(YEAR, PROVINCE_STATE)), colour=PROVINCE_STATE),
            se=FALSE, alpha=0.1, size=1) +
  xlab("Month") +
  ylab("Standardised plague-related deaths\n") +
  scale_x_date(date_labels="%b", breaks=as.Date(c("01-01-1900",
                                          "01-04-1900",
                                          "01-07-1900",
                                          "01-10-1900"), format="%d-%m-%Y")) +
  scale_y_continuous(position="right") +
  scale_colour_manual(guide=FALSE, values=hcl(360*sort(unique(plague$ID))/25 + 15, 100, 65)) +
  gg_theme +
  theme(aspect.ratio = 1) +
  theme(axis.title.y=element_text(margin=margin(0,0,0,8,"mm"))) +
  theme(axis.text.y = element_text(margin=margin(0,0,0,4,"mm"))) +
  theme(panel.border = element_rect(colour="black", fill=NA)) +
  theme(plot.margin = unit(c(5,10,5,0), "mm")) +
  facet_wrap(.~PROVINCE_STATE, nrow=6, ncol=1)

# Combine in a single plot.
gg_combined <- grid.arrange(gg_plague, gg_plague_month,
             layout_matrix=matrix(c(1,1,1,1,2), nrow=1, ncol=5))
filename <- "fig_plague_cases.pdf"
ggsave(filename=filename, plot=gg_combined,
       height=8.27, width=11.69, dpi=600, path="Figures", scale=1.2)
system(paste0("bash -c \"pdfcrop --margin '14.22638' Figures/", filename, " Figures/", filename ,"\""))
