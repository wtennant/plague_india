# gg_theme.R: Contains the code for the default theme to use in ggplot.

# Set the default theme.
gg_theme <- theme_bw() +
    theme(axis.line = element_line(colour="black")) +
    theme(axis.text = element_text(colour="black")) +
    theme(axis.text.x = element_text(margin=margin(4,0,0,0,"mm"))) +
    theme(axis.text.y = element_text(margin=margin(0,4,0,0,"mm"))) +
    theme(axis.ticks = element_line(colour="black")) +
    theme(axis.ticks.length=unit(4,"mm")) +
    theme(axis.title.x=element_text(margin=margin(8,0,0,0,"mm"))) +
    theme(axis.title.y=element_text(margin=margin(0,8,0,0,"mm"))) +
    theme(legend.key = element_blank()) +
    theme(legend.key.width = unit(7.5,"mm")) +
    theme(legend.key.height = unit(7.5,"mm")) +
    theme(legend.position = "top") +
    theme(legend.title = element_text(face="bold", size=14)) +
    theme(panel.border = element_blank()) + 
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(plot.margin = unit(c(5,10,5,10), "mm")) +
    theme(strip.text = element_text(size=10)) +
    theme(text = element_text(size=16, colour="black"))
