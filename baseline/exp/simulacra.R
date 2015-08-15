
# To run experiment, set working directory and source("simulacra.R")

# Load packages
library(ggplot2)
library(reshape2)

# Theme for experiment
te <- theme(text = element_text(size=0),
            axis.text.x = element_text(size=16),
            axis.ticks.y = element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_rect(fill = "gray95"))
options(digits=3)

# Init data
load("stimuli.RData") # Load stimuli
subj <- readline(prompt="Subject ID: ")
# Load sample statistics into results data frame
d <- data.frame(cbind("id" = subj, sample_stats))

# Begin experiment
cat("\014", "\n\n##################### >> SIMULACRA << #####################\n\n")

# Loop over stimuli in random order
for (trial in sample(1:nrow(d))) {
    # Create plot
    tmp <- melt(data.frame(g1 = samples_1[[trial]], 
                           g2 = samples_2[[trial]]), id.vars=NULL)
    print(ggplot(tmp, aes(variable, value)) + 
              geom_point(position = position_jitter(h=0, w=.08), size=3) +
              coord_cartesian(ylim = c(min(tmp$value)-1, max(tmp$value)+1)) +
              scale_x_discrete(labels=c("group1", "group2")) +
              te)
    # Get response
    resp <- NA
    while (!(resp %in% c("y", "n", "q"))) {
        resp <- readline(prompt="Signal present? (y/n): ")    
    }
    if (resp=="q") {break}
    d[trial,"resp"] <- ifelse(resp=="y", 1, 0) # Convert to 1/0
}

# Save subject's data file
write.csv(d, paste("../raw_data/", subj, ".csv", sep=""))

# Show output
source("simulacra_output.R")
source("simulacra_manual_output.R")
    