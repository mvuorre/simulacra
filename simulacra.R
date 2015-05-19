
# If you need to install packages
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("BayesFactor")
# install.packages("reshape2")

# To run experiment, set working directory and source("simulacra.R")

# Load packages
library(ggplot2)
library(BayesFactor)
library(reshape2)

# Customize output
options(digits=2)
# Theme for output
tt <- theme_bw(base_size = 16) + 
    theme(axis.title=element_text(size=14),
          axis.title.y=element_text(angle=90))
# Theme for experiment
te <- theme(text = element_text(size=0),
            axis.text.x = element_text(size=16),
            axis.ticks.y = element_blank(),
            panel.grid = element_blank(),
            axis.title.y=element_blank())
theme_set(tt)
# Set experiment options
trials=20
n=20

# Begin experiment
cat("\n\n##################### >> SIMULACRA << #####################\n\n")

# Init data
subj <- readline(prompt="Subject ID: ")
l1 = vector(mode="list", length=trials)
l2 = vector(mode="list", length=trials)
conditions <- sample( rep(c("lognorm", "norm"), each=n/2) )
d <- data.frame(d=NA, d_obs=NA, cohens_d=NA, response=NA, resp=NA, 
                p=NA, bf=NA, ksp=NA, lognorm=conditions, stringsAsFactors = FALSE)

# Loop over trials
for (trial in 1:trials) {
    # Set difference in population means
    d[trial,"d"] <- rexp(1, rate=1.5)
    # Sample for group 1
    l1[[trial]] = g1 = rnorm(n)
    # Sample for group 2 based on condition
    if(d[trial, "lognorm"]=="lognorm") 
        {g2 = (rlnorm(n, meanlog=0, sdlog=1) + d[trial,"d"])} else 
        {g2 = rnorm(n, mean = d[trial,"d"])}
    l2[[trial]] = g2
    # Observed difference in means
    d[trial,"d_obs"] <- abs(mean(g1) - mean(g2))
    # Compute Cohen's d
    d[trial,"cohens_d"] <- abs(mean(g1) - mean(g2)) /
        sqrt((((n-1)*var(g1) + (n-1)*var(l2[[trial]]))) / (n*2)/2)
    # Compute p-value from indp t-test
    d[trial, "p"] <- t.test(g1, g2)$p.val
    # Default bayes factor
    d[trial, "bf"] <- ttestBF(g1, g2)@bayesFactor$bf
    # Two-sample Kolmogorov-Smirnov test
    d[trial, "ksp"] <- ks.test(g1, g2)$p.value
    # Create plot
    tmp = melt(data.frame(g1, g2), id.vars=NULL)
    print(ggplot(tmp, aes(variable, value)) + 
              geom_point(position = position_jitter(h=0, w=.08), size=3) +
              coord_cartesian(ylim = c(min(tmp$value)-1, max(tmp$value)+1)) +
              scale_x_discrete(labels=c("group1", "group2")) +
              te)
    # Get response
    resp=NA
    while (!(resp %in% c("y", "n", "q"))) {
        resp <- readline(prompt="Signal present? (y/n): ")    
    }
    if (resp=="q") {break}
    d[trial,"response"] <- resp
    d[trial,"resp"] <- ifelse(d[trial,"response"]=="y", 1, 0) # Convert to 1/0
}

# Save subject's data file
write.csv(d, paste(subj, ".csv", sep=""))

# Show output
source("simulacra_output.R")

# Show detailed output
source("simulacra_manual_output.R")
    