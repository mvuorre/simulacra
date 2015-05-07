
# If you need to install packages
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("ggthemes")
# install.packages("BayesFactor")
# install.packages("reshape2")

# Load packages
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(BayesFactor)
library(reshape2)

# Customize output
options(digits=2)
theme_set(theme_fivethirtyeight(base_size = 16) + 
              theme(axis.title=element_text(size=14),
                    axis.title.y=element_text(angle=90)))
# Set experiment options
trials=30
n=20

# Begin experiment
cat("\n\n##################### >> SIMULACRA << #####################\n\n")

# Init data
subj <- readline(prompt="Subject ID: ")
l1 = vector(mode="list", length=trials)
l2 = vector(mode="list", length=trials)
d <- data.frame(d=NA, d_obs=NA, cohens_d=NA, response=NA, resp=NA, 
                p=NA, bf=NA, stringsAsFactors = FALSE)

# Loop over trials
for (trial in 1:trials) {
    d[trial,"d"] = rnorm(1) # Set difference in population means
    l1[[trial]] = g1 = rnorm(n) # Sample for group 1
    l2[[trial]] = g2 = rnorm(n, mean = d[trial,"d"]) # Sample for group 2
    d[trial,"d_obs"] <- abs(mean(g1) - mean(g2)) # Observed difference in means
    # Compute Cohen's d
    d[trial,"cohens_d"] <- abs(mean(g1) - mean(g2)) /
        sqrt((((n-1)*var(g1) + (n-1)*var(l2[[trial]]))) / (n*2)/2)
    # Compute p-value from indp t-test
    d[trial, "p"] <- t.test(g1, g2)$p.val
    # Default bayes factor
    d[trial, "bf"] = ttestBF(g1, g2)@bayesFactor$bf
    # Create plot
    tmp = melt(data.frame(g1, g2), id.vars=NULL)
    print(ggplot(tmp, aes(variable, value)) + 
              geom_point(position = position_jitter(h=0, w=.08), size=3) +
              coord_cartesian(ylim = c(-4, 4)) +
              scale_x_discrete(labels=c("group1", "group2")) +
              theme_stata() +
              theme(text = element_text(size=0),
                    axis.text.x = element_text(size=16),
                    axis.ticks.y = element_blank(),
                    panel.grid = element_blank()))
    # Get response
    d[trial,"response"] <- readline(prompt="Signal present? (y/n): ")
    d[trial,"resp"] <- ifelse(d[trial,"response"]=="y", 1, 0) # Convert to 1/0
}

# Save subject's data file
write.csv(d, paste(subj, ".csv", sep=""))

# Fit models and generate curves
m1 <- glm(resp ~ cohens_d, data=d, family='binomial')
m2 <- glm(resp ~ p, data=d, family='binomial')
m3 <- glm(resp ~ bf, data=d, family='binomial')
new1 <- data.frame(cohens_d = seq(min(d$cohens_d), 
                                  max(d$cohens_d), length=64))
new2 <- data.frame(p = seq(min(d$p), max(d$p), length=64))
new3 <- data.frame(bf = seq(min(d$bf), max(d$bf), length=64))
new1$fit_d <- predict(m1, type="response", newdata=new1)
new2$fit_p <- predict(m2, type="response", newdata=new2)
new3$fit_bf <- predict(m3, type="response", newdata=new3)
m1_pse <- as.numeric(coef(m1)[1]/-coef(m1)[2])
m2_pse <- as.numeric(coef(m2)[1]/-coef(m2)[2])
m3_pse <- as.numeric(coef(m3)[1]/-coef(m3)[2])

# Plot subject's responses
m1_x <- sort(c(seq(0, max(d$cohens_d), length=3), m1_pse))
m2_x <- sort(seq(m2_pse, max(d$p), length=4))
m3_x <- sort(seq(m3_pse, max(d$bf), length=4))
# Cohen's d plot
m1_plot <- ggplot(new1, aes(x=cohens_d, y=fit_d)) + 
    geom_vline(x=m1_pse, col="red", lty=2, size=1.2) +
    geom_hline(y=.5, col="red", lty=3, size=.7) +
    geom_point(data=d,aes(y=resp), shape=1, size=4) +
    geom_point(data=d,aes(y=resp), alpha=.4, size=4) +
    geom_line(size=1.2) +
    scale_x_continuous(breaks=m1_x) +
    labs(x="Cohen's d", y="P(signal)", 
         title="Cohen's d") +
    coord_cartesian(xlim=c(0,max(d$d_obs)))
# P value plot
m2_plot <- ggplot(new2, aes(x=p, y=fit_p)) + 
    geom_vline(x=m2_pse, col="red", lty=2, size=1.2) +
    geom_hline(y=.5, col="red", lty=3, size=.7) +
    geom_point(data=d,aes(y=resp), shape=1, size=4) +
    geom_point(data=d,aes(y=resp), alpha=.4, size=4) +
    geom_line(size=1.2) +
    scale_x_continuous(breaks=m2_x) +
    labs(x="p-value", y="P(signal)", 
         title="p-value")
# Bayes factor plot
m3_plot <- ggplot(new3, aes(x=bf, y=fit_bf)) + 
    geom_vline(x=m3_pse, col="red", lty=2, size=1.2) +
    geom_hline(y=.5, col="red", lty=3, size=.7) +
    geom_point(data=d,aes(y=resp), shape=1, size=4) +
    geom_point(data=d,aes(y=resp), alpha=.4, size=4) +
    geom_line(size=1.2) +
    scale_x_continuous(breaks=m3_x) +
    labs(x="bayes factor", y="P(signal)", 
         title="Bayes factor")
# Plot observed differences
p_df <- ggplot(d, aes(x=d_obs)) + 
    stat_bin(geom="histogram", col="white") +
    labs(x="absolute difference", y="",
         title="Differences in means")
grid.arrange(ncol=2, m1_plot, m2_plot,
             m3_plot, p_df)
    