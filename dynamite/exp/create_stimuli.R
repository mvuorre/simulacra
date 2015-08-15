# Create fixed datasets for Simulacra

library(BayesFactor)

# Set experiment options
N <- 80 # Number of sample-pairs to create
n <- 25 # Number of observations in each sample

# Lists to record samples
samples_1 <- vector(mode="list", length=N)
samples_2 <- vector(mode="list", length=N)

# Vector of differences in population means
d_mu <- rexp(N, rate=1.5)
hist(d_mu, xlim=c(0,4), breaks=100)

# Record sample statistics
sample_stats <- data.frame(stimulus = 1:N, d_mu_obs=NA, 
                           d_sd_obs=NA, cohens_d=NA, 
                           p=NA, bf=NA, ksp=NA, resp=NA, 
                           stringsAsFactors = FALSE)

# Save 40 samples from each population to two lists
# And sample statistics to a data.frame
for (t in 1:N) {
    samples_1[[t]] = g1 = rnorm(n) # Sample for group 1
    samples_2[[t]] = g2 = rnorm(n, mean = d_mu[t]) # Sample for group 2
    # Compute output statistics
    sample_stats[t,"d_mu_obs"] <- abs(mean(g1) - mean(g2)) # Observed difference
    sample_stats[t,"d_sd_obs"] <- sd(g1) - sd(g2) # Observed difference in SDs
    sample_stats[t,"cohens_d"] <- abs(mean(g1) - mean(g2)) /
        sqrt((((n-1)*var(g1) + (n-1)*var(g2))) / (n*2)/2)
    sample_stats[t, "p"] <- t.test(g1, g2)$p.val # Welch t-test p
    sample_stats[t, "bf"] <- ttestBF(g1, g2)@bayesFactor$bf # Default BF
    sample_stats[t, "ksp"] <- ks.test(g1, g2)$p.value # Kolmogorov-Smirnov test
}

# Save stimuli to file
save(samples_1, samples_2, sample_stats, file="stimuli.RData")
