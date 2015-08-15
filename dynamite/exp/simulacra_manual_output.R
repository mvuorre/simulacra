
library(gridExtra)

# Fit models and generate curves
m1 <- glm(resp ~ cohens_d * condition, data=d, family='binomial')
m2 <- glm(resp ~ p * condition, data=d, family='binomial')
m3 <- glm(resp ~ bf * condition, data=d, family='binomial')
m4 <- glm(resp ~ ksp * condition, data=d, family='binomial')
new1 <- data.frame(expand.grid(cohens_d = seq(min(d$cohens_d), 
                                              max(d$cohens_d), length=64), 
                               condition = unique(d$condition)))
new2 <- data.frame(expand.grid(p = seq(min(d$p), max(d$p), length=64), 
                               condition = unique(d$condition)))
new3 <- data.frame(expand.grid(bf = seq(min(d$bf), max(d$bf), length=64), 
                               condition = unique(d$condition)))
new4 <- data.frame(expand.grid(ksp = seq(min(d$ksp), max(d$ksp), length=64), 
                               condition = unique(d$condition)))

new1$fit_d <- predict(m1, type="response", newdata=new1)
new2$fit_p <- predict(m2, type="response", newdata=new2)
new3$fit_bf <- predict(m3, type="response", newdata=new3)
new4$fit_ksp <- predict(m4, type="response", newdata=new4)

m1_pse <- data.frame(
    condition = sort(unique(d$condition)), 
    pse = c( -coef(m1)[1]/coef(m1)[2], 
             -(coef(m1)[1] + coef(m1)[3]) / ( coef(m1)[2] + coef(m1)[4]) ) )

m2_pse <- data.frame(
    condition = sort(unique(d$condition)), 
    pse = c( -coef(m2)[1]/coef(m2)[3], 
             -(coef(m2)[1] + coef(m2)[3]) / ( coef(m2)[2] + coef(m2)[4]) ) )

m3_pse <- data.frame(
    condition = sort(unique(d$condition)), 
    pse = c( -coef(m3)[1]/coef(m3)[3], 
             -(coef(m3)[1] + coef(m3)[3]) / ( coef(m3)[2] + coef(m3)[4]) ) )

m4_pse <- data.frame(
    condition = sort(unique(d$condition)), 
    pse = c( -coef(m4)[1]/coef(m4)[3], 
             -(coef(m4)[1] + coef(m4)[3]) / ( coef(m4)[2] + coef(m4)[4]) ) )

# Plot subject's responses
#m1_x <- sort(c(seq(0, max(d$cohens_d), length=3), m1_pse))
#m2_x <- sort(seq(m2_pse, max(d$p), length=4))
#m3_x <- sort(seq(m3_pse, max(d$bf), length=4))
#m4_x <- sort(seq(m4_pse, max(d$ksp), length=4))
# Cohen's d plot
m1_plot <- ggplot(new1, aes(x=cohens_d, y=fit_d, col=condition)) + 
    geom_vline(data=m1_pse, aes(xintercept=pse, col=condition), lty=2, size=1.2) +
    geom_hline(y=.5, col="red", lty=3, size=.7) +
    geom_point(data=d,aes(y=resp), shape=1, size=4) +
    geom_point(data=d,aes(y=resp), alpha=.4, size=4) +
    geom_line(size=1.2) +
    #scale_x_continuous(breaks=m1_x) +
    labs(x="Cohen's d", y="P(signal)", 
         title="Cohen's d")
# P value plot
m2_plot <- ggplot(new2, aes(x=p, y=fit_p, col=condition)) + 
    geom_vline(data=m2_pse, aes(xintercept=pse, col=condition), lty=2, size=1.2) +
    geom_hline(y=.5, col="red", lty=3, size=.7) +
    geom_point(data=d,aes(y=resp), shape=1, size=4) +
    geom_point(data=d,aes(y=resp), alpha=.4, size=4) +
    geom_line(size=1.2) +
    #scale_x_continuous(breaks=m2_x) +
    coord_cartesian(xlim=c(0, .2)) +
    labs(x="p-value", y="P(signal)", 
         title="t-test")
# Bayes factor plot
m3_plot <- ggplot(new3, aes(x=bf, y=fit_bf, col=condition)) + 
    geom_vline(data=m3_pse, aes(xintercept=pse, col=condition), lty=2, size=1.2) +
    geom_hline(y=.5, col="red", lty=3, size=.7) +
    geom_point(data=d,aes(y=resp), shape=1, size=4) +
    geom_point(data=d,aes(y=resp), alpha=.4, size=4) +
    geom_line(size=1.2) +
    #scale_x_continuous(breaks=c(m3_x)) +
    #coord_cartesian(xlim=c(0,20)) +
    labs(x="logBF", y="P(signal)", 
         title="Bayes factor")
# Kolmogorov-Smirnov p-value plot
m4_plot <- ggplot(new4, aes(x=ksp, y=fit_ksp, col=condition)) + 
    geom_vline(data=m4_pse, aes(xintercept=pse, col=condition), lty=2, size=1.2) +
    geom_hline(y=.5, col="red", lty=3, size=.7) +
    geom_point(data=d,aes(y=resp), shape=1, size=4) +
    geom_point(data=d,aes(y=resp), alpha=.4, size=4) +
    geom_line(size=1.2) +
    #scale_x_continuous(breaks=m4_x) +
    labs(x="K-S p-value", y="P(signal)", 
         title="Kolmogorov-Smirnov test")

grid.arrange(ncol=2, m1_plot, m2_plot,
             m3_plot, m4_plot)