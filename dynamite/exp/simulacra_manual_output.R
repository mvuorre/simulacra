
library(gridExtra)

# Fit models and generate curves
m1 <- glm(resp ~ cohens_d, data=d, family='binomial')
m2 <- glm(resp ~ p, data=d, family='binomial')
m3 <- glm(resp ~ bf, data=d, family='binomial')
m4 <- glm(resp ~ ksp, data=d, family='binomial')
new1 <- data.frame(cohens_d = seq(min(d$cohens_d), 
                                  max(d$cohens_d), length=64))
new2 <- data.frame(p = seq(min(d$p), max(d$p), length=64))
new3 <- data.frame(bf = seq(min(d$bf), max(d$bf), length=64))
new4 <- data.frame(ksp = seq(min(d$ksp), max(d$ksp), length=64))
new1$fit_d <- predict(m1, type="response", newdata=new1)
new2$fit_p <- predict(m2, type="response", newdata=new2)
new3$fit_bf <- predict(m3, type="response", newdata=new3)
new4$fit_ksp <- predict(m4, type="response", newdata=new4)
m1_pse <- as.numeric(-coef(m1)[1]/coef(m1)[2])
m2_pse <- as.numeric(-coef(m2)[1]/coef(m2)[2])
m3_pse <- as.numeric(-coef(m3)[1]/coef(m3)[2])
m4_pse <- as.numeric(-coef(m4)[1]/coef(m4)[2])

# Plot subject's responses
m1_x <- sort(c(seq(0, max(d$cohens_d), length=3), m1_pse))
m2_x <- sort(seq(m2_pse, max(d$p), length=4))
m3_x <- sort(seq(m3_pse, max(d$bf), length=4))
m4_x <- sort(seq(m4_pse, max(d$ksp), length=4))
# Cohen's d plot
m1_plot <- ggplot(new1, aes(x=cohens_d, y=fit_d)) + 
    geom_vline(x=m1_pse, col="red", lty=2, size=1.2) +
    geom_hline(y=.5, col="red", lty=3, size=.7) +
    geom_point(data=d,aes(y=resp), shape=1, size=4) +
    geom_point(data=d,aes(y=resp), alpha=.4, size=4) +
    geom_line(size=1.2) +
    scale_x_continuous(breaks=m1_x) +
    labs(x="Cohen's d", y="P(signal)", 
         title="Cohen's d")
# P value plot
m2_plot <- ggplot(new2, aes(x=p, y=fit_p)) + 
    geom_vline(x=m2_pse, col="red", lty=2, size=1.2) +
    geom_hline(y=.5, col="red", lty=3, size=.7) +
    geom_point(data=d,aes(y=resp), shape=1, size=4) +
    geom_point(data=d,aes(y=resp), alpha=.4, size=4) +
    geom_line(size=1.2) +
    scale_x_continuous(breaks=m2_x) +
    coord_cartesian(xlim=c(0, .2)) +
    labs(x="p-value", y="P(signal)", 
         title="t-test")
# Bayes factor plot
m3_plot <- ggplot(new3, aes(x=bf, y=fit_bf)) + 
    geom_vline(x=m3_pse, col="red", lty=2, size=1.2) +
    geom_hline(y=.5, col="red", lty=3, size=.7) +
    geom_point(data=d,aes(y=resp), shape=1, size=4) +
    geom_point(data=d,aes(y=resp), alpha=.4, size=4) +
    geom_line(size=1.2) +
    scale_x_continuous(breaks=c(m3_x)) +
    #coord_cartesian(xlim=c(0,20)) +
    labs(x="logBF", y="P(signal)", 
         title="Bayes factor")
# Kolmogorov-Smirnov p-value plot
m4_plot <- ggplot(new4, aes(x=ksp, y=fit_ksp)) + 
    geom_vline(x=m4_pse, col="red", lty=2, size=1.2) +
    geom_hline(y=.5, col="red", lty=3, size=.7) +
    geom_point(data=d,aes(y=resp), shape=1, size=4) +
    geom_point(data=d,aes(y=resp), alpha=.4, size=4) +
    geom_line(size=1.2) +
    scale_x_continuous(breaks=m4_x) +
    labs(x="K-S p-value", y="P(signal)", 
         title="Kolmogorov-Smirnov test")

grid.arrange(ncol=2, m1_plot, m2_plot,
             m3_plot, m4_plot)