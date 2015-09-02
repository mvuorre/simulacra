
library(dplyr)

# Theme for output
tt <- theme_bw(base_size = 16) + 
    theme(axis.title=element_text(size=14),
          axis.title.y=element_text(angle=90))
theme_set(tt)

print(select(d, cohens_d, p, bf, resp, condition) %>% 
    melt(id.vars=c("resp", "condition")) %>%
    data.frame() %>%
    ggplot(aes(y=resp, x=value, col=condition)) +
    geom_point() +
    scale_y_continuous(limits=c(0,1)) +    
    geom_smooth(method="glm", family="binomial", se=T, level=.8) +
    facet_wrap(~variable, scales="free"))
