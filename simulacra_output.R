
library(dplyr)

print(select(d, -response, -d, -d_obs) %>% 
    melt(id.vars=c("resp", "lognorm")) %>%
    data.frame() %>%
    ggplot(aes(y=resp, x=value)) +
    geom_point() +
    scale_y_continuous(limits=c(0,1)) +    
    geom_smooth(method="glm", family="binomial", se=T, level=.8) +
    facet_wrap(~variable, scales="free"))
