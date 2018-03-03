# install required packages
install.packages("tidyverse")
install.packages("HMDHFDplus")
# load libraries
library(tidyverse) # version 1.0.0
library(HMDHFDplus) # version 1.1.8
# import data direct from human mortality db
country <- getHMDcountries()
exposures <- list()
for (i in 1: length(country)) {
  cnt <- country[i]
  exposures[[cnt]] <- readHMDweb(cnt, "E0per", "username", "password")
}
sr_age <- list()
for (i in 1:length(exposures)) {
  di <- exposures[[i]]
  sr_agei <- di %>%
    transmute(country = names(exposures)[i],
              year = Year, female = Female, male = Male,
              sr_age = Male / Female * 100)
  sr_age[[i]] <- sr_agei
}
sr_age <- bind_rows(sr_age)
# remove optional populations
sr_age1 <- sr_age %>% filter(!country %in% c("DEUTE","DEUTW","GBRCENW","FRACNP","NZL_MA","NZL_NM"))
df_plot <- sr_age1
# font
myfont <- "sans"
# plot
gg <- ggplot(df_plot, aes(year, sr_age, color = country))+
  geom_line(size=0.4)+
  geom_point(size = 0.5, pch = 1)+
  geom_hline(yintercept = 100, color = 'grey50', size = 1, lty = 2)+
  geom_smooth(method = "loess", aes(group = 1), col = "black", level = 0.9, size = 0.5)+
  guides(col = guide_legend(ncol = 5))+
  ylab('Average life expectancy: males / females (%)')+
  theme_minimal(base_family = myfont, base_size = 12)+
  theme(legend.position='right', legend.title = element_blank())+
  theme(axis.line=element_line())+
  theme(axis.text.y = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.title.y = element_text(size = rel(1.3)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(plot.margin = unit(c(0.6,0.4,0.4,0.4), "cm"), legend.justification=c(0,0),
        legend.position=c(0.001,0.001))
gg