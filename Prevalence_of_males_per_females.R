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
  exposures[[cnt]] <- readHMDweb(cnt, "Exposures_1x1", "username", "password")
}
sr_age <- list()
for (i in 1:length(exposures)) {
  di <- exposures[[i]]
  sr_agei <- di %>% select(Year,Age,Female,Male) %>% 
    filter(Year %in% 2013) %>%
    select(-Year) %>%
    transmute(country = names(exposures)[i],
              age = Age, sr_age = Male / Female * 100)
  sr_age[[i]] <- sr_agei
}
sr_age <- bind_rows(sr_age)
# remove optional populations
sr_age1 <- sr_age %>% filter(!country %in% c("DEUTE","DEUTW","GBRCENW","FRACNP","NZL_MA","NZL_NM"))
df_plot <- sr_age1
df_plot <- df_plot[!grepl("NaN", df_plot$sr_age),]
df_plot <- df_plot[!grepl("Inf", df_plot$sr_age),]
# font
myfont <- "sans"
# plot
gg <- ggplot(df_plot, aes(age, sr_age, color = country, group = country))+
  geom_hline(yintercept = 100, color = 'grey50', size = 0.5)+
  geom_smooth(size = 1, method = "loess", se = FALSE)+
  scale_y_continuous(limits = c(40, 120), expand = c(0, 0), breaks = seq(40, 120, 20))+
  scale_x_continuous(limits = c(1, 99), expand = c(0, 0), breaks = seq(0, 100, 20))+
  xlab('Age')+
  ylab('Prevalence of males per 100 females in 2013')+
  facet_wrap(~country, ncol = 6)+
  theme_minimal(base_family = myfont, base_size = 12)+
  theme(legend.position='none',
        panel.border = element_rect(size = 0.5, fill = NA),
        panel.spacing.x=unit(0.4, "cm"),
        panel.spacing.y=unit(0.1, "cm"))+
  theme(axis.title=element_text(size = 14))
gg