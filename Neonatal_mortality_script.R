# Plot the ratio of male to female neonatal deaths per country within HMD
# load libraries
library(tidyverse) # version 1.0.0
library(HMDHFDplus) # version 1.1.8
library(grid) # version 
library(gridExtra) # version
# import data direct from human mortality db
country <- getHMDcountries()
exposures <- list()
for (i in 1: length(country)) {
  cnt <- country[i]
  exposures[[cnt]] <- readHMDweb(cnt, "Deaths_1x1",
                                 "username", "password")
}
sr_age <- list()
for (i in 1:length(exposures)) {
  di <- exposures[[i]]
  sr_agei <- di %>% select(Year,Age,Female,Male) %>% 
    transmute(country = names(exposures)[i],
              age = Age, year = Year, sr_age = Male / Female * 100)
  sr_age[[i]] <- sr_agei
}
sr_age <- bind_rows(sr_age)
# remove unnecessary populations
sr_age <- sr_age %>% filter(!country %in% c("DEUTE", "DEUTW","GBRCENW", "FRACNP", "NZL_MA", "NZL_NM"))
# subset by age: zero, one year, and two years of age
sr_perinatal <- sr_age %>% subset(age==0)
aged1 <- sr_age %>% subset(age==1)
aged2 <- sr_age %>% subset(age==2)
# Average these to get ratio of male/female
# deaths within the neonatal period
sr_perinatal$sr_age <- (sr_perinatal$sr_age+aged1$sr_age+aged2$sr_age)/3
# summarize all countries and remove NA or Inf (div by zero)
sr_countries <- sr_perinatal$country
df_plot <- sr_perinatal
df_plot <- df_plot[!grepl("NaN", df_plot$sr_age),]
df_plot <- df_plot[!grepl("Inf", df_plot$sr_age),]
# font style (helvetica)
myfont <- "sans"
# plot
gg <- ggplot(df_plot, aes(year, sr_age, color = country, group = country))+
  geom_hline(yintercept = 100, color = 'grey10', size = 0.5, lty=2)+
  geom_point(size = 1, pch = 1)+
  geom_line(size = 0.2)+
  geom_smooth(method = "loess", aes(group = 1), col = "black", level = 0.9, size = 0.5)+
  ylim(50, 350)+
  xlab('Year')+
  ylab('Perinatal mortality ratio: males per 100 females')+
  theme_minimal()+
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  ) +
  theme(axis.line=element_line())+
  theme(axis.text.y = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(size = rel(1.5)))+
  theme(axis.title.y = element_text(size = rel(1.3)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(plot.margin = unit(c(0.6,0.4,0.4,0.4), "cm"), legend.justification=c(0,1),
        legend.position=c(0.001,1.05))+
  scale_colour_discrete(guide = guide_legend(title = NULL), guide_legend(nrow = 15),
                      breaks=c("AUS","AUT","BEL","BGR","BLR","CAN",
                               "CHE","CHL","CZE","DEUTNP","DNK","ESP","EST","FIN",
                               "FRATNP","GBR_NIR","GBR_NP","GBR_SCO","GBRTENW","GRC",
                               "HUN","IRL","ISL","ISR","ITA","JPN","LTU","LUX",
                               "LVA","NLD","NOR","NZL_NP","POL","PRT",
                               "RUS","SVK","SVN","SWE","TWN","UKR","USA"),
                      labels=c("Australia (AUS)","Austria (AUT)","Belgium (BEL)",
                               "Bulgaria (BGR)","Belarus (BLR)","Canada (CAN)",
                               "Switzerland (CHE)","Chile (CHL)","Czech Republic (CZE)",
                               "Germany (DEUTNP)","Denmark (DNK)","Spain (ESP)",
                               "Estonia (EST)","Finland (FIN)","France (FRATNP)",
                               "Northern Ireland (NIR)","Great Britain (GBR_NP)",
                               "Scotland (SCO)","England & Wales (ENW)",
                               "Greece (GRC)","Hungary (HUN)","Ireland (IRL)",
                               "Iceland (ISL)","Israel (ISR)","Italy (ITA)","Japan (JPN)",
                               "Lithuania (LTU)","Luxembourg (LUX)","Latvia (LVA)",
                               "Netherlands (NLD)","Norway (NOR)","New Zealand (NZL_NP)",
                               "Poland (POL)","Portugal (PRT)","Russia (RUS)",
                               "Slovakia (SVK)","Slovenia (SVN)","Sweden (SWE)",
                               "Taiwan (TWN)","Ukraine (UKR)","U.S.A. (USA)"))
gg
#grid.arrange(gg, left = textGrob("B", hjust = -1, vjust = -12, gp=gpar(fontsize=18)))
# save plot to pdf
ggsave(gg, filename = "mortality_summary_all_42_countries_individual.pdf",
      device = "pdf", scale = 1, width = 17, height = 11, 
       units = c("cm"), dpi = 300)
dev.off()