library(tidyverse)
library(magrittr)

library(survey)
options(survey.lonely.psu = "adjust")
NSDUH_opioid<- svydesign(id = ~verep, strata = ~vestr, weights = ~pooledwt , data = opioid_data , nest = TRUE )

lgb_opioid_gender_age<-svyby(~opinm_year_num,~lgb_d+age_cat+sex_d,NSDUH_opioid,svymean, na.rm=TRUE)
lgb_opioid_gender_ageCI<-(confint(svyby(~opinm_year_num,~lgb_d+age_cat+sex_d,NSDUH_opioid,svymean, na.rm=TRUE)))
lgb_opioid_gender_age_data<-as.data.frame(cbind(lgb_opioid_gender_age,lgb_opioid_gender_ageCI))
lgb_opioid_gender_age_data<-lgb_opioid_gender_age_data %>%
  rename(opioid=opinm_year_num, LCI=`2.5 %`,UCI=`97.5 %`) %>%
  mutate(opioid=opioid*100, LCI=LCI*100, UCI=UCI*100) %>%
  select(lgb_d,age_cat,sex_d,opioid,LCI,UCI)

lgb_opioid_gender_age_data$lgb_d<-factor(lgb_opioid_gender_age_data$lgb_d, levels=c("LGB", "Heterosexual"))

plot_opioid_age_gender<-lgb_opioid_gender_age_data %>%
  #filter(opioidression=="opioid_lifetime") %>%
  ggplot(aes(x=age_cat,y=opioid, fill=lgb_d))+
  facet_wrap(~sex_d)+
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=LCI, ymax=UCI), colour="grey") +
  scale_fill_grey(start = 0.55, end = 0.95) +
  theme_bw()   + 
  #scale_x_discrete(labels=c("MDE_lifetime", "MDE_LastYear","K6_lifetime", "K6_LastYear")) +
  scale_y_continuous(limits=c(0,16)) +
  labs(x="Age", y="Percent Prevalence", fill="Sexual Orientation") +
  theme(plot.caption = element_text(size=rel(0.6)), legend.position = "bottom")
plot_opioid_age_gender
#ggsave("Figure2_byracegender.pdf", dpi=300, width=8, height=10,units="in")

ggsave("opioid_prevalence_weighted_figure.pdf", dpi="print", width=8, height=8, units="in")
ggsave("opioid_prevalence_weighted_figure.tiff")