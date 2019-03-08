library(tidyverse)
library(magrittr)

library(survey)
options(survey.lonely.psu = "adjust")
opioid_data<- opioid_data %>% mutate(opioid=opinm_year_num*100)
NSDUH_opioid<- svydesign(id = ~verep, strata = ~vestr, weights = ~pooledwt , data = opioid_data , nest = TRUE )


#---------------------------------------------------------------#
##   Total Use By Sexual Minority (Not Gender or Age Group)
#---------------------------------------------------------------#

samplesizes<-opioid_data %>% 
  group_by(lgb_indicator) %>% 
  count(opinm_year_fct) %>% 
  mutate(group_n=sum(n), pct=(n/group_n))

het_total_n<-prettyNum(as.numeric(samplesizes$group_n[2]), big.mark = ",")
lgb_total_n<-prettyNum(as.numeric(samplesizes$group_n[4]), big.mark = ",")
het_opioid_n<-prettyNum(as.numeric(samplesizes$n[2]), big.mark = ",")
lgb_opioid_n<-prettyNum(as.numeric(samplesizes$n[4]), big.mark = ",")
het_opioid_pct<-round(as.numeric(samplesizes$pct[2]*100), digits=1)
lgb_opioid_pct<-round(as.numeric(samplesizes$pct[4]*100), digits=1)


totpop_n<-svytotal(~lgb_d,NSDUH_opioid, na.rm=TRUE)
totpop_nCI<-(confint(svytotal(~lgb_d,NSDUH_opioid, na.rm=TRUE)))
totpop_ndata<-as.data.frame(cbind(totpop_n,totpop_nCI))
totpop_ndata<-totpop_ndata %>%
  rename(totnLCI=`2.5 %`, totnUCI=`97.5 %`) %>%
  tibble::rownames_to_column() %>%
  mutate(lgb_d=as.factor(if_else(rowname=="lgb_dHeterosexual", "Heterosexual", "LGB"))) %>%
  select(lgb_d,totpop_n,totnLCI,totnUCI)

tot_op_n<-svyby(~opinm_year_num,~lgb_d,NSDUH_opioid,svytotal, na.rm=TRUE)
tot_op_nCI<-(confint(svyby(~opinm_year_num,~lgb_d,NSDUH_opioid,svytotal, na.rm=TRUE)))
tot_op_ndata<-as.data.frame(cbind(tot_op_n,tot_op_nCI))
tot_op_ndata<-tot_op_ndata %>%
  rename(opnLCI=`2.5 %`, opnUCI=`97.5 %`, tot_op_n=opinm_year_num) %>%
  #mutate(lgb_d=as.factor(if_else(rowname=="lgb_dHeterosexual", "Heterosexual", "LGB"))) %>%
  select(lgb_d,tot_op_n,opnLCI,opnUCI)

tot_op_pct<-svyby(~opinm_year_num,~lgb_d,NSDUH_opioid,svymean, na.rm=TRUE)
tot_op_pctCI<-(confint(svyby(~opinm_year_num,~lgb_d,NSDUH_opioid,svymean, na.rm=TRUE)))
tot_op_pctdata<-as.data.frame(cbind(tot_op_pct,tot_op_pctCI))
tot_op_pctdata<-tot_op_pctdata %>%
  mutate(oppctLCI=round(`2.5 %`*100, digits=2), oppctUCI=round(`97.5 %`*100, digits = 2), tot_op_pct=round(opinm_year_num*100, digits=2)) %>%
  #mutate(lgb_d=as.factor(if_else(rowname=="lgb_dHeterosexual", "Heterosexual", "LGB"))) %>%
  select(lgb_d,tot_op_pct,oppctLCI,oppctUCI)

supp_t2_temp<-left_join(totpop_ndata,tot_op_ndata)
supp_t2<-left_join(supp_t2_temp,tot_op_pctdata)

t_het_pct<-round(supp_t2[1,8], digits = 1)
t_lgb_pct<-round(supp_t2[2,8], digits = 1)
t_het_n<-prettyNum(supp_t2[1,5], big.mark = ",")
t_lgb_n<-prettyNum(supp_t2[2,5], big.mark = ",")

opioid_model_tot<-svyglm(opioid~lgb_d,NSDUH_opioid, family=stats::gaussian())
summary(opioid_model_tot)
tot_pd<-round(coef(opioid_model_tot)[2], digits=1)
tot_pd_lci<-round(confint(opioid_model_tot)[2,1], digits=1)
tot_pd_uci<-round(confint(opioid_model_tot)[2,2], digits=1)
tot_pd_p<-ifelse(summary(opioid_model_tot)$coefficients[2,4]<0.001, "<0.001", round(summary(opioid_model_tot)$coefficients[2,4], digits=3))
tot_pd_p2<-ifelse(summary(opioid_model_tot)$coefficients[2,4]<0.001, "***", 
                ifelse(summary(opioid_model_tot)$coefficients[2,4]>=0.001 & summary(opioid_model_tot)$coefficients[2,4]<0.01, "**", 
                ifelse(summary(opioid_model_tot)$coefficients[2,4]>=0.01 & summary(opioid_model_tot)$coefficients[2,4]<0.05, "*", " ")))


#-----------------------------------------------------------------------------#
##   Estimated % and Population Prevalence of Opioid Misuse by Age and Gender
#-----------------------------------------------------------------------------#

lgb_opioid_age_n<-svyby(~opinm_year_num,~lgb_d+age_cat,NSDUH_opioid,svytotal, na.rm=TRUE)
lgb_opioid_age_nCI<-(confint(svyby(~opinm_year_num,~lgb_d+age_cat,NSDUH_opioid,svytotal, na.rm=TRUE)))
lgb_opioid_age_ndata<-as.data.frame(cbind(lgb_opioid_age_n,lgb_opioid_age_nCI))
lgb_opioid_age_ndata<-lgb_opioid_age_ndata %>%
  rename(n=opinm_year_num, nLCI=`2.5 %`, nUCI=`97.5 %`) %>%
  select(lgb_d,age_cat,n,nLCI,nUCI)
#View(lgb_opioid_age_ndata)

n_1825_het<-prettyNum(round(lgb_opioid_age_ndata[1,3], digits = 0), big.mark = ",")
n_1825_lgb<-prettyNum(round(lgb_opioid_age_ndata[2,3], digits = 0), big.mark = ",")
n_2634_het<-prettyNum(round(lgb_opioid_age_ndata[3,3], digits = 0), big.mark = ",")
n_2634_lgb<-prettyNum(round(lgb_opioid_age_ndata[4,3], digits = 0), big.mark = ",")
n_3549_het<-prettyNum(round(lgb_opioid_age_ndata[5,3], digits = 0), big.mark = ",")
n_3549_lgb<-prettyNum(round(lgb_opioid_age_ndata[6,3], digits = 0), big.mark = ",")
n_ge50_het<-prettyNum(round(lgb_opioid_age_ndata[7,3], digits = 0), big.mark = ",")
n_ge50_lgb<-prettyNum(round(lgb_opioid_age_ndata[8,3], digits = 0), big.mark = ",")


lgb_opioid_gender_age_pct<-svyby(~opinm_year_num,~lgb_d+age_cat+sex_d,NSDUH_opioid,svymean, na.rm=TRUE)
lgb_opioid_gender_age_pctCI<-(confint(svyby(~opinm_year_num,~lgb_d+age_cat+sex_d,NSDUH_opioid,svymean, na.rm=TRUE)))
lgb_opioid_gender_age_pctdata<-as.data.frame(cbind(lgb_opioid_gender_age_pct,lgb_opioid_gender_age_pctCI))
lgb_opioid_gender_age_pctdata<-lgb_opioid_gender_age_pctdata %>%
  mutate(pct=opinm_year_num*100, pctLCI=`2.5 %`*100, pctUCI=`97.5 %`*100) %>%
  select(lgb_d,age_cat,sex_d,pct,pctLCI,pctUCI)

lgb_opioid_gender_age_n<-svyby(~opinm_year_num,~lgb_d+age_cat+sex_d,NSDUH_opioid,svytotal, na.rm=TRUE)
lgb_opioid_gender_age_nCI<-(confint(svyby(~opinm_year_num,~lgb_d+age_cat+sex_d,NSDUH_opioid,svytotal, na.rm=TRUE)))
lgb_opioid_gender_age_ndata<-as.data.frame(cbind(lgb_opioid_gender_age_n,lgb_opioid_gender_age_nCI))
lgb_opioid_gender_age_ndata<-lgb_opioid_gender_age_ndata %>%
  rename(n=opinm_year_num, nLCI=`2.5 %`, nUCI=`97.5 %`) %>%
  select(lgb_d,age_cat,sex_d,n,nLCI,nUCI)

prevdata<-left_join(lgb_opioid_gender_age_pctdata,lgb_opioid_gender_age_ndata)

f_1825_het_n<-prettyNum(round(prevdata[1,7], digits = 0), big.mark = ",")
f_1825_lgb_n<-prettyNum(round(prevdata[2,7], digits = 0), big.mark = ",")
f_2634_het_n<-prettyNum(round(prevdata[3,7], digits = 0), big.mark = ",")
f_2634_lgb_n<-prettyNum(round(prevdata[4,7], digits = 0), big.mark = ",")
f_3549_het_n<-prettyNum(round(prevdata[5,7], digits = 0), big.mark = ",")
f_3549_lgb_n<-prettyNum(round(prevdata[6,7], digits = 0), big.mark = ",")
f_ge50_het_n<-prettyNum(round(prevdata[7,7], digits = 0), big.mark = ",")
f_ge50_lgb_n<-prettyNum(round(prevdata[8,7], digits = 0), big.mark = ",")
m_1825_het_n<-prettyNum(round(prevdata[9,7], digits = 0), big.mark = ",")
m_1825_lgb_n<-prettyNum(round(prevdata[10,7], digits = 0), big.mark = ",")
m_2634_het_n<-prettyNum(round(prevdata[11,7], digits = 0), big.mark = ",")
m_2634_lgb_n<-prettyNum(round(prevdata[12,7], digits = 0), big.mark = ",")
m_3549_het_n<-prettyNum(round(prevdata[13,7], digits = 0), big.mark = ",")
m_3549_lgb_n<-prettyNum(round(prevdata[14,7], digits = 0), big.mark = ",")
m_ge50_het_n<-prettyNum(round(prevdata[15,7], digits = 0), big.mark = ",")
m_ge50_lgb_n<-prettyNum(round(prevdata[16,7], digits = 0), big.mark = ",")

f_1825_het_pct<-round(prevdata[1,4], digits = 1)
f_1825_lgb_pct<-round(prevdata[2,4], digits = 1)
f_2634_het_pct<-round(prevdata[3,4], digits = 1)
f_2634_lgb_pct<-round(prevdata[4,4], digits = 1)
f_3549_het_pct<-round(prevdata[5,4], digits = 1)
f_3549_lgb_pct<-round(prevdata[6,4], digits = 1)
f_ge50_het_pct<-round(prevdata[7,4], digits = 1)
f_ge50_lgb_pct<-round(prevdata[8,4], digits = 1)
m_1825_het_pct<-round(prevdata[9,4], digits = 1)
m_1825_lgb_pct<-round(prevdata[10,4], digits = 1)
m_2634_het_pct<-round(prevdata[11,4], digits = 1)
m_2634_lgb_pct<-round(prevdata[12,4], digits = 1)
m_3549_het_pct<-round(prevdata[13,4], digits = 1)
m_3549_lgb_pct<-round(prevdata[14,4], digits = 1)
m_ge50_het_pct<-round(prevdata[15,4], digits = 1)
m_ge50_lgb_pct<-round(prevdata[16,4], digits = 1)


#####-------------------------------------------------------------------######
#####--------------------         MODELS        ------------------------######
#####-------------------------------------------------------------------######
NSDUH_opioid_m1825<- subset(NSDUH_opioid, sex_d=="Male" & age_cat=="18-25")
NSDUH_opioid_m2634<- subset(NSDUH_opioid, sex_d=="Male" & age_cat=="26-34")
NSDUH_opioid_m3549<- subset(NSDUH_opioid, sex_d=="Male" & age_cat=="35-49")
NSDUH_opioid_mge50<- subset(NSDUH_opioid, sex_d=="Male" & age_cat=="50+")

NSDUH_opioid_f1825<- subset(NSDUH_opioid, sex_d=="Female" & age_cat=="18-25")
NSDUH_opioid_f2634<- subset(NSDUH_opioid, sex_d=="Female" & age_cat=="26-34")
NSDUH_opioid_f3549<- subset(NSDUH_opioid, sex_d=="Female" & age_cat=="35-49")
NSDUH_opioid_fge50<- subset(NSDUH_opioid, sex_d=="Female" & age_cat=="50+")

# to check that using svyby gives the same results as using a subset
# m1825pct<-svyby(~opinm_year_num,~lgb_d,NSDUH_opioid_m1825,svymean, na.rm=TRUE)
# m1825pctCI<-(confint(svyby(~opinm_year_num,~lgb_d,NSDUH_opioid_m1825,svymean, na.rm=TRUE)))
# m1825n<-svyby(~opinm_year_num,~lgb_d,NSDUH_opioid_m1825,svytotal, na.rm=TRUE)
# m1825nCI<-(confint(svyby(~opinm_year_num,~lgb_d,NSDUH_opioid_m1825,svytotal, na.rm=TRUE)))
# 
# 
# cbind(m1825pct,m1825pctCI)
# cbind(m1825n,m1825nCI)

opioid_model_f1825<-svyglm(opioid~lgb_d,NSDUH_opioid_f1825, family=stats::gaussian())
summary(opioid_model_f1825)
f1825_pd<-round(coef(opioid_model_f1825)[2], digits=1)
f1825_pd_lci<-round(confint(opioid_model_f1825)[2,1], digits=1)
f1825_pd_uci<-round(confint(opioid_model_f1825)[2,2], digits=1)
f1825_pd_p<-ifelse(summary(opioid_model_f1825)$coefficients[2,4]<0.001, "<0.001", round(summary(opioid_model_f1825)$coefficients[2,4], digits=3))
f1825_pd_p2<-ifelse(summary(opioid_model_f1825)$coefficients[2,4]<0.001, "***", 
                ifelse(summary(opioid_model_f1825)$coefficients[2,4]>=0.001 & summary(opioid_model_f1825)$coefficients[2,4]<0.01, "**", 
                ifelse(summary(opioid_model_f1825)$coefficients[2,4]>=0.01 & summary(opioid_model_f1825)$coefficients[2,4]<0.05, "*", " ")))

f1825_pd

opioid_model_f2634<-svyglm(opioid~lgb_d,NSDUH_opioid_f2634, family=stats::gaussian())
f2634_pd<-round(coef(opioid_model_f2634)[2], digits=1)
f2634_pd_lci<-round(confint(opioid_model_f2634)[2,1], digits=1)
f2634_pd_uci<-round(confint(opioid_model_f2634)[2,2], digits=1)
f2634_pd_p<-ifelse(summary(opioid_model_f2634)$coefficients[2,4]<0.001, "<0.001", round(summary(opioid_model_f2634)$coefficients[2,4], digits=3))
f2634_pd_p2<-ifelse(summary(opioid_model_f2634)$coefficients[2,4]<0.001, "***", 
                ifelse(summary(opioid_model_f2634)$coefficients[2,4]>=0.001 & summary(opioid_model_f2634)$coefficients[2,4]<0.01, "**", 
                ifelse(summary(opioid_model_f2634)$coefficients[2,4]>=0.01 & summary(opioid_model_f2634)$coefficients[2,4]<0.05, "*", " ")))


opioid_model_f3549<-svyglm(opioid~lgb_d,NSDUH_opioid_f3549, family=stats::gaussian())
f3549_pd<-round(coef(opioid_model_f3549)[2], digits=1)
f3549_pd_lci<-round(confint(opioid_model_f3549)[2,1], digits=1)
f3549_pd_uci<-round(confint(opioid_model_f3549)[2,2], digits=1)
f3549_pd_p<-ifelse(summary(opioid_model_f3549)$coefficients[2,4]<0.001, "<0.001", round(summary(opioid_model_f3549)$coefficients[2,4], digits=3))
f3549_pd_p2<-ifelse(summary(opioid_model_f3549)$coefficients[2,4]<0.001, "***", 
                ifelse(summary(opioid_model_f3549)$coefficients[2,4]>=0.001 & summary(opioid_model_f3549)$coefficients[2,4]<0.01, "**", 
                ifelse(summary(opioid_model_f3549)$coefficients[2,4]>=0.01 & summary(opioid_model_f3549)$coefficients[2,4]<0.05, "*", " ")))

opioid_model_fge50<-svyglm(opioid~lgb_d,NSDUH_opioid_fge50, family=stats::gaussian())
fge50_pd<-round(coef(opioid_model_fge50)[2], digits=1)
fge50_pd_lci<-round(confint(opioid_model_fge50)[2,1], digits=1)
fge50_pd_uci<-round(confint(opioid_model_fge50)[2,2], digits=1)
fge50_pd_p<-ifelse(summary(opioid_model_fge50)$coefficients[2,4]<0.001, "<0.001", round(summary(opioid_model_fge50)$coefficients[2,4], digits=3))
fge50_pd_p2<-ifelse(summary(opioid_model_fge50)$coefficients[2,4]<0.001, "***", 
                ifelse(summary(opioid_model_fge50)$coefficients[2,4]>=0.001 & summary(opioid_model_fge50)$coefficients[2,4]<0.01, "**", 
                ifelse(summary(opioid_model_fge50)$coefficients[2,4]>=0.01 & summary(opioid_model_fge50)$coefficients[2,4]<0.05, "*", " ")))

##----------------MALE-------------------------------------------------------##
opioid_model_m1825<-svyglm(opioid~lgb_d,NSDUH_opioid_m1825, family=stats::gaussian())
#summary(opioid_model_m1825)
m1825_pd<-round(coef(opioid_model_m1825)[2], digits=1)
m1825_pd_lci<-round(confint(opioid_model_m1825)[2,1], digits=1)
m1825_pd_uci<-round(confint(opioid_model_m1825)[2,2], digits=1)
m1825_pd_p<-ifelse(summary(opioid_model_m1825)$coefficients[2,4]<0.001, "<0.001", round(summary(opioid_model_m1825)$coefficients[2,4], digits=3))
m1825_pd_p2<-ifelse(summary(opioid_model_m1825)$coefficients[2,4]<0.001, "***", 
                ifelse(summary(opioid_model_m1825)$coefficients[2,4]>=0.001 & summary(opioid_model_m1825)$coefficients[2,4]<0.01, "**", 
                ifelse(summary(opioid_model_m1825)$coefficients[2,4]>=0.01 & summary(opioid_model_m1825)$coefficients[2,4]<0.05, "*", " ")))


opioid_model_m2634<-svyglm(opioid~lgb_d,NSDUH_opioid_m2634, family=stats::gaussian())
m2634_pd<-round(coef(opioid_model_m2634)[2], digits=1)
m2634_pd_lci<-round(confint(opioid_model_m2634)[2,1], digits=1)
m2634_pd_uci<-round(confint(opioid_model_m2634)[2,2], digits=1)
m2634_pd_p<-ifelse(summary(opioid_model_m2634)$coefficients[2,4]<0.001, "<0.001", round(summary(opioid_model_m2634)$coefficients[2,4], digits=3))
m2634_pd_p2<-ifelse(summary(opioid_model_m2634)$coefficients[2,4]<0.001, "***", 
                ifelse(summary(opioid_model_m2634)$coefficients[2,4]>=0.001 & summary(opioid_model_m2634)$coefficients[2,4]<0.01, "**", 
                ifelse(summary(opioid_model_m2634)$coefficients[2,4]>=0.01 & summary(opioid_model_m2634)$coefficients[2,4]<0.05, "*", " ")))


opioid_model_m3549<-svyglm(opioid~lgb_d,NSDUH_opioid_m3549, family=stats::gaussian())
m3549_pd<-round(coef(opioid_model_m3549)[2], digits=1)
m3549_pd_lci<-round(confint(opioid_model_m3549)[2,1], digits=1)
m3549_pd_uci<-round(confint(opioid_model_m3549)[2,2], digits=1)
m3549_pd_p<-ifelse(summary(opioid_model_m3549)$coefficients[2,4]<0.001, "<0.001", round(summary(opioid_model_m3549)$coefficients[2,4], digits=3))
m3549_pd_p2<-ifelse(summary(opioid_model_m3549)$coefficients[2,4]<0.001, "***", 
                ifelse(summary(opioid_model_m3549)$coefficients[2,4]>=0.001 & summary(opioid_model_m3549)$coefficients[2,4]<0.01, "**", 
                ifelse(summary(opioid_model_m3549)$coefficients[2,4]>=0.01 & summary(opioid_model_m3549)$coefficients[2,4]<0.05, "*", " ")))

opioid_model_mge50<-svyglm(opioid~lgb_d,NSDUH_opioid_mge50, family=stats::gaussian())
mge50_pd<-round(coef(opioid_model_mge50)[2], digits=1)
mge50_pd_lci<-round(confint(opioid_model_mge50)[2,1], digits=1)
mge50_pd_uci<-round(confint(opioid_model_mge50)[2,2], digits=1)
mge50_pd_p<-ifelse(summary(opioid_model_mge50)$coefficients[2,4]<0.001, "<0.001", round(summary(opioid_model_mge50)$coefficients[2,4], digits=3))
mge50_pd_p2<-ifelse(summary(opioid_model_mge50)$coefficients[2,4]<0.001, "***", 
                ifelse(summary(opioid_model_mge50)$coefficients[2,4]>=0.001 & summary(opioid_model_mge50)$coefficients[2,4]<0.01, "**", 
                ifelse(summary(opioid_model_mge50)$coefficients[2,4]>=0.01 & summary(opioid_model_mge50)$coefficients[2,4]<0.05, "*", " ")))