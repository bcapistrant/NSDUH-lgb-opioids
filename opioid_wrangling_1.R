#opioid data wrangling

library(tidyverse)
library(magrittr)

# substance use variables: Past Month Cigarette Smoking (IRCIGRC 1), Past Year Marijuana (IRMJRC 1,2), Heavy Episodic Drinking (HVYDRKMON 1), other illicit (ILLEMYR 1), AUD/SUD (DPPYILLALC 1)
# covariates: race/ethnicity (NEWRACE2), education (IREDUHIGHST2), urbanicity (COUTYP4), employment status (IRWRKSTAT), household income (INCOME), health insurance status (IRINSUR4), self-reated health (HEALTH2), marital status (IRMARIT), HH size (IRHHSIZ2), # of children in HH (IRKI17_2)
# 
#Brining in Data
## 2015
load("NSDUH_2015.RData")
nsduh2015<-PUF2015_021518 %>%
  mutate(year=2015) %>%
  select(sexident,
    mrjyr, cocyr, crkyr, heryr, hallucyr, inhalyr, methamyr, pnrnmyr, psychyr, opinmyr,
    ircigrc,irmjrc,hvydrkmon,bngdrkmon,illyr,illemyr,dppyillalc,
    IREDUHIGHST2, COUTYP4,irwrkstat,income,IRINSUR4,HEALTH2,
    irmaritstat,IRHHSIZ2,IRKI17_2, CATAG3,CATAG6,irsex,NEWRACE2,
    QUESTID2,year, ANALWT_C, verep, vestr) %>%
  rename_all(tolower)

#table(PUF2015_021518$COUTYP4)
## 2016
load("NSDUH-2016-DS0001-data-r.RData")
nsduh2016<-PUF2016_101617 %>%
  mutate(year=2016) %>%
  select(sexident,
    mrjyr, cocyr, crkyr, heryr, hallucyr, inhalyr, methamyr, pnrnmyr, psychyr, opinmyr,
    ircigrc,irmjrc,hvydrkmon,bngdrkmon,illyr, illemyr,dppyillalc,
    IREDUHIGHST2,COUTYP4,irwrkstat,income,IRINSUR4,HEALTH2,
    irmarit,IRHHSIZ2,IRKI17_2, CATAG3,CATAG6,irsex,NEWRACE2,
    QUESTID2,year, ANALWT_C, verep, vestr) %>%
  rename_all(tolower)

## 2017
# library(readstata13)
# dat <- read.dta13("NSDUH_2017.DTA")
# nsduh2017<-dat %>%
#   mutate(year=2017) %>%
#   select(sexident, suicthnk,suicplan,suick6_month,CATAG3,CATAG6,irsex,NEWRACE2,QUESTID2,year, ANALWT_C, verep, vestr) %>%     rename_all(tolower)


dat2017 <- read.table(file = 'NSDUH_2017_Tab.tsv', sep = '\t', header = TRUE)
nsduh2017<-dat2017 %>%
  mutate(year=2017) %>%
  select(SEXIDENT,
    MRJYR, COCYR, CRKYR, HERYR, HALLUCYR, INHALYR, METHAMYR, PNRNMYR, PSYCHYR, OPINMYR,
    IRCIGRC,IRMJRC,HVYDRKMON,BNGDRKMON,ILLYR,ILLEMYR,DPPYILLALC,
    IREDUHIGHST2,COUTYP4,IRWRKSTAT,INCOME,IRINSUR4,HEALTH2,
    IRMARIT,IRHHSIZ2,IRKI17_2, CATAG3,CATAG6,IRSEX,NEWRACE2,
    QUESTID2,year, ANALWT_C, VEREP, VESTR) %>% 
  rename_all(tolower) %>%
  mutate(questid2=as.character(questid2))

rm(dat2017,PUF2016_101617,PUF2015_021518)

# Data Wrangling
## making variables
### Sexual Orientation: Categorical (sexorient) and Binary/Dichotomous (LGB_d)
### Mental Health: Major Depressive Episode
### Demographics: Age (age_cat) and Gender/Race/Ethnicity Combined Categories (genrace)
nsduh_merged<-bind_rows(nsduh2015,nsduh2016,nsduh2017) %>%
  filter(!is.na(sexident)) %>%
  
  mutate(sexorient=as.factor(if_else(sexident==1, "Heterosexual",
                                     if_else(sexident==2, "Gay",
                                             if_else(sexident==3, "Bisexual", NA_character_)))),
         lgb_d=as.factor(if_else(sexorient %in% c("Gay", "Bisexual"), "LGB", "Heterosexual")),
         lgb_indicator=as.numeric(if_else(sexorient %in% c("Gay", "Bisexual"), 1, 0)),
         
# substance use variables: Past Month Cigarette Smoking (IRCIGRC 1), Past Year Marijuana (IRMJRC 1,2), Heavy Episodic Drinking (HVYDRKMON 1), other illicit (ILLEMYR 1), AUD/SUD (DPPYILLALC 1)
# covariates: race/ethnicity (NEWRACE2), education (IREDUHIGHST2), urbanicity (COUTYP4), employment status (IRWRKSTAT), household income (INCOME), health insurance status (IRINSUR4), self-reated health (HEALTH2), marital status (IRMARIT), HH size (IRHHSIZ2), # of children in HH (IRKI17_2)
    
         cig_mo_fct=as.factor(if_else(ircigrc!=1, "No",
                                     if_else(ircigrc==1, "Yes", NA_character_))),
         mj_year_fct=as.factor(if_else(irmjrc %in% 3:9, "No",
                                     if_else(irmjrc %in% 1:2, "Yes", NA_character_))),
         hed_mo_fct=as.factor(if_else(hvydrkmon!=1, "No",
                                     if_else(hvydrkmon==1, "Yes", NA_character_))),
         bng_mo_fct=as.factor(if_else(bngdrkmon!=1, "No",
                                     if_else(bngdrkmon==1, "Yes", NA_character_))),
        bng_mo_num=as.numeric(if_else(bngdrkmon!=1, 0,
                                     if_else(bngdrkmon==1, 1, NA_real_))),
        drg_year_fct=as.factor(if_else(illyr==0, "No",
                                     if_else(illyr==1, "Yes", NA_character_))),
        drg_year_num=as.numeric(if_else(illyr==0, 0,
                                     if_else(illyr==1, 1, NA_real_))),
        ill_year_fct=as.factor(if_else(illemyr==0, "No",
                                     if_else(illemyr==1, "Yes", NA_character_))),
         audsud_year_fct=as.factor(if_else(dppyillalc==0, "No",
                                     if_else(dppyillalc==1, "Yes", NA_character_))),
        audsud_year_num=as.numeric(if_else(dppyillalc==0, 0,
                                     if_else(dppyillalc==1, 1, NA_real_))),


        mrj_year_fct=as.factor(if_else(mrjyr==0, "No",
                                     if_else(mrjyr==1, "Yes", NA_character_))),
        mrj_year_num=as.numeric(if_else(mrjyr==0, 0,
                                     if_else(mrjyr==1, 1, NA_real_))),
        coc_year_fct=as.factor(if_else(cocyr==0, "No",
                                     if_else(cocyr==1, "Yes", NA_character_))),
        coc_year_num=as.numeric(if_else(cocyr==0, 0,
                                     if_else(cocyr==1, 1, NA_real_))),
        crk_year_fct=as.factor(if_else(crkyr==0, "No",
                                     if_else(mrjyr==1, "Yes", NA_character_))),
        crk_year_num=as.numeric(if_else(crkyr==0, 0,
                                     if_else(mrjyr==1, 1, NA_real_))),
        her_year_fct=as.factor(if_else(heryr==0, "No",
                                     if_else(heryr==1, "Yes", NA_character_))),
        her_year_num=as.numeric(if_else(heryr==0, 0,
                                     if_else(heryr==1, 1, NA_real_))),
        halluc_year_fct=as.factor(if_else(hallucyr==0, "No",
                                     if_else(hallucyr==1, "Yes", NA_character_))),
        halluc_year_num=as.numeric(if_else(hallucyr==0, 0,
                                     if_else(hallucyr==1, 1, NA_real_))),
        inhal_year_fct=as.factor(if_else(inhalyr==0, "No",
                                     if_else(inhalyr==1, "Yes", NA_character_))),
        inhal_year_num=as.numeric(if_else(inhalyr==0, 0,
                                     if_else(inhalyr==1, 1, NA_real_))),
        metham_year_fct=as.factor(if_else(methamyr==0, "No",
                                     if_else(methamyr==1, "Yes", NA_character_))),
        metham_year_num=as.numeric(if_else(methamyr==0, 0,
                                     if_else(methamyr==1, 1, NA_real_))),
        pnrnm_year_fct=as.factor(if_else(pnrnmyr==0, "No",
                                     if_else(pnrnmyr==1, "Yes", NA_character_))),
        pnrnm_year_num=as.numeric(if_else(pnrnmyr==0, 0,
                                     if_else(pnrnmyr==1, 1, NA_real_))),
        psych_year_fct=as.factor(if_else(psychyr==0, "No",
                                     if_else(psychyr==1, "Yes", NA_character_))),
        psych_year_num=as.numeric(if_else(psychyr==0, 0,
                                     if_else(psychyr==1, 1, NA_real_))),        
        opinm_year_fct=as.factor(if_else(opinmyr==0, "No",
                                     if_else(opinmyr==1, "Yes", NA_character_))),
        opinm_year_num=as.numeric(if_else(opinmyr==0, 0,
                                     if_else(opinmyr==1, 1, NA_real_))), 
    
         age_cat=as.factor(if_else(catag3==2, "18-25",
                                   if_else(catag3==3, "26-34",
                                           if_else(catag3==4, "35-49",
                                                   if_else(catag3==5, "50+",NA_character_))))),
          sex_d=as.factor(if_else(irsex==1, "Male",
                            if_else(irsex==2, "Female",NA_character_
            ))),
    
          lgb_sex_cat=as.factor(if_else(irsex==1 & sexident==1, "Male, Het",
                                if_else(irsex==1 & sexident %in% 2:3, "Male, LGB",
                                if_else(irsex==2 & sexident==1, "Female, Het",
                                if_else(irsex==2 & sexident %in% 2:3, "Female, LGB",NA_character_
            ))))),
    
          lgb_sex_cat2=as.factor(if_else(irsex==1 & sexident==1, "Male, Het",
                                if_else(irsex==1 & sexident==2, "Male, Gay",
                                if_else(irsex==1 & sexident==3, "Male, Bi",
                                if_else(irsex==2 & sexident==1, "Female, Het",
                                if_else(irsex==2 & sexident==2, "Female, Gay",
                                if_else(irsex==2 & sexident==3, "Female, Bi",NA_character_
            ))))))),
         race_cat=as.factor(if_else(newrace2==1, "White",
                            if_else(newrace2==2, "Black",
                            if_else(newrace2==3, "Native Am",
                            if_else(newrace2==4, "Hawaiian",
                            if_else(newrace2==5, "Asian",
                            if_else(newrace2==6, "Multiple",
                            if_else(newrace2==7, "Hispanic",NA_character_)))))))),
         race_6cat=as.factor(if_else(newrace2==1, "White",
                            if_else(newrace2==2, "Black",
                            if_else(newrace2 %in% 3:4, "Native Am/Hawaiian",
                            if_else(newrace2==5, "Asian",
                            if_else(newrace2==6, "Multiple",
                            if_else(newrace2==7, "Hispanic",NA_character_))))))),
         race_4cat=as.factor(if_else(newrace2==1, "White",
                            if_else(newrace2==2, "Black",
                            if_else(newrace2==5, "Asian",
                            if_else(newrace2==7, "Hispanic",NA_character_))))),
  
         pooledwt=analwt_c/3
           )%>%
  select(questid2, 
    sexorient, lgb_d, lgb_indicator, 
    cig_mo_fct,
    mj_year_fct,
    hed_mo_fct,
    bng_mo_fct,bng_mo_num,
    drg_year_fct,drg_year_num,ill_year_fct,
    audsud_year_fct,audsud_year_num, 
    mrj_year_fct,mrj_year_num,
    coc_year_fct,coc_year_num,
    crk_year_fct,crk_year_num,
    her_year_fct,her_year_num,    
    halluc_year_fct,halluc_year_num,
    inhal_year_fct,inhal_year_num,
    metham_year_fct,metham_year_num,
    pnrnm_year_fct,pnrnm_year_num,
    psych_year_fct,psych_year_num,
    opinm_year_fct,opinm_year_num,
    age_cat,sex_d, lgb_sex_cat, lgb_sex_cat2, 
    race_cat,race_6cat,race_4cat,
    year, pooledwt,analwt_c, verep, vestr)

opioid_data<-nsduh_merged %>%
  filter(!is.na(lgb_d), !is.na(sex_d), !is.na(opinm_year_fct), !is.na(age_cat))

opioid_nrow<-nrow(opioid_data)