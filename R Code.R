---
title: "Irene Hsueh's Research Contribution"
author: "Irene Hsueh"
date: ''
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(naniar)
library(expss)
library(table1)
library(aod)
library(survey)
```

# 2020 Dataset
```{r}
load("C:/Irene Hsueh's Documents/MS Applied Biostatistics/More Research/2020 NSDUH Release/NSDUH_2020.RData")

#Selecting Variables for Analysis
crimes2020 <- NSDUH_2020 %>% 
  dplyr::select(medmj_law = MEDMJPA2, 
                illegal_crimes = snysell, 
                steal = snystole, 
                attack = snyattak, 
                age = CATAG6, 
                sex = irsex, 
                race = NEWRACE2, 
                marital_status = irmarit, 
                education = IREDUHIGHST2, 
                cigarettes = cigever, 
                alcohol = alcever, 
                mj_use = mjever,
                cluster = verep, 
                strata = VESTRQ1Q4_C, 
                weights = ANALWTQ1Q4_C) %>%
#Selecting Participants 18 or Older
  filter(age>=2) %>%
#Converting Missing Data to NA
  naniar::replace_with_na_all(condition= ~.x %in% c(85, 94, 97, 98, 99)) %>% 
#Excluding Observations with NA
  na.omit() %>% 
#Adding Year Variable & Recoding Variables  
  mutate(year=2020, 
         illegal_crimes = recode_factor(illegal_crimes, 
                                       "1"="No", 
                                       "2"="Yes",
                                       "3"="Yes", 
                                       "4"="Yes",
                                       "5"="Yes"),
         steal = recode_factor(steal,
                               "1"="No", 
                               "2"="Yes",
                               "3"="Yes", 
                               "4"="Yes",
                               "5"="Yes"),
         attack = recode_factor(attack,
                                "1"="No",
                                "2"="Yes",
                                "3"="Yes", 
                                "4"="Yes",
                                "5"="Yes"),
         age = recode_factor(age, 
                             "2"="18-25 years", 
                             "3"="26-34 years", 
                             "4"="35-49 years", 
                             "5"="50-64 years", 
                             "6"=">65 years"), 
         sex = recode_factor(sex, "1"="Male", "2"="Female"),
         race = recode_factor(race, 
                              "1"="White", 
                              "2"="Black", 
                              "7"="Hispanic", 
                              "3"="Other", 
                              "4"="Other", 
                              "5"="Other", 
                              "6"="Other"), 
         marital_status = recode_factor(marital_status, 
                                        "1"="Married", 
                                        "2"="Widowed", 
                                        "3"="Divorced/Separated", 
                                        "4"="Never Married"), 
         education = recode_factor(education, 
                                   "1"="High School Not Completed", 
                                   "2"="High School Not Completed", 
                                   "3"="High School Not Completed", 
                                   "4"="High School Not Completed", 
                                   "5"="High School Not Completed", 
                                   "6"="High School Not Completed", 
                                   "7"="High School Not Completed", 
                                   "8"="High School Diploma", 
                                   "9"="Some College", 
                                   "10"="Associate Degree", 
                                   "11"="College Graduate")
         ) %>%
#Reassign Values to Binary Variables
  mutate_at(c("medmj_law", "cigarettes", "alcohol", "mj_use"), 
            list(~dplyr::recode(., "2"="No", .default="Yes"))) %>%
#Factoring Variables
mutate(across(.cols=-weights, .fns=as.factor)) %>%
#Reordering Variables
  select(-"cluster", -"strata", -"weights", everything()) %>%
#Labeling Variables
  apply_labels(illegal_crimes = "Sold Illegal crimes", 
               steal = "Stolen/Tried to Steal Anything Worth >$50", 
               attack = "Attacked Someone With Intent to Seriously Hurt Them",
               medmj_law = "State Medical MJ Law Passed", 
               age = "Age Category", 
               sex = "Sex", 
               race = "Race", 
               marital_status = "Marital Status", 
               education = "Education", 
               cigarettes = "Ever Smoked a Cigarette", 
               alcohol = "Ever Had a Drink of Alcohol", 
               mj_use = "Ever Used Marijuana",
               year = "Year of Interview", 
               cluster = "Primary Sampling Unit", 
               strata = "Strata", 
               weights = "Person-Level Weights")


#Descriptive Statistics
table2020_html <- table1(~illegal_crimes + steal + attack + age + sex + race +  marital_status + education + cigarettes + alcohol + mj_use | medmj_law, data=crimes2020, overall="Total", rowlabelhead="State Medical MJ Law Passed", caption="2020 Dataset")
```



# Weighted 2020 Dataset 
```{r}
crimes2020_weighted <- svydesign(id = ~cluster, 
                                 strata = ~strata, 
                                 weights = ~weights,
                                 data = crimes2020,
                                 nest = TRUE)
crimes2020_nomedmj_weighted <- subset(crimes2020_weighted, medmj_law=="No")
crimes2020_medmj_weighted <- subset(crimes2020_weighted, medmj_law=="Yes")


#Descriptive Statistics
overall_table_weighted <- lapply(names(crimes2020[,-c(13:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=crimes2020_weighted))

overall_prop_weighted <- lapply(names(crimes2020[,-c(13:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=crimes2020_weighted))*100, 2))


#nomedmjlaw_table 
nomedmj_table_weighted <- lapply(names(crimes2020[-c(1, 13:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=crimes2020_nomedmj_weighted))

nomedmj_prop_weighted <- lapply(names(crimes2020[,-c(1, 13:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=crimes2020_nomedmj_weighted))*100, 2))


#medmjlaw_table 
medmj_table_weighted <- lapply(names(crimes2020[-c(1, 13:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=crimes2020_medmj_weighted))

medmj_prop_weighted <- lapply(names(crimes2020[,-c(1, 13:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=crimes2020_medmj_weighted))*100, 2))


#Chi-Squared Tests
chisq_weighted <- lapply(names(crimes2020[-c(1, 13:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = crimes2020_weighted))
pvalues_weighted <- lapply(names(crimes2020[-c(1, 13:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = crimes2020_weighted)$p.value)
```



# Weighted 2020 Dataset Logistic Regression
```{r}
#Sold Illegal Drugs
illegaldrug2020_weighted <- svyglm(medmj_law ~ illegal_crimes + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, design=crimes2020_weighted, family="binomial")

summary(illegaldrug2020_weighted)
round(exp(cbind(OR = coef(illegaldrug2020_weighted), confint(illegaldrug2020_weighted))),4)



#Stolen/Tried to Steal Anything Worth >$50
steal2020_weighted <- svyglm(medmj_law ~ steal + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, design=crimes2020_weighted, family="binomial")

summary(steal2020_weighted)
round(exp(cbind(OR = coef(steal2020_weighted), confint(steal2020_weighted))),4) 



#Important That Friends Share Religious Beliefs 
attack2020_weighted <- svyglm(medmj_law ~ attack + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use, design=crimes2020_weighted, family="binomial")

summary(attack2020_weighted)
round(exp(cbind(OR = coef(attack2020_weighted), confint(attack2020_weighted))),4)
```



# Checking Interaction with Sex in Weighted 2020 Dataset
```{r}
#Sold Illegal Drugs
illegaldrug_sex_weighted <- svyglm(illegal_crimes ~ medmj_law + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + medmj_law:sex, design=crimes2020_weighted, family="binomial")

summary(illegaldrug_sex_weighted)
round(exp(cbind(OR = coef(illegaldrug_sex_weighted), confint(illegaldrug_sex_weighted))),4)



#Stolen/Tried to Steal Anything Worth >$50
steal_sex_weighted <- svyglm(steal ~ medmj_law + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + medmj_law:sex, design=crimes2020_weighted, family="binomial")

summary(steal_sex_weighted)
round(exp(cbind(OR = coef(steal_sex_weighted), confint(steal_sex_weighted))),4)



#Important That Friends Share Religious Beliefs 
attack_sex_weighted <- svyglm(attack ~ medmj_law + age + sex + race + marital_status + education + cigarettes + alcohol + mj_use + medmj_law:sex, design=crimes2020_weighted, family="binomial")

summary(attack_sex_weighted)
round(exp(cbind(OR = coef(attack_sex_weighted), confint(attack_sex_weighted))),4)
```




# Stratifying Weighted 2020 Dataset by Sex
```{r}
#Race-Stratified Datasets
crimes_female_weighted <- subset(crimes2020_weighted, sex=="Female")
crimes_male_weighted <- subset(crimes2020_weighted, sex=="Male")


#Female Descriptive Statistics
female_table_weighted <- lapply(names(crimes2020[,-c(5, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=crimes_female_weighted))
female_prop_weighted <- lapply(names(crimes[,-c(7, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=crimes_female_weighted))*100, 2))

nomedmj_female_table_weighted <- lapply(names(crimes2020[-c(1, 5, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=subset(crimes_female_weighted, medmj_law=="No")))
nomedmj_female_prop_weighted <- lapply(names(crimes2020[-c(1, 5, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=subset(crimes_female_weighted, medmj_law=="No")))*100, 2))

medmj_female_table_weighted <- lapply(names(crimes2020[-c(1, 5, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=subset(crimes_female_weighted, medmj_law=="Yes")))
medmj_female_prop_weighted <- lapply(names(crimes2020[-c(1, 5, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=subset(crimes_female_weighted, medmj_law=="Yes")))*100, 2))

chisq_female_weighted <- lapply(names(crimes2020[-c(1, 5, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = crimes_female_weighted))
pvalues_female_weighted <- lapply(names(crimes2020[-c(1, 5, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = crimes_female_weighted)$p.value)



#Male Descriptive Statistics
male_table_weighted <- lapply(names(crimes2020[,-c(5, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=crimes_male_weighted))
male_prop_weighted <- lapply(names(crimes[,-c(5, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=crimes_male_weighted))*100, 2))

nomedmj_male_table_weighted <- lapply(names(crimes2020[-c(1, 5, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=subset(crimes_male_weighted, medmj_law=="No")))
nomedmj_male_prop_weighted <- lapply(names(crimes2020[-c(1, 5, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=subset(crimes_male_weighted, medmj_law=="No")))*100, 2))

medmj_male_table_weighted <- lapply(names(crimes2020[-c(1, 5, 14:16)]), function(x)
  svytable(bquote(~.(as.name(x))), design=subset(crimes_male_weighted, medmj_law=="Yes")))
medmj_male_prop_weighted <- lapply(names(crimes2020[-c(1, 5, 14:16)]), function(x)
  round(prop.table(svytable(bquote(~.(as.name(x))), design=subset(crimes_male_weighted, medmj_law=="Yes")))*100, 2))

chisq_male_weighted <- lapply(names(crimes2020[-c(1, 5, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = crimes_male_weighted))
pvalues_male_weighted <- lapply(names(crimes[-c(1, 5, 14:16)]), function(x)
  svychisq(as.formula(paste("~", x, " + medmj_law")), design = crimes_male_weighted)$p.value)
```



# Sex-Stratified Weighted 2020 Dataset Analysis
```{r}
#Female Logistic Regressions
illegaldrug_female_weighted <- svyglm(medmj_law ~ illegal_crimes + age + race + marital_status + education + cigarettes + alcohol + mj_use, design=crimes_female_weighted, family="binomial")
summary(illegaldrug_female_weighted)
round(exp(cbind(OR = coef(illegaldrug_female_weighted), confint(illegaldrug_female_weighted))),4)

steal_female_weighted <- svyglm(medmj_law ~ steal + age + race + marital_status + education + cigarettes + alcohol + mj_use, design=crimes_female_weighted, family="binomial")
summary(steal_female_weighted)
round(exp(cbind(OR = coef(steal_female_weighted), confint(steal_female_weighted))),4) 

attack_female_weighted <- svyglm(medmj_law ~ attack + age + race + marital_status + education + cigarettes + alcohol + mj_use, design=crimes_female_weighted, family="binomial")
summary(attack_female_weighted)
round(exp(cbind(OR = coef(attack_female_weighted), confint(attack_female_weighted))),4)



#Male Logistic Regressions
illegaldrug_male_weighted <- svyglm(medmj_law ~ illegal_crimes + age + race + marital_status + education + cigarettes + alcohol + mj_use, design=crimes_male_weighted, family="binomial")
summary(illegaldrug_male_weighted)
round(exp(cbind(OR = coef(illegaldrug_male_weighted), confint(illegaldrug_male_weighted))),4)

steal_male_weighted <- svyglm(medmj_law ~ steal + age + race + marital_status + education + cigarettes + alcohol + mj_use, design=crimes_male_weighted, family="binomial")
summary(steal_male_weighted)
round(exp(cbind(OR = coef(steal_male_weighted), confint(steal_male_weighted))),4)

attack_male_weighted <- svyglm(medmj_law ~ attack + age + race + marital_status + education + cigarettes + alcohol + mj_use, design=crimes_male_weighted, family="binomial")
summary(attack_male_weighted)
round(exp(cbind(OR = coef(attack_male_weighted), confint(attack_male_weighted))),4)
```








