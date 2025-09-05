---
title: "Irene Park's Research Contribution"
author: "Irene Park"
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
library(survey)
library(gtsummary)
```



### National Survey on Drug Use and Health 2020 Dataset
```{r}
load("C:/Irene Park's Documents/Academics/MS Applied Biostatistics/More Research/2020 NSDUH Release/NSDUH_2020.RData")


crimes <- NSDUH_2020 %>% 
  # Select Variables for Analysis
  dplyr::select(medmj_law = MEDMJPA2, 
                illegal_drugs = snysell, 
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
  # Select Participants 18 or Older
  dplyr::filter(age>=2) %>%
  # Convert Missing Data to NA
  naniar::replace_with_na_all(condition= ~.x %in% c(85, 94, 97, 98, 99)) %>% 
  # Exclude Observations with NA
  stats::na.omit() %>% 
  # Add Year Variable, Recode and Factor Variables
    dplyr::mutate(illegal_drugs = recode_factor(illegal_drugs, 
                                                "1" = "No", 
                                                "2" = "Yes",
                                                "3" = "Yes", 
                                                "4" = "Yes",
                                                "5" = "Yes"),
                  steal = recode_factor(steal,
                                        "1" = "No", 
                                        "2" = "Yes",
                                        "3" = "Yes", 
                                        "4" = "Yes",
                                        "5" = "Yes"),
                  attack = recode_factor(attack,
                                         "1" = "No",
                                         "2" = "Yes",
                                         "3" = "Yes", 
                                         "4" = "Yes",
                                         "5" = "Yes"),
                  age = dplyr::recode_factor(age, 
                                             "2" = "18-25 years", 
                                             "3" = "26-34 years", 
                                             "4" = "35-49 years", 
                                             "5" = "50-64 years", 
                                             "6" = ">65 years"),
                  sex = dplyr::recode_factor(sex, 
                                             "1" = "Male", 
                                             "2" = "Female"),
                  race = dplyr::recode_factor(race, 
                                              "1" = "White", 
                                              "2" = "Black", 
                                              "7" = "Hispanic", 
                                              "3" = "Other", 
                                              "4" = "Other", 
                                              "5" = "Other", 
                                              "6" = "Other"), 
                  marital_status = dplyr::recode_factor(marital_status, 
                                                        "1" = "Married", 
                                                        "2" = "Widowed", 
                                                        "3" = "Divorced/Separated", 
                                                        "4" = "Never Married"), 
                  education = dplyr::recode_factor(education, 
                                                   "1" = "High School Not Completed", 
                                                   "2" = "High School Not Completed", 
                                                   "3" = "High School Not Completed", 
                                                   "4" = "High School Not Completed", 
                                                   "5" = "High School Not Completed", 
                                                   "6" = "High School Not Completed", 
                                                   "7" = "High School Not Completed", 
                                                   "8" = "High School Diploma", 
                                                   "9" = "Some College", 
                                                   "10" = "Associate Degree", 
                                                   "11" = "College Graduate")) %>%
  # Convert to Binary Yes/No Variables
  dplyr::mutate_at(.vars = c("medmj_law", "cigarettes", "alcohol", "mj_use"), 
                     .funs = ~dplyr::recode_factor(., "2" = "No", "1" = "Yes")) %>%
  # Reorder Variables
  dplyr::select(-cluster, -strata, -weights, everything()) %>%
  # Label Variables
  expss::apply_labels(illegal_drugs = "Sold Illegal Drugs", 
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
                      cluster = "Primary Sampling Unit", 
                      strata = "Strata", 
                      weights = "Person-Level Weights")
```



```{r Weighted Dataset}
# Create Weighted Survey Datasetc
crimes_weighted <- survey::svydesign(id = ~cluster, 
                                     strata = ~strata, 
                                     weights = ~weights,
                                     data = crimes,
                                     nest = TRUE) 



# Table 1 - Demographic Characteristics 
table1 <- crimes_weighted %>%
  gtsummary::tbl_svysummary(
    by = medmj_law, 
    include = c(illegal_drugs, steal, attack, 
                age, sex, race, marital_status, education,
                cigarettes, alcohol, mj_use), 
    type = gtsummary::all_dichotomous() ~ "categorical",
    percent = "column", 
    statistic = gtsummary::all_categorical() ~ "{n_unweighted} ({p}%)", 
    digits = gtsummary::all_categorical() ~ c(0, 2),
    label = list(illegal_drugs = "Sold Illegal Drugs", 
                 steal = "Stolen/Tried to Steal Anything Worth >$50", 
                 attack = "Attacked Someone With Intent to Seriously Hurt Them",
                 age = "Age Group", 
                 sex = "Sex", 
                 race = "Race", 
                 marital_status = "Marital Status", 
                 education = "Education", 
                 cigarettes = "Ever Smoked a Cigarette", 
                 alcohol = "Ever Had a Drink of Alcohol",
                 mj_use = "Ever Used Marijuana")
    ) %>%
  # Add Overall Column
  gtsummary::add_overall(last = TRUE) %>%
  # Calculate p-values
  gtsummary::add_p(pvalue_fun = gtsummary::label_style_pvalue(digits = 3)) %>%
  # Add Headers
  gtsummary::modify_header(
    update = list(
      label = "",
      stat_1 = "**State with No MML** \n 
      N = {prettyNum(n_unweighted, big.mark = ',')} \n
      ({round(p*100, 2)}%)", 
      stat_2 = "**State with MML** \n  
      N = {prettyNum(n_unweighted, big.mark = ',')} \n
      ({round(p*100, 2)}%)", 
      stat_0 = "**Overall** \n 
      N = {prettyNum(n_unweighted, big.mark = ',')}")) %>%
  # Remove Footnote
  gtsummary::remove_footnote_header(columns = gtsummary::everything()) 
```



### Weighted Logistic Regression
```{r}
# Function to Output Weighted Logistic Regression Results for Three Main Predictors
logistic_regression <- function(dataset, outcome){
  output <- dataset %>%
    survey::svyglm(
      stats::as.formula(paste(outcome, "~ medmj_law + age + sex + race + marital_status + 
                              education + cigarettes + alcohol + mj_use")), 
                   design = .,
                   family = "quasibinomial")

  summary <- summary(output)
  
  odds_ratio <- cbind(OR = stats::coef(output), stats::confint(output)) %>%
    exp() %>%
    round(4)
  
  return(list("Summary" = summary, "Odds Ratios" = odds_ratio))
}



# Sold Illegal Drugs
illegal_drugs <- logistic_regression(dataset = crimes_weighted, 
                                     outcome = "illegal_drugs")

# Stolen/Tried to Steal Anything Worth >$50
steal <- logistic_regression(dataset = crimes_weighted, 
                             outcome = "steal")

# Attacked Someone With Intent to Seriously Hurt Them
attack <- logistic_regression(dataset = crimes_weighted, 
                              outcome = "steal")
```
