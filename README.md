# NSDUH-Analysis-Crimes
I used the NSDUH datasets to analyze the association between a participant's self-reported criminal behaviors and whether their state of residence had medical marijuana laws (MML). Datasets can be found at 
[2018 Dataset](https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2018-nsduh-2018-ds0001), 
[2019 Dataset](https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2019-nsduh-2019-ds0001),
[2020 Dataset](https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2020-nsduh-2020-ds0001).

I combined the 2018 and 2019 waves into one dataset and analyzed the 2020 wave separately because of different data collection procedures in 2020 due to the COVID-19 pandemic. The three outcomes were binary yes/no answer to the questions 1) "Sold illegal drugs" 2) "Stolen/tried to steal anything worth >$50" and 3) "Attacked someone with the intent to seriously hurt them". The main predictor of interest was a binary yes/no variable of whether respondents were living in a state that had a law allowing marijuana use for medical reasons at the time of the interview. I also adjusted for covariates of age category, sex, race, marital status, education level, whether someone had ever smoked, drank, or used marijuana, and year of interview. 

First, I ran three weighted adjusted multivariable logistic regressions in the 2018-2019 combined dataset, none of which showed any significant association between MML and self-reported criminal behaviors. Then, I looked for any interaction between the three predictor variables and covariates. Only interaction with race warranted stratification. Surprisingly, only in the 2020 datset but not the 2018-2019 dataset, those who lived in states with MML had higher odds of all three self-reported criminal behaviors.  

I'd like to thank my advisors, Professor Ching-Ti Liu and Professor Yen-Han Lee. 
Feel free to contact me at irenehsueh49@gmail.com with any questions about my code!
