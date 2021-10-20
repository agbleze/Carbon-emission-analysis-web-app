library(readr)
library(tidyverse)
library(skimr)
library(ggplot2)


sect4c1_plantingw3 <- read_csv("~/Desktop/GDN Conference/PAPER SUBMISSION/DATA/NIG DATASET/NIG_2015/NGA_2015_GHSP-W3_v02_M_CSV/sect4c1_plantingw3.csv")

sect4c2_plantingw3 <- read_csv("~/Desktop/GDN Conference/PAPER SUBMISSION/DATA/NIG DATASET/NIG_2015/NGA_2015_GHSP-W3_v02_M_CSV/sect4c2_plantingw3.csv")

sect3_harvestw3 <- read_csv("~/Desktop/GDN Conference/PAPER SUBMISSION/DATA/NIG DATASET/NIG_2015/NGA_2015_GHSP-W3_v02_M_CSV/sect3_harvestw3.csv")

sect3_plantingw3_someColumnsRemovedToSaveSpace <- read_csv("~/Desktop/GDN Conference/PAPER SUBMISSION/DATA/NIG DATASET/sect3_plantingw3_someColumnsRemovedToSaveSpace.csv")


labour_earnings_planting <- sect3_plantingw3_someColumnsRemovedToSaveSpace

labour_earnings_harvest <- sect3_harvestw3

credit_loan_amount <- sect4c2_plantingw3
credit_access_data <- sect4c1_plantingw3

summary(credit_access_data)
View(credit_access_data)
table(credit_access_data$s4cq1)
table(na.omit(credit_access_data$s4cq1))

skim(credit_loan_amount)

loan_amt <- credit_loan_amount %>%
  dplyr::select(1:6, 15) %>%
  na.omit()
nrow(loan_amt)


### Income at post planting
labour_earnings_planting <- labour_earnings_planting %>%
  select(1:6, 24, 29, 35, 40)


labor_earn_plt.filter <- labour_earnings_planting %>%
  dplyr::select(1:7) %>%
  na.omit() #%>%

labor_earning.group <-   dplyr::group_by(labor_earn_plt.filter, state, lga, hhid)
View(labor_earning.group)

labor_earning_plt.mean <- summarize(labor_earning.group, income_mean = mean(s3q21a))


View(labour_mean_income_plt)
View(labor_earn_plt.filter)

View(labour_earnings_planting.na.rm)
View(labour_earnings_planting)

View(loan_amt)
View(credit_loan_amount)
remove(credict_loan_amount)
summary(loan_amt)
View(labour_earnings_harvest)
