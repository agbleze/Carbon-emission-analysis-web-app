library(readr)
library(tidyverse)
library(skimr)
library(ggplot2)


sect4c1_plantingw3 <- read_csv("~/Desktop/GDN Conference/PAPER SUBMISSION/DATA/NIG DATASET/NIG_2015/NGA_2015_GHSP-W3_v02_M_CSV/sect4c1_plantingw3.csv")

sect4c2_plantingw3 <- read_csv("~/Desktop/GDN Conference/PAPER SUBMISSION/DATA/NIG DATASET/NIG_2015/NGA_2015_GHSP-W3_v02_M_CSV/sect4c2_plantingw3.csv")

sect3_harvestw3 <- read_csv("~/Desktop/GDN Conference/PAPER SUBMISSION/DATA/NIG DATASET/NIG_2015/NGA_2015_GHSP-W3_v02_M_CSV/sect3_harvestw3.csv")

sect3_plantingw3_someColumnsRemovedToSaveSpace <- read_csv("~/Desktop/GDN Conference/PAPER SUBMISSION/DATA/NIG DATASET/sect3_plantingw3_someColumnsRemovedToSaveSpace.csv")

sect3_harvestw3_RemoveVariableSaveSpace <- read_csv("~/Desktop/GDN Conference/PAPER SUBMISSION/DATA/NIG DATASET/sect3_harvestw3.RemoveVariableSaveSpace.csv")

sect11b_harvestw3 <- read_csv("~/Desktop/GDN Conference/PAPER SUBMISSION/DATA/NIG DATASET/NIG_2015/NGA_2015_GHSP-W3_v02_M_CSV/sect11b_harvestw3.csv")
View(sect11b_harvestw3)
fuel_data_harvest <- sect11b_harvestw3 %>%
  dplyr::filter(item_desc == "KEROSENE" | item_desc == "GAS" | item_desc == "ELECTRICITY" | item_desc
                == "FIREWOOD" | item_desc == "CHARCOAL" | item_desc == "PETROL" | item_desc 
                == "DIESEL") %>%
  na.omit() %>%
  spread(key = item_desc, value = s11bq4) %>%
  replace_na(list("CHARCOAL" = 0, "DIESEL" = 0, "ELECTRICITY" = 0, "FIREWOOD" = 0, "GAS" = 0,
                  "KEROSENE" = 0, "PETROL" = 0))
  


skim(fuel_data_harvest)
  
View(fuel_data_harvest)



labor_earn_harvest <- sect3_harvestw3_RemoveVariableSaveSpace 
labor_earn_harvest.select <- labor_earn_harvest %>%
  select(1:6, s3q21a) %>%
  na.omit()

labor_earn_harvest.grp <- group_by(labor_earn_harvest.select, state, lga, hhid)
labor_mean_income_harvest <- summarize(labor_earn_harvest.grp, income_mean = mean(s3q21a))
View(labor_mean_income_harvest)

View(sect3_harvestw3_RemoveVariableSaveSpace)



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
