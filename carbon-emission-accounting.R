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



fuel_data_harvest.grp <- group_by(fuel_data_harvest, state, lga, hhid)  
fuel_data_harvest.sum <- summarize_at(fuel_data_harvest.grp,c("CHARCOAL", "DIESEL", "ELECTRICITY",
                                                               "KEROSENE","GAS", "PETROL"), .funs = sum)

### estimate co2 emission for petrol at household level
petrol_data_harvet.CO2emission <- fuel_data_harvest.sum %>%
  select(1:3, PETROL) %>%
  mutate(petrol.total_expend = PETROL, price_per_liter = 87, total_liter_consumed = (petrol.total_expend/price_per_liter),
         GHG_emission_factor.CO2 = 2.31, total.CO2_produced.kg = (total_liter_consumed * GHG_emission_factor.CO2))

### estimate co2 emission for kerosene at household level
kerosene_data_harvest.CO2emission <- fuel_data_harvest.sum%>%
  select(1:3, KEROSENE) %>%
  mutate(kerosene.total_expend = KEROSENE, price_per_liter = 50, total_liter_consumed = (kerosene.total_expend/price_per_liter),
         GHG_emission_factor.CO2 = 2.5, total.CO2_produced.kg = (total_liter_consumed * GHG_emission_factor.CO2))

### estimate co2 emission for LPG at household level
lpgas_data_harvest.CO2emission <- fuel_data_harvest.sum %>%
  select(1:3, GAS) %>%
  mutate(gas.total_expend = GAS, price_per_liter = 368.396, total.kg_consumed = (gas.total_expend/price_per_liter), kg_to_liter_conversion = 1.96, 
         total_liter_consumed = (total.kg_consumed * kg_to_liter_conversion), GHG_emission_factor.CO2 = 1.51,
          total.CO2_produced.kg = (total_liter_consumed * GHG_emission_factor.CO2))


### estimate co2 emission for electricity at household level
electricity_data_harvest.CO2emission <- fuel_data_harvest.sum %>%
  select(1:3, ELECTRICITY) %>%
  mutate(electricity_total_expend = ELECTRICITY, price_per_KWh = 29, total.KWh_consumed = (electricity_total_expend/price_per_KWh),
         GHG_emission_factor.CO2 = 0.4034043, total.CO2.consumed = (total.KWh_consumed * GHG_emission_factor.CO2))

## estimate co2 emission for charcoal at household level
charcoal_data_harvest_co2emission <- fuel_data_harvest.sum %>%


View(electricity_data_harvest.CO2emission)
View(lpgas_data_harvest.CO2emission)
View(kerosene_data_harvest.CO2emission)
View(fuel_data_harvest.grp)
View(fuel_data_harvest.mean)
View(fuel_data_harvest)
View(fuel_data_harvest.sum)
View(petrol_data_harvet)

## assign data to new variable
labor_earn_harvest <- sect3_harvestw3_RemoveVariableSaveSpace 

## select required variables and remove NAs
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
