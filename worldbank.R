#install.packages("wbstats")
#install.packages("devtools")
#install.packages("wb")
#devtools::install_github("nset-ornl/wbstats")

library(tidyverse)
library(wbstats)
library(readxl)

str(wb_cachelist, max.level=1)

#?wbcache

wbindicators(lang = "en")

new_cache <- wb_cache()


GDP_inds <- wb_search("gdp")

#head(GDP_inds)


gdp_fixed <- wb_data(indicator = GDP_inds$indicator_id[454])

#Filter out the dates
filtered_gdp_fixed <- gdp_fixed %>% 
  filter(date==2022)

filtered_gdp_fixed <- filtered_gdp_fixed %>%
  select(-iso2c,-iso3c,-unit,-obs_status,-footnote,-last_updated)  
  colnames(filtered_gdp_fixed)[1] <- "Country"
  colnames(filtered_gdp_fixed)[3] <- "GDP (PPP)"

  
merging_GDP <- filtered_gdp_fixed %>% 
  select(-date)

merged_data <- full_join(AI_index, merging_GDP, by = "Country")
merged_data_plus_scaled <- full_join(merged_data,agg_sentiment, by = "Country")



#Open the SIPRI data

# Specify the Excel file path
Mil_exp <- read_excel("Mil_exp.xlsx", sheet = "Share of spending 2022")

#cleaning
Mil_exp<- Mil_exp %>% 
  drop_na()

Mil_exp_cleaned <- Mil_exp[!(Mil_exp$`%Spending` %in% c("...", "xxx")),]

merged_data_plus_scaled <- full_join(merged_data, Mil_exp_cleaned, by = "Country")

