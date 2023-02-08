# Script to clean and subset mrktb data from EcoPPR dataframes to only Tanzania data 
# data sourced from /Users/bethsavagar/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/PPR Collaborations/Data Bank/EcoPPR-1/ecopprmarketscsvs

library(readr)
library(tidyverse)

# key informant interview data 1
mrktc_generalinfo <- read_csv("~/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/PPR Collaborations/Data Bank/ecoppr_data/data_raw/ecopprmarketscsvs/mrktc_generalinfo.csv")

# country lookup
mrktc_lkpcountry <- read_csv("~/OneDrive - Royal Veterinary College/PPR Collaborations/Data Bank/ecoppr_data/data_raw/ecopprmarketscsvs/mrktc_lkpcountry.csv")

## Clean colnames of mrkta data:
colnames(mrktc_generalinfo)

mrktc_1_clean <- mrktc_generalinfo %>% 
  rename_with(tolower) %>% # all lowercase
  rename_with(~gsub("[[:punct:]]", " ", .x)) %>% # replace all punctuation with space " " 
  rename_with(~gsub("\\s+", ".", .x)) # replace any space with single "."
  # rename specific columns...
  # rename(
  #   "region.see.mrkta.lkpregion." ="région.province.commune.see.mrkta.lkpregion.",
  #   "department.ward.see.mrkta.lkpward." ="département.cercle.see.mrkta.lkpward.",
  #   "immature.sheep.less.1."="immature.sheep.1.year.",
  #   "adult.sheep.more.1."="adult.sheep.1.year.",
  #   "immature.goats.less.1."="immature.goats.1.year.",
  #   "adult.goats.more.1."="adult.goats.1.year.",
  #   "3.sick.animals.obs"="3.did.you.observe.any.sick.animals.in.the.market.give.details.of.numbers.and.clinical.signs.",
  #   "4.if.yes.number.sick.by.symptom"="4.if.yes.give.the.number.of.animals.affected.by.each.clinical.syndrome.sign"
  # )

# Filter data to Tanzania

# Tanzania country code from lookup
tanzania_id <- mrktc_lkpcountry %>%
  filter(Description == "Tanzania") %>%
  pull(Code)

# filter mrkta_1 to tanzania only
mrktc_1_tanzania <- mrktc_1_clean %>% 
  filter(country.see.mrkta.lkpcountry. == tanzania_id)

#write.csv(mrktc_1_tanzania, "tanzania_data/mrktc_generalinfo_tanzania.csv", row.names = F)

