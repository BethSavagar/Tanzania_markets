# Script to clean and subset mrktb data from EcoPPR dataframes to only Tanzania data 
# data sourced from /Users/bethsavagar/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/PPR Collaborations/Data Bank/EcoPPR-1/ecopprmarketscsvs


library(tidyverse)

# key informant interview data 1
mrktb_generalinfo <- read_csv("~/OneDrive - Royal Veterinary College/PPR Collaborations/Data Bank/ecoppr_data/data_raw/ecopprmarketscsvs/mrktb_generalinfo.csv")

# country lookup
mrktb_lkpcountry <- read_csv("~/OneDrive - Royal Veterinary College/PPR Collaborations/Data Bank/ecoppr_data/data_raw/ecopprmarketscsvs/mrkta_lkpcountry.csv")

## Clean colnames of mrkta data:
colnames(mrktb_generalinfo)

mrktb_1_clean <- mrktb_generalinfo %>% 
  rename_with(tolower) %>% # all lowercase
  rename_with(~gsub("[[:punct:]]", " ", .x)) %>% # replace all punctuation with space " " 
  rename_with(~gsub("\\s+", ".", .x)) %>% # replace any space with single "."
  # rename specific columns...
  rename(
    "region.see.mrkta.lkpregion." ="région.province.commune.see.mrkta.lkpregion.",
    "department.ward.see.mrkta.lkpward." ="département.cercle.see.mrkta.lkpward.",
    "immature.sheep.less.1."="immature.sheep.1.year.",
    "adult.sheep.more.1."="adult.sheep.1.year.",
    "immature.goats.less.1."="immature.goats.1.year.",
    "adult.goats.more.1."="adult.goats.1.year.",
    "3.sick.animals.obs"="3.did.you.observe.any.sick.animals.in.the.market.give.details.of.numbers.and.clinical.signs.",
    "4.if.yes.number.sick.by.symptom"="4.if.yes.give.the.number.of.animals.affected.by.each.clinical.syndrome.sign"
  )

# Filter data to Tanzania

# Tanzania country code from lookup
tanzania_id <- mrktb_lkpcountry %>%
  filter(Description == "Tanzania") %>%
  pull(Code)

# filter mrkta_1 to tanzania only
mrktb_1_tanzania <- mrktb_1_clean %>% 
  filter(country.see.mrkta.lkpcountry. == tanzania_id)

write.csv(mrktb_1_tanzania, "tanzania_data/mrktb_generalinfo_tanzania.csv", row.names = F)

