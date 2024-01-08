# Script to clean and subset mrkta data from EcoPPR dataframes to only Tanzania data 
# data sourced from /Users/bethsavagar/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/PPR Collaborations/Data Bank/EcoPPR-1/ecopprmarketscsvs


library(tidyverse)

# key informant interview data 1
mrkta_generalinfo <- read_csv("~/OneDrive - Royal Veterinary College/PPR Collaborations/Data Bank/ecoppr_data/data_raw/ecopprmarketscsvs/mrkta_generalinfo.csv")

# country lookup
mrkta_lkpcountry <- read_csv("~/OneDrive - Royal Veterinary College/PPR Collaborations/Data Bank/ecoppr_data/data_raw/ecopprmarketscsvs/mrkta_lkpcountry.csv")

## Clean colnames of mrkta data:
colnames(mrkta_generalinfo)

mrkta_1_clean <- mrkta_generalinfo %>% 
  rename_with(tolower) %>% # all lowercase
  rename_with(~gsub("[[:punct:]]", " ", .x)) %>% # replace all punctuation with space " " 
  rename_with(~gsub("\\s+", ".", .x)) %>% # replace any space with single "."
  # rename specific columns...
  rename(
    "1.location.sold.from"="1.for.the.sheep.and.goats.being.sold.in.this.market.which.areas.do.they.come.from.",
    "2.location.taken.to."="2.for.the.sheep.and.goats.that.are.bought.in.this.market.which.places.are.they.taken.to.",
    "3a.no.sheep.sold.per.day"="3a.approximately.how.many.sheep.are.sold.each.market.day.",
    "3b.no.goats.sold.per.day"="3b.approximately.how.many.goats.are.sold.each.market.day.",
    "4.seasonal.variation.in.trade"="4.is.there.any.variation.in.the.numbers.or.types.of.sheep.and.goats.being.bought.and.sold.at.different.times.seasons.of.the.year.",
    "4.if.yes.describe"="if.yes.describe.",
    "5.if.yes.describe"="if.yes.can.you.describe.how.this.is.done."
  )

# Filter data to Tanzania

# Tanzania country code from lookup
tanzania_id <- mrkta_lkpcountry %>%
  filter(Description == "Tanzania") %>%
  pull(Code)

# filter mrkta_1 to tanzania only
mrkta_1_tanzania <- mrkta_1_clean %>% 
  filter(country.see.mrkta.lkpcountry. == tanzania_id)

# write.csv(mrkta_1_tanzania, "tanzania_data/mrkta_generalinfo_tanzania.csv", row.names = F)

