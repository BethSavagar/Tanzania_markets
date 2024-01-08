# clean market and village names by crossreferencing market survey A (correct) and market survey C (for cleaning)

# load market survey A data:

mrkta_mrktname <- read.csv("scripts/clean_data_raw/mrkta_mrktname_clean.csv")
mrkta_vilname <- read.csv("scripts/clean_data_raw/mrkta_vilname_clean.csv")
mrkta_vilmrktname <- read.csv("scripts/clean_data_raw/mrkta_vilmrktname_clean.csv")

mrktc_mrktname <- read.csv("scripts/clean_data_raw/mrktc_mrktname_clean.csv")
mrktc_vilname <- read.csv("scripts/clean_data_raw/mrktc_vilname_clean.csv")
mrktc_vilmrktname <- read.csv("scripts/clean_data_raw/mrktc_vilmrktname_clean.csv")