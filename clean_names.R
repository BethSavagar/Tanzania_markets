# clean market and village names by crossreferencing market survey A (correct) and market survey C (for cleaning)

# load market survey A data:

mrkta_mrktname <- read.csv("scripts/analysis_tanzania/mrkta_mrktname_clean.csv", row.names = F)
mrkta_vilname <- read.csv("scripts/analysis_tanzania/mrkta_vilname_clean.csv", row.names = F)
mrkta_vilmrktname <- read.csv("scripts/analysis_tanzania/mrkta_vilmrktname_clean.csv", row.names = F)

mrktc_mrktname <- read.csv("scripts/analysis_tanzania/mrktc_mrktname_clean.csv", row.names = F)
mrktc_vilname <- read.csv("scripts/analysis_tanzania/mrktc_vilname_clean.csv", row.names = F)
mrktc_vilmrktname <- read.csv("scripts/analysis_tanzania/mrktc_vilmrktname_clean.csv", row.names = F)