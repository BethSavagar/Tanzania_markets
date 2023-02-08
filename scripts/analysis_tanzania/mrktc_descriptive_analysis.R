# mrktc analysis
library(readr)
library(tidyverse)

mrktc_generalinfo <- read_csv("tanzania_data/mrktc_generalinfo_tanzania.csv")
mrktc_mrktactivities <- read_csv("~/Library/CloudStorage/OneDrive-RoyalVeterinaryCollege/PPR Collaborations/Data Bank/ecoppr_data/data_raw/ecopprmarketscsvs/mrktc_mrktactivities.csv")


# subset mrktc activities with tanzania only mrkts

colnames(mrktc_mrktactivities)
fids <- mrktc_generalinfo %>%
  pull(fid.see.mrktc.idtable.)%>%
  as.vector()
activities_tanzania <- mrktc_mrktactivities %>%
  filter(`fid (see mrktc_generalinfo)` %in%  fids)

activities_tanzania %>% 
  group_by(`Name of place...8`) %>%
  count() %>%
  View()
