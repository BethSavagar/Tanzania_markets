# clean market activities lookups

# tidy up the market activities lookup
mrktc_lkpmrkt_actvty <- mrktc_lkpmrkt_actvty %>%
  mutate(Description2 = ifelse(Code == -66, "other",
                               ifelse(Code == 1, "selling",
                                      ifelse(Code == 2, "middleman.broker",
                                             ifelse(Code == 3, "trader.buy.sell", "buying")))))

# what are the "other" activities: sa ve this as a csv to create a lookup for the mrktactivities.specify. Add breeding and butchery to  the activities lookup
other_activities <- mrktactivities_clean %>%
  select(`1.activities.at.market`,
         `1.specify.activities.at.market`) %>%
  filter(!is.na(`1.specify.activities.at.market`))

# write.csv(other_activities,"tanzania_data/lkpmrkt_acvty_spcfy.csv", row.names = F)
# clean this script and dataframe