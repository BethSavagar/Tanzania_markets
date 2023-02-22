mrktc_generalinfo <- read_csv("tanzania_data/mrktc_generalinfo_tanzania.csv")

mrktc_gps <- mrktc_generalinfo %>%
  select(
    "fid.see.mrktc.idtable.",
    "country.see.mrkta.lkpcountry.",
    "district.region.see.mrkta.lkpregion.",
    "ward.see.mrkta.lkpward.",
    "village",
    "name.of.market",
    "gps.coordinates.of.the.market",
    "unique.row.identifier.uuid."
  )

# write.csv(mrktc_gps, file = "tanzania_data/mrktc_gps_tanzania.csv", row.names = F)
