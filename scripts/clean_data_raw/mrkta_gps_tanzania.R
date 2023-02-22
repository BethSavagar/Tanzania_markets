mrkta_generalinfo <- read_csv("tanzania_data/mrkta_generalinfo_tanzania.csv")

mrkta_gps <- mrkta_generalinfo %>%
  select(
    "fid.see.mrkta.idtable.",
    "unique.row.id",
    "country.see.mrkta.lkpcountry.",
    "district.region.see.mrkta.lkpregion.",
    "ward.see.mrkta.lkpward.",
    "village",
    "name.of.market",
    "gps.coordinates.of.the.market",
    "unique.row.identifier.uuid."
  )

# write.csv(mrkta_gps, file = "tanzania_data/mrkta_gps_tanzania.csv", row.names = F)
