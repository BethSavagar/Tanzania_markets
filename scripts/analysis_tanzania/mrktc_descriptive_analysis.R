# mrktc analysis -- 
library(readr)
library(tidyverse)

# load tanzania data for market c general info and activities surveys
mrktc_generalinfo <- read_csv("tanzania_data/mrktc_generalinfo_tanzania.csv")
lkpmrkt_type <- read_csv("ecopprmarketscsvs/mrktc_lkpmrkt_type.csv")

colnames(mrktc_generalinfo)

# Market Details: name, location

mrktc_details <- mrktc_generalinfo %>%
  select("fid.see.mrktc.idtable.",
         "country.see.mrkta.lkpcountry.",
         "district.region.see.mrkta.lkpregion.",
         "ward.see.mrkta.lkpward.",
         "village",
         "name.of.market",
         "type.of.market.see.mrktc.lkpmrkt.type."
         )

# clean text variables

mrktc_details <- mrktc_details %>%
  mutate(
    "name.of.market" = tolower(`name.of.market`), # lowercase
    "name.of.market" = gsub("[[:punct:]]", " ", `name.of.market`), # remove punctuation
    "name.of.market" = trimws(`name.of.market`), #trim leading and trailing spaces
    "name.of.market" = gsub("\\s+", ".", `name.of.market`)# replace 1+ spaces with "."
  )  %>% 
  mutate(
    "village" = tolower(`village`), # lowercase
    "village" = gsub("[[:punct:]]", " ", `village`), # remove punctuation
    "village" = trimws(`village`),
    "village" = gsub("\\s+", ".", `village`)
  )
  


mrktc_details %>% group_by(name.of.market) %>% count() %>% View()
mrktc_details %>% group_by(village) %>% count() %>% View()

# number of markets in each district / ward / village

mrktc_details %>% 
  group_by(village,name.of.market) %>% 
  count() %>% 
  View() %>% 
  group_by(village) %>% 
  count() %>% 
  View()


## TYPE OF MARKET ##

mrktc_mrkttype <- mrktc_generalinfo %>%
  select(name.of.market, 
         "market.type.code" = type.of.market.see.mrktc.lkpmrkt.type.) %>%
  left_join(lkpmrkt_type %>% select(Code, "market.type" = Description),
            by = c("market.type.code" = "Code"))

mrktc_mrkttype %>% group_by(market.type) %>% count() %>% View()

ggplot(mrktc_mrkttype, aes(market.type))+
  geom_bar(stat = "count")


## MARKET INTERACTIONS ##
# have you visited other markets in last 1 month?

mrktc_mrktvisits <- mrktc_generalinfo %>%
  select("other.market.visited"= "13.have.you.visited.other.markets.to.sell.or.buy.sheep.and.goats.in.the.past.one.month.",
  "number.markets.visited" ="number.of.other.markets.you.have.visited.to.sell.or.buy.sheep.and.goats.in.the.past.one.month",
  "trader" = "researcher.is.this.person.a.trader.meaning.they.buy.and.sell.animals.as.a.business.") %>%
  mutate(number.markets.visited = ifelse(`other.market.visited`==0, 0,`number.markets.visited`),
         # trader = ifelse(trader==1,"yes", ifelse(trader==0,"no",NA))
         )

ggplot(mrktc_mrktvisits, aes(factor(number.markets.visited), group = trader, fill= factor(trader)))+
  geom_bar(stat="count", position = "dodge")+
  labs(x="number of other markets visited",
       fill = "trader")+
  theme_bw(base_size = 14)

