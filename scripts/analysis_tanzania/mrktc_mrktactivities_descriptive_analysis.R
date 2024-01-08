mrktc_mrktactivities <- read_csv("tanzania_data/mrktc_mrktactivities_tanzania.csv") # load tanzania market activities df
sell_placetype_lkp <- read_csv("ecopprmarketscsvs/mrktc_lkpsell_placetype.csv") # load lookup for sell.placetype
placetype_lkp <- read_csv("ecopprmarketscsvs/mrktc_lkpplacetype.csv") # load lookup for sell.placetype

##################################

total_obs <- nrow(mrktc_mrktactivities)

# Numbers buying and selling per month

# How many times in a month do you buy/sell small ruminants in this market?
buysell_mnth <-  mrktc_mrktactivities %>%
  rename(buysell_mnth = `12.how.many.times.in.a.month.do.you.buy.or.sell.sheep.and.or.goats.in.this.market.`) %>%
  group_by(buysell_mnth) %>%
  count()

ggplot(buysell_mnth, aes(x=factor(buysell_mnth), y=n))+
  geom_col()+
  labs(x="How many times buy/sell at this market per month",
       y="count")+
  theme_bw(base_size = 14)

## SELLING ##

# select only selling variables from mrktactivities dataframe

selling <- mrktc_mrktactivities %>% 
  select("fid.see.mrktc.generalinfo.",
         "unique.row.id.see.mrktc.generalinfo.",
         "unique.row.id",
         "sell.placetype.code"="type.of.place.see.mrktc.lkpsell.placetype.",
         "sell.placetype.specify"="if.other.specify",
         "sell.name.place"="name.of.place.8",
         "sell.ward"="ward.9",
         "sell.district"="district.10")

# join sell.placetype.lookup for placetype variable
selling <- selling %>% 
  left_join(sell_placetype_lkp %>% select("Code","sell.placetype" = "Description"),
             by = c("sell.placetype.code" = "Code"))

ggplot(selling, aes(`sell.placetype`))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  annotate("label", x = 1, y = 1750, label = paste0("total=",total_obs))+
  labs(y="count")+
  theme_bw(base_size=14)

# SELLING: "OTHER" PLACETYPE >>> 
selling %>% group_by(sell.placetype.specify) %>% count() %>% View()

other_placetype_recode <- selling %>% 
  select(sell.placetype.specify) %>%
  mutate("sell.placetype.specify.english" = 
           recode(sell.placetype.specify, 
                  `Amekuja kununua` = "buyer",
                  `Anakuja kununua` = "buyer",
                  `Ananunua mifugo kwa wafugaji njiani kabla ya kuwaleta mnadani` = "He buys livestock from breeders on the way before bringing them to the market",
                  `Hajaleta mbuzi wa kuuza Ila amekuja kununua mbuzi.` = "He has not brought goats to sell, but he has come to buy goats.",
                  `Hapa mnadani` = "Here in the market", 
                  `Hapahapa mnadani` = "There is no market", 
                  `Kwa watu binafsi` = "For individuals",
                  `Kwasasa nimekuja kununua` = "buyer",
                  `Mimi ni mfanyabiashara nakuja tu kununua na kusafirisha` = "I am a businessman, I just come to buy and deliver",
                  `Mnadani Gongoni` = "In the market",
                  `Mtaa wa pambazuko ulioko Ifakara mjini`="Pambazuko Street in Ifakara City",
                  `Nauza mbuzi wa hapahapa`="I'm selling a local goat",
                  `Nawakuta mnadani`="I find them in the market",
                  `Nawatoa m8nada tofauti tofauti` = "different auctions?", 
                  `Ndio nimekuja kununua na kuuza` = "Yes I have come to buy and sell", 
                  `Nimekuja kunuju hapa` = "I came here to visit you", 
                  `Nimekuja kununua nakupeleka kwa walanguzi au wauza supu` = "I have come to buy and I am taking you to traffickers or soup sellers", 
                  `Nimewanunua hapa mnadani` = "bought at market", 
                  `Nimewanunua hapahapa mnadani` = "bought at market", 
                  `Njiani ( kijiji cha mziha wakati anaelekea mnada wa mziha)` = "Mziha village on the way to Mziha market", 
                  `Pikipiki` = "Motorcycle", 
                  `Yeye ni Mswagaji` = "he is a vegetarian")) %>% 
           group_by(sell.placetype.specify.english) %>% 
           count()



selling %>% group_by(name.of.place) %>% count() %>% View()

selling %>% group_by(ward) %>% count() %>% View()

selling %>% group_by(district) %>% count() %>% View()


## SELLING ##


buying <- mrktc_mrktactivities %>% 
  select("fid.see.mrktc.generalinfo.",
         "unique.row.id.see.mrktc.generalinfo.",
         "unique.row.id",
         "buy.placetype.code"="type.of.place.see.mrktc.lkpplacetype.",
         "buy.placetype.specify"="if.other.specify.20",
         "buy.place.name"="name.of.place.21",
         "buy.ward"="ward.22",
         "buy.district"="district.23" 
         )

# join buy.placetype.lookup for placetype variable
buying <- buying %>% 
  left_join(placetype_lkp %>% select("Code","buy.placetype" = "Description"),
             by = c("buy.placetype.code" = "Code"))

ggplot(buying, aes(`buy.placetype`))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  annotate("label", x = 1, y = 1750, label = paste0("total=",total_obs))+
  labs(y="count")+
  theme_bw(base_size=14)


# BUYING "OTHER" PLACETYPE >>> 
#
#
#
#
#####

buying %>% group_by(buy.place.name) %>% count() %>% View()

buying %>% group_by(buy.ward) %>% count() %>% View()

buying %>% group_by(buy.district) %>% count() %>% View()
