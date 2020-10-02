rca <- read_csv("data/US_RCA_ST202009112223_v1.csv",
  col_types = cols(
    Year = col_double(),
    SitcRev3Product = col_character(),
    `SitcRev3Product Label` = col_character(),
    Series = col_double(),
    `Series Label` = col_character(),
    Economy = col_character(),
    `Economy Label` = col_character(),
    Index = col_double(),
    `Index Footnote` = col_character()
  )
) %>%
  rename(
    year = Year,
    sit3 = SitcRev3Product,
    series = Series,
    sit3_lab = `SitcRev3Product Label`,
    series_label = `Series Label`,
    country_code = Economy,
    economy_label = `Economy Label`,
    index = Index,
    index_f = `Index Footnote`
  ) %>%
  select(-sit3_lab, -series_label, -country_code, -index_f) %>%
  group_by(economy_label, year) %>%
  summarise(ivcr = mean(index, na.rm = T), .groups = "drop") %>%
  ungroup()


tot <- read_csv("data/US_TermsOfTrade_ST202007011402_v1.csv",
  col_types = cols(
    Year = col_double(),
    Economy = col_character(),
    `Economy Label` = col_character(),
    TradeIndex = col_double(),
    `TradeIndex Label` = col_character(),
    `Index Base 2000` = col_double(),
    `Index Base 2000 Footnote` = col_logical(),
    `Index Base 2015` = col_double(),
    `Index Base 2015 Footnote` = col_logical(),
    `Index Base 2010` = col_double(),
    `Index Base 2010 Footnote` = col_logical()
  )
) %>% 
  rename(
    year = Year,
    country_code = Economy,
    economy_label = `Economy Label`,
    trade_index = TradeIndex,
    trade_index_label = `TradeIndex Label`,
    index2000 = `Index Base 2000`,
    index2000f = `Index Base 2000 Footnote`,
    index2015 = `Index Base 2015`,
    index2015f = `Index Base 2015 Footnote`,
    index2010 = `Index Base 2010`,
    index2010f = `Index Base 2010 Footnote`
  ) %>% 
  select(-ends_with("f"), -trade_index,
         -index2000, -index2010, -country_code) %>% 
  pivot_wider(names_from = "trade_index_label",
              values_from = "index2015")

