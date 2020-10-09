# IVCR
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
  summarise(values = mean(index, na.rm = T), .groups = "drop") %>%
  ungroup() %>% 
  mutate(variable = "ivcr")

# Terms of Trade
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
    values = `Index Base 2015`,
    index2015f = `Index Base 2015 Footnote`,
    index2010 = `Index Base 2010`,
    index2010f = `Index Base 2010 Footnote`
  ) %>% 
  select(-ends_with("f"), -trade_index,
         -index2000, -index2010, -country_code) %>% 
  mutate(var = "terms_trade") %>% 
  mutate(variable = paste0(var, "_", trade_index_label)) %>% 
  select(-var, -trade_index_label) 
  # pivot_wider(names_from = "trade_index_label",
  #             values_from = "index2015")

# Trade Serv Digital
digital <- read_csv("data/US_TradeServDigital_ST202007241022_v1.csv",
  col_types = cols(
    Year = col_double(),
    Economy = col_character(),
    `Economy Label` = col_character(),
    Partner = col_character(),
    `Partner Label` = col_character(),
    Flow = col_character(),
    `Flow Label` = col_character(),
    Category = col_character(),
    `Category Label` = col_character(),
    `US dollars at current prices in millions` = col_double(),
    `US dollars at current prices in millions Footnote` = col_character(),
    `Growth rate (over previous period)` = col_double(),
    `Growth rate (over previous period) Footnote` = col_character(),
    `Percentage of total world` = col_double(),
    `Percentage of total world Footnote` = col_character(),
    `Percentage of total trade in services` = col_double(),
    `Percentage of total trade in services Footnote` = col_character()
  )
) %>%
  rename(
    year = Year,
    country_code = Economy,
    economy_label = `Economy Label`,
    partner_label = `Partner Label`,
    partner = `Partner`,
    flow_label = `Flow Label`,
    flow = Flow,
    category = Category,
    category_label = `Category Label`,
    us_mill = `US dollars at current prices in millions`,
    us_mill_foot = `US dollars at current prices in millions Footnote`,
    growth_rate = `Growth rate (over previous period)`,
    growth_rate_foot = `Growth rate (over previous period) Footnote`,
    perc_world = `Percentage of total world`,
    perc_world_foot =`Percentage of total world Footnote`,
    perc_services = `Percentage of total trade in services`,
    perc_services_foot = `Percentage of total trade in services Footnote`
  ) %>% 
  mutate(var = "trade_digital_deliverable_services") %>%
  select(-ends_with("foot"), -category, -category_label,
         -partner, -partner_label, -flow) %>% 
  pivot_longer(cols = c("us_mill", "growth_rate",
                        "perc_world", "perc_services"),
               names_to = "vars", values_to = "values") %>% 
  mutate(variable = paste0(flow_label, "_",
                           var, "_",
                           vars
                      )) %>% 
  select(-flow_label, -var, -vars, -country_code) %>% 
  filter(economy_label != "LDCs: Africa and Haiti")

# Goods And Serv Trade Openness
tradeopenness <-
  read_csv("data/US_GoodsAndServTradeOpennessBpm6_ST202009031424_v1.csv",
    col_types = cols(
      Year = col_double(),
      Series = col_double(),
      `Series Label` = col_character(),
      Economy = col_character(),
      `Economy Label` = col_character(),
      Flow = col_character(),
      `Flow Label` = col_character(),
      `US dollars at current prices in millions` = col_double(),
      `US dollars at current prices in millions Footnote` = col_character(),
      `Percentage of Gross Domestic Product` = col_double(),
      `Percentage of Gross Domestic Product Footnote` = col_character()
    )
  )

concent <- read_csv("data/US_ConcentDiversIndices_ST202009111058_v1.csv",
  col_types = cols(
    Year = col_double(),
    Economy = col_character(),
    `Economy Label` = col_character(),
    Partner = col_character(),
    `Partner Label` = col_character(),
    Flow = col_character(),
    `Flow Label` = col_character(),
    `Absolute value` = col_double(),
    `Absolute value Footnote` = col_logical(),
    `Concentration Index` = col_double(),
    `Concentration Index Footnote` = col_character(),
    `Diversification Index` = col_double(),
    `Diversification Index Footnote` = col_character()
  )
) %>% 
  rename(
    year = Year,
    economy = Economy,
    economy_label = `Economy Label`,
    partner = Partner,
    partner_label = `Partner Label`,
    flow = Flow,
    flow_label = `Flow Label`,
    abs_value = `Absolute value`,
    abs_value_foot = `Absolute value Footnote`,
    concentration_index = `Concentration Index`,
    concentration_index_foot = `Concentration Index Footnote`,
    diversificacion_index = `Diversification Index`,
    diversificacion_index_foot = `Diversification Index Footnote`
  ) %>% 
  filter(partner == "0000") %>% 
  select(-ends_with("foot"),  -economy, -flow, -partner,
         -partner_label, -abs_value) %>% 
  pivot_longer(cols = c("concentration_index",
                       "diversificacion_index"),
               names_to = "var",
               values_to = "values") %>% 
  mutate(variable = paste0(flow_label, "_", var)) %>% 
  select(-var, -flow_label)

# tariff data 
tf <- read_csv("data/US_Tariff_ST201910091711_v2.csv",
  col_types = cols(
    Year = col_double(),
    DutyType = col_double(),
    `DutyType Label` = col_character(),
    Market = col_double(),
    `Market Label` = col_character(),
    Origin = col_character(),
    `Origin Label` = col_character(),
    ProductCategory = col_character(),
    `ProductCategory Label` = col_character(),
    TariffIndicator = col_character(),
    `TariffIndicator Label` = col_character(),
    `Absolute value` = col_double(),
    `Absolute value Footnote` = col_logical(),
    Rate = col_double(),
    `Rate Footnote` = col_logical()
  )
) %>% 
  rename(
    year = Year,
    dutytype = DutyType,
    duty_type_label = `DutyType Label`,
    market = Market,
    market_label = `Market Label`,
    origin = Origin,
    origin_label = `Origin Label`,
    productcategory = ProductCategory,
    productcategory_label = `ProductCategory Label`,
    tariff_ind = TariffIndicator,
    tariff_ind_label = `TariffIndicator Label`,
    abs = `Absolute value`,
    abs_foot = `Absolute value Footnote`,
    rate = Rate,
    rate_foot = `Rate Footnote`
  )

balance <- read_csv("data/US_GoodsAndServBalanceBpm6_ST202009031601_v1.csv") %>%
  group_by(`Series Label`) %>% tally()



tf %>% group_by(productcategory_label) %>% tally()

tradeopenness %>% group_by(`Series Label`) %>% tally()
# joining -----------------------------------------------------------------
data_un <- bind_rows(rca, tot, digital, concent) %>%
  mutate(economy_label = case_when(
    economy_label == "Switzerland, Liechtenstein" ~ "Switzerland",
    economy_label == "Yemen, Democratic" ~ "Yemen",
    TRUE ~ economy_label
  )) %>% 
  filter(economy_label != "LDCs: Africa and Haiti") %>% 
  filter(economy_label != "LDCs: Islands and Haiti") %>% 
  mutate(country_code = countryname(economy_label, "iso3c")) %>% 
  filter(!is.na(country_code))
  # group_by(economy_label) %>% 
  # tally() %>% 
  # print(n = 200)
  # select(-economy_label)

# year_1
year_1 = 2015

data_un %>% 
  group_by(variable) %>% 
  tally() %>% 
  print(n = 200)
# filtro
data_unctad <- data_un %>% 
  filter(year == year_1) %>% 
  pivot_wider(names_from = "variable",
              values_from = "values") %>% 
  select(year, country_code, "terms_trade_Terms of trade index",
         "Exports_trade_digital_deliverable_services_us_mill",
         "Imports_trade_digital_deliverable_services_us_mill",
         "terms_trade_Purchasing power index of exports"
         # "Exports_diversificacion_index",
         # # "Exports_concentration_index",
         # # "Imports_diversificacion_index",
         # "Imports_concentration_index"
         ) %>% 
  select(-year)

