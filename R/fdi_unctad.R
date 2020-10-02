##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
fdi_unctad <- function(path = "data/US_FdiFlowsStock_ST202007161100_v1.csv",
                       select_year = 2016,
                       flow = "Flow",
                       direction = "Inward") {
  
  data <-
    read_csv(path,
      col_types = cols(
        Year = col_double(),
        Economy = col_character(),
        `Economy Label` = col_character(),
        Mode = col_character(),
        `Mode Label` = col_character(),
        Direction = col_double(),
        `Direction Label` = col_character(),
        `US dollars at current prices in millions` = col_double(),
        `US dollars at current prices in millions Footnote` = col_character(),
        `US dollars at current prices per capita` = col_double(),
        `US dollars at current prices per capita Footnote` = col_logical(),
        `Percentage of total world` = col_double(),
        `Percentage of total world Footnote` = col_logical(),
        `Percentage of Gross Domestic Product` = col_double(),
        `Percentage of Gross Domestic Product Footnote` = col_logical(),
        `Percentage of Gross Fixed Capital Formation` = col_double(),
        `Percentage of Gross Fixed Capital Formation Footnote` = col_logical()
      )
    ) %>%
    rename(
        year = Year,
        country_code = Economy,
        economy_label = `Economy Label`,
        mode = Mode,
        mode_label = `Mode Label`,
        direction2 = Direction,
        direction_label = `Direction Label`,
        fdi_cp_million = `US dollars at current prices in millions`,
        fdi_cp_million_foot = `US dollars at current prices in millions Footnote`,
        fdi_cp_pc = `US dollars at current prices per capita`,
        fdi_cp_pc_foot = `US dollars at current prices per capita Footnote`,
        fdi_perc = `Percentage of total world`,
        fdi_perc_foot = `Percentage of total world Footnote`,
        fdi_perc_gdp = `Percentage of Gross Domestic Product`,
        fdi_perc_gdp_foot = `Percentage of Gross Domestic Product Footnote`,
        fdi_perc_fcf = `Percentage of Gross Fixed Capital Formation`,
        fdi_perc_fdc_foot = `Percentage of Gross Fixed Capital Formation Footnote`
    ) %>% 
    filter(year == select_year) %>%
    filter(mode_label == flow) %>%
    filter(direction_label == direction) %>%
    mutate(largo = nchar(country_code)) %>%
    filter(largo == 3) %>% 
    select(-ends_with("foot")) %>% 
    select(-year, -mode_label, -direction2, -mode,
           -direction_label, -largo
           ) %>% 
    mutate(economy_label =
             if_else(economy_label == "Switzerland, Liechtenstein",
                     "Switzerland",
                     economy_label)) %>% 
    mutate(country_code = countryname(economy_label, "iso3c")) %>% 
    # remove dup france
    filter(economy_label != "France, metropolitan")
  return(data)
}