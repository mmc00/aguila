# debuggin firts function
# # data <- data_list[["WITS 2018"]]
# # data_name <- "WITS 2018"
# lista_data <- clean_data(path = "data/dataset_indicadores_28082020.xlsx")
# # data2 <- lista_data %>% 
# #   bind_rows
# # 
# # data = lista_data[[1]]
# # 
# # 
# data = data_list2[[13]]
# data_name = names(data_list2[13])
# path = "data/dataset_indicadores_28082020.xlsx"
#                        countries_names_var = c(
#                           "Country", "Region/economy",
#                           "Reporter Name", "Country Name",
#                           "Country/Territory", "País",
#                           "Country", "...2",
#                           "Country / Region"
#                         ),
#                        code_names = c("Code", "CODE"),
#                        data_cols_years = c(
#                           "Human Development Index (HDI)",
#                           "Inflows FDI",
#                           "WITS 2018",
#                           "WITS completo 2017",
#                           "WB", "Telecomm",
#                           "TiVA", "RTA"
#                         )
#                        control_id_var = c(
#                          "Indicator",
#                          "Series Code",
#                          "Indicador",
#                          "Indicator Name"
#                          )
#                        two_headers = c(
#                           "VoiceandAccountability",
#                           "RegulatoryQuality",
#                           "ControlofCorruption",
#                           "GovernmentEffectiveness",
#                           "RuleofLaw"
#                         )
#                        var_in_cols_type_sheet = c(
#                          "DB", "FTA CR"
#                        )
#                        var_to_remove_in_cols = c(
#                          "Country code", "Region",
#                          "Income group"
#                        )
#                        var_year = c(
#                          "DB Year",
#                          "Year of entry into force"
#                        )
#                        
#fixing_countries_names(data)
# loadd(long_data)
# data = long_data

# data with good name country
# loadd(data_country)
# vars <- data_country %>%
#   group_by(variable, grouping_var) %>%
#   tally()
# 
#  maping vars
# data3 <- data_country %>% 
#   mutate(region = countrycode(country_code, 
#                               origin = "iso3c",
#                               destination = "region")) %>% 
#   filter(region == "Latin America & Caribbean") %>% 
#   select(country_code, region) %>% 
#   distinct(., .keep_all = T)
# write_csv(data3, "auxi/latam.csv")
         

# country_dic <- data_country %>%
#   select(country, country_code) %>%
#   distinct(., .keep_all = T)
