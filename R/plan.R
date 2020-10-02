the_plan <-
  drake_plan(
    # load raw data
    long_data =
      clean_data(path = 
                   file_in("data/dataset_indicadores_28082020.xlsx")),
  
    # fixing countries names
    data_country = 
      fixing_countries_names(long_data),
    
    # filter data with specific map vars
    data_filtered = filtering_data(data_country,
                           path_map = file_in("auxi/map.xlsx"),
                           output = file_out("auxi/data_filtered.xlsx")),
    
    # adding WEF, RTA and WB data to data filtered
    data_merge = wef_rta_wb_join(data_filtered, 
                                 file_in("data/wef_rta_wb.xlsx")),

    # year selected for analysis
    year = 2016,
    
    # second filter
    data2fil = target(filtering_data2(data_merge,
                           path_map = file_in("auxi/map2.xlsx"),
                           select_year = year),
                dynamic = map(year)),
    
    # tariff data complete
    tariff = target(tariff_wits(path = file_in(
      "data/DataJobID-2059520_2059520_aguila2.csv"
    ),
    select_year = year
    ), dynamic = map(year))
)
