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
                                 file_in("data/wef_rta_wb.xlsx"))

    )
