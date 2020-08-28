the_plan <-
  drake_plan(

    long_data =
      clean_data(path = 
                   file_in("data/dataset_indicadores_28082020.xlsx")),
    data_country = 
      fixing_countries_names(long_data),
    
    data_filtered = filtering_data(data_country,
                           path_map = file_in("auxi/map.xlsx"),
                           output = file_out("auxi/data_filtered.xlsx"))

    )
