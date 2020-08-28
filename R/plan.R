the_plan <-
  drake_plan(

    long_data =
      clean_data(path = 
                   file_in("data/dataset_indicadores_28082020.xlsx")),
    data_country = 
      fixing_countries_names(long_data)

    )
