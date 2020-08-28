the_plan <-
  drake_plan(

    long_data =
      clean_data(path = 
                   file_in("data/dataset_indicadores_28082020.xlsx"))

    )
