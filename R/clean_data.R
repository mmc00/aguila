##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param path path of the data
##' @param two_headers sheets with double col
##' @param countries_names_var list of countries var name in sheets
##' @param data_cols_years sheets with year as col
clean_data <- function(path = "data/dataset_indicadores_19082020.xlsx",
                       countries_names_var <- c(
                          "Country", "Region/economy",
                          "Reporter Name", "Country Name",
                          "Country/Territory", "País",
                          "Country"
                        ),
                       code_names = c("Code", "CODE"),
                       data_cols_years <- c(
                          "Human Development Index (HDI)",
                          "Inflows FDI",
                          "WITS 2018",
                          "WB", "Telecomm"
                        ),
                       control_id_var = c(
                         "Indicator",
                         "Series Code",
                         "Indicador"
                         ),
                       two_header <- c(
                          "VoiceandAccountability",
                          "RegulatoryQuality",
                          "ControlofCorruption",
                          "GovernmentEffectiveness",
                          "RuleofLaw"
                        )
                       ) {
  sheets_on_data <- excel_sheets(path)
  data_list <- list()
  # read sheets
  data_list <- sheets_on_data %>%
    map(~ read_excel(path, .x)) %>%
    set_names(sheets_on_data)
  # rename cols
  rename_country <- function(data, cnv){
    col_replace_name <- colnames(data)[which(colnames(data) %in% cnv)]
    data <- data %>% 
      rename_with(~paste0("country"), all_of(col_replace_name))
  }
  data_list <- data_list %>%
    map(rename_country, cnv = countries_names_var) %>%
    set_names(sheets_on_data)
  # reshaping data
  reshape_data <- function(data, data_name){
    data <- data %>% select(country, everything(.))
    # two headers
    if (data_name %in% two_headers){
      # fixing code name and order data.frame
      col_replace_name <- colnames(data)[which(colnames(data) %in% code_names)]
      data <- data %>%
        rename_with(~paste0("code"), all_of(col_replace_name )) %>%
        select(country, code, everything(.))
      # paste two rows into one
      names(data)[3:ncol(data)] <- paste(data[1, 3:ncol(data)],
                                   names(data)[3:ncol(data)],
                                 sep = "-")
      # remove second row
      data <- data[2:nrow(data), ]
      # to long format
      data <- data %>%
        pivot_longer(
          cols = -c(country, code), names_to = "var",
          values_to = "values"
        ) %>% 
        separate(var, c("variable", "year"),
                 sep = "-") %>% 
        mutate(year = substr(year, 1, 4)) %>% 
        mutate(grouping_var = data_name)
    }
    # year in cols
    if (data_name %in% data_cols_years){
      # cols to replace
      col_replace_name <- colnames(data)[which(colnames(data) %in% 
                                                 control_id_var)]
      # rename col variable
      if(!is_empty(col_replace_name)){
      data <- data %>%
        rename_with(~ paste0("varible"), all_of(col_replace_name))
      } else{
      data <- data %>% 
        mutate(variable = data_name)
      }
      # to long
      # cols to long
      cols_to_long = colnames(data)
      cols_to_long = substr(cols_to_long, 1, 4)
      suppressWarnings({
      cols_to_long = as.numeric(cols_to_long)
      })
      cols_to_long = which(!is.na(cols_to_long))
      # convert data to long
      data <- data %>% 
        pivot_longer(cols = all_of(cols_to_long),
                     names_to = "year",
                     values_to = "values") %>% 
        select(all_of(c("country", "variable",
                        "year", "values"))) %>% 
        mutate(grouping_var = data_name)
    }
  
  }
}
