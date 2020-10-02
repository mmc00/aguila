##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
filtering_data2 <- function(dat,
                           path_map = "auxi/map2.xlsx",
                           drop_vars_map_sheet = "varsdrop",
                           country_var_sheet = "country_var",
                           growth_vars_sheet = "growth_vars",
                           select_year = 2017) {
  # read maps
  drop_vars <- read_xlsx(path_map, sheet = drop_vars_map_sheet)
  country_var <- read_xlsx(path_map, sheet = country_var_sheet)
  growth_vars <- read_xlsx(path_map, sheet = growth_vars_sheet) %>%
    select(1) %>%
    unlist() %>%
    unname()
  # filtering
  data <- dat %>% 
    left_join(drop_vars, by = c("variable",	"grouping_var")) %>% 
    filter(is.na(drop)) %>% 
    select(-drop) %>% 
    left_join(country_var, by = c("country",	"grouping_var")) %>% 
    filter(is.na(drop2)) %>% 
    select(-drop2)
  # changing estimates and with grouping Variable
  data <- data %>% 
    mutate(variable =case_when(
      variable == "Estimate" ~ grouping_var,
      TRUE ~ variable
    ))
  # adding growth rate vars
  data2 <- data %>%
    arrange(country, variable, year) %>%
    group_by(country, variable) %>%
    mutate(values_growth = values / lag(values) - 1) %>%
    ungroup() %>%
    filter(!is.na(values_growth)) %>%
    select(-values) %>%
    rename(values = values_growth) %>%
    filter(variable %in% growth_vars) %>% 
    mutate(variable = paste0("growth_", variable))
  # join vars
  data <- data2 %>% 
          bind_rows(data)
  # summary data
  data_year_sum <- data %>% 
  group_by(variable, grouping_var, country_code) %>%
  summarise(
    max_year = max(year),
    min_year = min(year), .groups = "drop"
  ) %>%
  mutate(years = paste0(min_year, "_", max_year)) %>%
  select(variable, grouping_var, country_code, years) %>%
  pivot_wider(names_from = country_code, values_from = years)
  # summary nas
  data_wide <- data %>%
    filter(year == select_year) %>%
    select(-country_code, -region, -grouping_var, -year) %>% 
    pivot_wider(names_from = "variable",
                values_from = "values"
                #values_fn = length
                ) %>% 
    mutate(count_na = rowSums(is.na(.)))
  # export
  # wb <- createWorkbook()
  # addWorksheet(wb, "data")
  # addWorksheet(wb, "summary")
  # addWorksheet(wb, "data_wide")
  # writeData(wb, "data", data)
  # writeData(wb, "summary", data_year_sum)
  # writeData(wb, "data_wide", data_wide)
  # saveWorkbook(wb, file = output, overwrite = T)
  return(data_wide)
}
