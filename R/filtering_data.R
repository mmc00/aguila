##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
filtering_data <- function(dat,
                           path_map = "auxi/map.xlsx",
                           map_countries_sheet = "region",
                           drop_vars_map_sheet = "varsdrop",
                           output) {
  
  # read maps
  map_countries <- read_xlsx(path_map, sheet = map_countries_sheet)
  drop_vars <- read_xlsx(path_map, sheet = drop_vars_map_sheet)
  # filtering
  data <- dat %>% 
    left_join(map_countries, by = "country_code") %>% 
    left_join(drop_vars, by = c("variable",	"grouping_var")) %>% 
    filter(!is.na(region)) %>% 
    filter(is.na(drop)) %>% 
    select(-drop)
  # summary data
  data_year_sum <- data %>% 
  group_by(variable, grouping_var, country_code) %>% 
  summarise(max_year = max(year),
            min_year = min(year), .groups = "drop") %>% 
  mutate(years = paste0(min_year, "_", max_year)) %>% 
  select(variable, grouping_var, country_code, years) %>% 
  pivot_wider(names_from = country_code, values_from = years)
  
  wb <- createWorkbook()
  addWorksheet(wb, "data")
  addWorksheet(wb, "summary")
  writeData(wb, "data", data)
  writeData(wb, "summary", data_year_sum )
  saveWorkbook(wb, file = output, overwrite = T)
  return(data)
}
