##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param dat
filtering_data <- function(dat,
                           path_map = "auxi/map.xlsx",
                           map_countries_sheet = "region",
                           drop_vars_map_sheet = "varsdrop") {
  
  # read maps
  map_countries <- read.xlsx(path_map, sheet = map_countries_sheet)
  drop_vars <- read.xlsx(path_map, sheet = drop_vars_map_sheet)
  # filtering
  data <- dat %>% 
    left_join(map_countries, by = "country_code") %>% 
    left_join(drop_vars, by = c("variable",	"grouping_var")) %>% 
    filter(!is.na(region)) %>% 
    filter(is.na(drop)) %>% 
    select(-drop)
  
  return(data)
}
