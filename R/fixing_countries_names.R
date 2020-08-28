##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
##' @param bad_names path of the bad names output
fixing_countries_names <- function(data,
                                   bad_names = "auxi/bad_names_countries.csv") {

  data2 <- data %>% 
    mutate(country_code = countryname(country, "iso3c"))
  
  # know how many are unknow
  data3 <- data2 %>% 
    group_by(country_code) %>% 
    tally()
  # finding
  data3 <- data2 %>% 
    filter(is.na(country_code)) %>% 
    select(country, country_code) %>% 
    distinct(., .keep_all = TRUE)
  # printing out
  write_csv(data3, bad_names)
  # checking good names respect codes
  data4 <- data2 %>% 
    filter((!is.na(country_code))) %>% 
    select(-code)
  return(data4)
}
