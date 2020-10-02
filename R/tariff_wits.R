##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
tariff_wits <- function(path = "data/DataJobID-2059520_2059520_aguila2.csv",
                      select_year = 2016) {

# wits data
wits_tariff <- read_csv(path) %>%
  select(
    `Reporter Name`, `Simple Average`, `Weighted Average`,
    `Tariff Year`, DutyType
  ) %>%
  rename(
    country = `Reporter Name`,
    simple = `Simple Average`,
    weighted = `Weighted Average`,
    type = DutyType,
    year = `Tariff Year`
  ) %>%
  mutate(year = as.numeric(year))

# getting years to complete the data 
min_years <- min(wits_tariff[, "year"])
max_years <- max(wits_tariff[, "year"])
countries <- wits_tariff %>%
  select(country) %>%
  distinct()

# adding cols
years <- data.frame(
  y = min_years:max_years,
  values = 0
) %>%
  pivot_wider(
    names_from = y,
    values_from = values
  ) %>% 
  mutate(id = row_number()) %>% 
  full_join(data.frame(id = 1:nrow(countries))) %>% 
  select(-id) %>% 
  mutate_all(replace_na, replace = 0)

# joining data 
types <- wits_tariff %>% 
  select(type) %>% 
  distinct() %>% 
  mutate(id = row_number())

data_tariff <- years %>%
  mutate(country = countries$country) %>%
  pivot_longer(-country,
    values_to = "values",
    names_to = "year"
  ) %>%
  mutate(id = row_number()) %>%
  full_join(types) %>%
  mutate(type = if_else(is.na(type), "other", type)) %>%
  pivot_wider(names_from = type, values_from = id) %>%
  select(-other) %>%
  mutate_all(replace_na, replace = 0) %>%
  pivot_longer(
    cols = -c(country, year, values),
    values_to = "values2",
    names_to = "type"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(wits_tariff, by = c("country", "year", "type")) %>%
  select(-values, -values2) %>%
  arrange(country, type, year) %>%
  mutate(simple2 = na.locf(simple, na.rm = FALSE)) %>%
  mutate(weighted2 = na.locf(weighted, na.rm = FALSE)) %>%
  ungroup() %>%
  select(-simple, -weighted) %>%
  rename(simple = simple2, weighted = weighted2) %>%
  pivot_longer(
    cols = c("simple", "weighted"), names_to = "mean_type",
    values_to = "values"
  ) %>%
  mutate(type = paste0(type, "_", mean_type)) %>%
  select(-mean_type) %>%
  pivot_wider(names_from = "type", values_from = "values") %>%
  filter(year == select_year) %>%
  select(-year) %>%
  mutate(country_code = countryname(country, "iso3c")) %>%
  select(-country)
return(data_tariff)
}