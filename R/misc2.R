# library("factoextra")
# data("decathlon2")
# df <- decathlon2[1:23, 1:10]
# 
# 
# loadd(data_filtered)
# install.packages("pmdplyr")
# remotes::install_github("jacob-long/panelr")
# 
# library(plotly)
# library(tidyverse)
# m <- matrix(rnorm(9), nrow = 3, ncol = 3)
# fig <- plot_ly(
#     x = c("a", "b", "c"), y = c("d", "e", "f"),
#     z = m, type = "heatmap"
# )

# library(panelr)
# install.packages("dplyr")
# remotes::install_github("jacob-long/panelr",
#                         upgrade = "never",
#                         dependencies = FALSE)
# install.versions('panelr', '0.7.3')


# loadd(data_merge)
# data <- data_merge %>%
#   group_by(variable) %>%
#   mutate(row = row_number()) %>%
#   pivot_wider(values_from = "values",
#                     names_from = "variable") %>%
#   select(-row) %>%
#   mutate(year = as.numeric(year))
# 
# 
# j0 <- data_merge %>%
#   filter(startsWith(variable, "Preva")) %>%
#   filter(country_code %in% c("CRI"))
# # 
# j0 <- data_merge %>%
#   filter(grouping_var == "DB") %>%
#   filter(country_code %in% c("USA"))

# j0 <- data_merge %>%
#   filter(grouping_var == "DB") %>%
#   filter(country_code %in% c("MEX"))

# 
# j1 <- data_merge %>%
#   filter(startsWith(variable, "Imports as a percentage of GDP*")) %>%
#   select(variable, grouping_var) %>% 
#   distinct()
# j1

# datap <- panel_data(data, id = country_code, wave = year)
# sum_data <- summary(datap)
# Viewing the second filer vars -------------------------------------------
# loadd(data2fil)
# data <- data2fil %>% 
#   filter(is.na(values)) %>% 
#   filter(year %in% c(2005:2015))
