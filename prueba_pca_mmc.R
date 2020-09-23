loadd(data2fil)

# packages
library(missMDA)
library(corrplot)
library(psych) # KMO
# #### Kim

# data <- read_xlsx("data_filtered.xlsx")
# head(data)
# data <- data[, c(1, 2, 3, 4)]
 
# data_cross <- data %>%
#   filter(year == "2016") %>%
#   pivot_wider(id_cols = country, names_from = variable, values_from = values)
# 
# data_cross$country <- as.factor(data_cross$country)
# 
# data_cross$count_na <- as.vector(rowSums(is.na(data_cross))) #contar los NA
# 
# data2fil <- data_cross

# #### 

# filtrar datos
data <- data2fil %>%
  # filtrar paises arbitrariamente
  filter(!(country %in% c(
#    "Antigua and Barbuda",
    "Barbados",
    "Belize",
    "Dominica",
#    "Jamaica",
#    "St. Lucia",
    "St. Kitts and Nevis",
#    "St. Vincent and the Grenadines",
    "Venezuela"
  ))) %>% 
  column_to_rownames(var = "country") %>% 
  filter(count_na < 20) %>%
  select(-count_na) %>% 
  select(-all_of("Score-Trading across borders (DB06-15 methodology)")) %>% 
  select(-all_of("Mobile-cellular telephone subscriptions")) %>% 
  select(-all_of("Percentage of Individuals using the Internet")) %>%
  select(-all_of("Fixed-broadband subscriptions")) %>% 
  select(-all_of("Human Development Index (HDI)")) %>% 
  # eliminadas por mi
  select(-all_of("Exports of goods and services (annual % growth)")) %>% 
  select(-all_of("Net barter terms of trade index (2000 = 100)")) %>% 
#  select(-all_of("Inflows FDI")) # %>%
  select(-all_of("growth_Inflows FDI"))

# transformation
library(countrycode)
library(wpp2019)

data("pop")
pop <- pop %>% 
  select(-name) %>% 
  pivot_longer(cols = -country_code, 
               names_to = "year",
               values_to = "values") %>% 
  filter(year == 2015) %>% 
  select(-year) # %>% 
  # mutate(country_code = as.character(country_code))

data2 <- data %>% 
  rownames_to_column("country") %>% 
  mutate(country_code = countryname(country, "iso3n" )) %>% 
  left_join(pop, by = "country_code") %>% 
  mutate(inflows_fdi_per = `Inflows FDI`/ values) %>% 
  select(-`Inflows FDI`) %>% 
  select(-values) %>% 
  column_to_rownames("country") %>% 
  select(-country_code)

# change data 
data <- data2
# Imputar datos:
# Estimar número de de dimensiones
nb <- estim_ncpPCA(data, ncp.min = 0,
                   ncp.max = 5,
                   method.cv = "Kfold",
                   nbsim = 50)

imputed_data <- data %>% 
  imputePCA(ncp = nb$ncp) %>% 
  pluck("completeObs") %>% 
  as.data.frame()

# En caso de eliminiar todos los que tengan NA
imputed_data2 <- data %>%
  drop_na

##### Eligir cuales datos usar
# imputed_data <- imputed_data2

# Plot de correlacion
corr_plot <- corrplot(cor(imputed_data), method = "square", type = "lower")
corr_data <- cor(imputed_data)

##############################
# Analisis factorial 

#prueba de kmo y bartlett
KMO(cor(imputed_data))

# this evaluates whether or not the variables
# intercorrelate at all, by evaluating the observed correlation matrix
# against an "identity matrix" (a matrix with ones along the principal diagonal,
# and zeroes everywhere else). If this test is not statistically significant,
# you should not employ a factor analysis.
cortest.bartlett(imputed_data)
# Significativo.

# Análisis de Factores
fit <- factanal(imputed_data, 4, rotation = "none",
                scores = "regression")  
fit
remove(data)
remove(imputed_data)
remove(nb) 
remove(data2fil)

broom::tidy(fit) %>%
  View()
