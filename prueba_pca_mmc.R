loadd(data2fil)
loadd(tariff)
loadd(fdi)
loadd(data_merge)
loadd(unctad)
# packages
library(missMDA)
library(corrplot)
library(psych) # KMO
library(factoextra)
library(FactoMineR)
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
    "Antigua and Barbuda",
    "Aruba",
    "Barbados",
    "Belize",
    "Dominica",
    "Suriname",
    "Guyana",
#    "Jamaica",
    "St. Lucia",
    "St. Kitts and Nevis",
    "St. Vincent and the Grenadines",
    "Venezuela",
    "Anguila",
    "Turks and Caicos Isl.",
    "Puerto Rico",
    "Korea, Dem. Rep.",
    "Venezuela, RB",
    "Virgin Islands (U.S.)",
    "Curacao",
    "Dominican Rep.",
    "Falkland (Malvinas) Is.",
    "Korea (Rep. of)",
    "Turks & Caicos Is.",
    "Virgin Islands (US)",
    "Brazil Rio de Janeiro",
    "Brazil São Paulo",
    "Japan Osaka",
    "Japan Tokyo",
    "Puerto Rico (U.S.)",
    "Trinidad and Tobago",
    "Luxembourg",
    "Bolivia"
  ))) %>% 
  filter(count_na < 20) %>%
  select(-count_na) %>% 
  select(-all_of("Score-Trading across borders (DB06-15 methodology)")) %>% 
  # select(-all_of("Score-Trading across borders (DB16-19 methodology)")) %>% 
  select(-all_of("Mobile-cellular telephone subscriptions")) %>% 
  select(-all_of("Percentage of Individuals using the Internet")) %>%
  select(-all_of("Fixed-broadband subscriptions")) %>% 
  select(-all_of("Human Development Index (HDI)")) %>% 
  select(-starts_with("DEXF")) %>% 
  select(-starts_with("FEXD")) %>% 
  select(-starts_with("Trade (% of GDP)")) %>% 
  # select(-all_of("MFN Weighted Average (%)")) %>%
  # select(-all_of("AHS Weighted Average (%)")) %>%
  # eliminadas por mi
  select(-all_of("Exports of goods and services (annual % growth)")) %>% 
  select(-all_of("Net barter terms of trade index (2000 = 100)")) %>% 
  # select(-all_of("Inflows FDI")) # %>%
  select(-all_of("growth_Inflows FDI")) 
# %>% 
#   column_to_rownames(var = "country")

# MFN ajuste
data_mfn <- data %>%
  mutate(country_code = countryname(country, "iso3c")) %>%
  left_join(tariff, by = "country_code") %>%
  select(-all_of("MFN Weighted Average (%)")) %>%
  select(-all_of("AHS Weighted Average (%)")) %>%
  select(-all_of("Trade tariffs, % duty*")) %>%
  select(-all_of("AHS_simple")) %>%
  select(-all_of("AHS_weighted")) %>%
  select(-all_of("BND_simple")) %>%
  select(-all_of("BND_weighted")) %>%
  select(-all_of("MFN_simple")) # %>%
  # select(-all_of("MFN_weighted")) # %>%
  
data <- data_mfn

# joining fdi data 
data_fdi <- data %>% 
  left_join(fdi, by = "country_code") %>% 
  select(-`Inflows FDI`) %>% 
  select(-economy_label) %>% 
  select(-fdi_cp_million) %>%
  # select(-fdi_cp_pc) %>%
  select(-fdi_perc) %>%
  select(-fdi_perc_gdp) %>%
  select(-fdi_perc_fcf)

# filter unctad vars
data <- data_fdi %>% 
  left_join(unctad, by = "country_code") %>% 
  column_to_rownames(var = "country") %>% 
  select(-country_code)
  

# fixing ICT imports for CRI
imputed_data <- data
# transformation
# library(countrycode)
# library(wpp2019)
# 
# data("pop")
# pop <- pop %>%
#   select(-name) %>%
#   pivot_longer(cols = -country_code,
#                names_to = "year",
#                values_to = "values") %>%
#   filter(year == 2015) %>%
#   select(-year) # %>%
  # mutate(country_code = as.character(country_code))

# data2 <- data %>%
#   rownames_to_column("country") %>%
#   mutate(country_code = countryname(country, "iso3n" )) %>%
#   left_join(pop, by = "country_code") %>%
#   mutate(inflows_fdi_per = `Inflows FDI`/ values) %>%
#   select(-`Inflows FDI`) %>%
#   select(-values) %>%
#   column_to_rownames("country") %>%
#   select(-country_code)

# change data 
# data <- data2
# Imputar datos:
# Estimar número de de dimensiones
# nb <- estim_ncpPCA(data, ncp.min = 0,
#                    ncp.max = 5,
#                    method.cv = "Kfold",
#                    nbsim = 50)
# 
# imputed_data <- data %>%
#   imputePCA(ncp = nb$ncp) %>%
#   pluck("completeObs") %>%
#   as.data.frame()
# 
# # En caso de eliminiar todos los que tengan NA
# imputed_data2 <- data %>%
#   drop_na

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
imputed_data2 <- imputed_data %>% 
  select(-all_of("terms_trade_Terms of trade index")) %>% 
  select(-all_of("ICT goods imports (% total goods imports)")) %>% 
  select(-all_of("No. Of imported HS6 digit Products")) %>% 
  # select(-all_of("Quality of electricity supply, 1-7 (best)")) %>% 
#  select(-all_of("fdi_cp_million")) %>% 
  select(-all_of("terms_trade_Purchasing power index of exports")) %>% 
  select(-all_of("HH Market concentration index")) %>%
  select(-all_of("Trade in services (% of GDP)")) %>%
  select(-all_of("Imports_trade_digital_deliverable_services_us_mill")) %>% 
  select(-all_of("Comercio de mercaderías (% del PIB)")) %>%
  select(-all_of("Quality of roads, 1-7 (best)")) #%>% 
  # select(-all_of("Burden of customs procedures, 1-7 (best)"))
  
    

corr_plot2 <- corrplot(cor(imputed_data2), method = "square",
                       type = "lower", tl.pos = "ld")

#fit <- factanal(imputed_data, 4, rotation = "none")
#fit
fit2 <- factanal(imputed_data2, 4, rotation = "varimax",
                 cutoff = 0.3)
# fit2 <- factanal(imputed_data2, 4, rotation = "varimax")

data_plot <- fit2 %>% 
  broom::tidy() %>% 
  select(variable) %>% 
  unname() %>% 
  unlist()
  # filter_at(.vars = c("fl1", "fl2",
  #                     "fl3", "fl4"), any_vars(. >= 0.3 | . <= -0.3))
data_plot
library(semPlot)
semPaths(fit2, 
         what = "std", 
         layout = "tree",
         style = "lisrel",
         reorder = T,
         residuals=FALSE,
         manifests = data_plot,
         latents = c("Factor1", "Factor2", "Factor3", "Factor4"),
         cut = 0.3,
         rotation = 2,
         sizeMan = 20,
         shapeMan = "rectangle",
         sizeMan2 = 10,
         sizeLat = 5)
         # pastel = TRUE, 
         # what = "par",
         # mar = c(2, 2, 2, 2),
         # intercepts = TRUE,
         # edge.label.cex = 1,
         # sizeMan = 7,

         # style="lisrel",         
         # layout="tree",
         # exoCov = FALSE,
         # optimizeLatRes = TRUE,
         # sizeLat = 10) 


# library(psych)
fit3 <- fa(imputed_data2, 4, fm = "ml", rotate = "varimax")
fit3$loadings
fa.diagram(fit3, sort = T, main = "Análisis Factorial",
                      digits = 3, 
           rsize = .6,
           esize = 3,
           size = 5,
           cex = .6,
           l.cex = .2,
           cut = .4)
           # marg=c(.2,.2,.2,.2))



fit1 <- broom::tidy(fit) %>%
  mutate_at(.vars = c("fl1", "fl2", "fl3", "fl4"), .funs = ~round(., 2))
View(fit1)

res_pca <- PCA(imputed_data, scale.unit = TRUE, ncp = 4, graph = F,
               )

fit2 <- res_pca$var$cor
View(fit2)


pca_res <- res_pca$var$cor
broom::tidy(pca_res) %>% View()

remove(data)
remove(imputed_data)
remove(nb) 
remove(data2fil)

# plot --------------------------------------------------------------------
library(plotly)
plot_data <- factanal(imputed_data, 3, rotation = "none") %>%
  broom::tidy() %>%
  select(-uniqueness) %>%
  mutate_at(.vars = c("fl1", "fl2", "fl3"), .funs = ~ round(., 2)) %>% 
  pivot_longer(cols = c("fl1", "fl2", "fl3"),
               names_to = "factor",
               values_to = "values") %>%
  mutate(type = case_when(
    ((values >= 0.50 | values <= -0.50) & factor == "fl1") ~ "Institucionalidad",
    ((values >= 0.50 | values <= -0.20) & factor == "fl2") ~ "Comercio bienes",
    ((values >= 0.50 | values <= -0.20) & factor == "fl3") ~ "Clima de inversión",
    TRUE ~ "Impacto leve"
  )) %>% 
  pivot_wider(names_from = "factor", values_from = "values")


plot_data %>%
  plot_ly(
    x = ~fl1, y = ~fl2, z = ~fl3,
    color = ~type,
    #hoverinfo = "text",
    text = ~paste0("Variable: ", variable)
  ) %>%
  add_markers()

plot_data %>%
  plot_ly(
    x = ~fl1, y = ~fl2, z = ~fl3)
read_csv('https://raw.githubusercontent.com/plotly/datasets/master/alpha_shape.csv')

