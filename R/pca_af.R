#Cargar librerias

library(readxl)
library(xlsx)
library(openxlsx)
library(xlsxjars)
library(tidyverse)
library(reshape2)
library(FactoMineR)
library(corrplot)
library(psych)
library(nFactors)
library(missMDA)
library(devtools)
library(factoextra)
library(data.table)

#setwd("C:/Users/Kimberly.valverde/OneDrive - Ministerio de Comercio Exterior/Kimberly Valverde/Papers y documentos de referencia/Impacto  COMEX Procomer cinde/BASES")


#Preparar los datos para analisis de componentes principales

data<-read_xlsx("data_filtered.xlsx")
head(data)
data<-data[,c(1,2,3,4)]

####

data_cross<- data %>% filter(year=="2016") %>%
  pivot_wider(id_cols=country, names_from = variable, values_from = values)

data_cross$country<-as.factor(data_cross$country)
data_cross$NAsum<-as.vector(rowSums(is.na(data_cross))) #contar los NA


#ANALISIS DE COMPONENTES PRINCIPALES


data_cross<-as.data.frame(data_cross)
row.names(data_cross)<-data_cross$country
data_cross<-data_cross[,-1]

Data_pca<-data_cross[, c(7,8,10,16,17,26,35,38,40,41,42,43,45,46,50,56,58)]

Data2<-Data_pca %>% filter(NAsum < 20)  #Base filtrada por numero de países con NA

Data2<-Data2[,-length(Data2)] #elimino la columna de suma de NA

#Imputar datos:
nb <- estim_ncpPCA(Data2,ncp.min=0,ncp.max=5,method.cv="Kfold",nbsim=50)
imputed <- imputePCA(Data2,ncp=nb$ncp)
H <- as.data.frame(imputed$completeObs) #Base de datos imputada

H1<-drop_na(Data2) #elimina todas las filas NA (Base sin NA)


#Grafico de correlación

corrplot(cor(H), method = "square", type = "lower")
A<-cor(H)

##COMPONENTES PRINCIPALES

#data_p<- as.data.frame(scale(data_p))

res_pca<-PCA(H, scale.unit = TRUE, ncp = 5, graph = TRUE)  #ajustar el número de componentes
summary(res_pca)


#Valores propios:

#Criterios: 
# Valor propio > 1  
# % de variancia explicada > 70%

eigenvalues <- res_pca$eig
head(eigenvalues[, 1:3])


#contribución de la variable al componente

res_pca$var$contrib

#contribución al primer componente (scores)
fviz_pca_contrib(res_pca, choice = "var", axes = 1)

#contribución al segundo componente (scores)
fviz_pca_contrib(res_pca, choice = "var", axes = 2)

res_desc <- dimdesc(res_pca, axes = c(1,2))

# Description of dimension 1

res_desc$Dim.1   ##Correlaciones del primer componente 
res_desc$Dim.2   ##Correlaciones del segundo componente

fviz_screeplot(res_pca, ncp=10)

#GrÃ¡fico de valores propios
barplot(res_pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res_pca$eig))
res_pca$var

#por país y por indicador
res_pca$ind
plot(res_pca,choix="ind",habillage=2) 
plot(res_pca,choix="ind")

dimdesc(res_pca, axes = 1:2)

##############################
#ANALISIS FACTORIAL

#prueba de kmo y bartlett
KMO(cor(data_imputed))
cortest.bartlett(data_imputed)  # this evaluates whether or not the variables intercorrelate at all, by evaluating the observed correlation matrix against an "identity matrix" (a matrix with ones along the principal diagonal, and zeroes everywhere else). If this test is not statistically significant, you should not employ a factor analysis.
#Significativo.

#Anlisis de Factores

fit <- factanal(H, 5, rotation = "none")  #prueba de chi cuadrado sobre numero de factores
fit

fit$loadings

cargas<-fit$loadings


ev1 <- eigen(cor(data_imputed)); ev1
ap1 <- parallel(subject = nrow(data_imputed), var = ncol(data_imputed), rep=100, cent=0.05); ap1
ns1 <- nScree(x=ev1$values, aparallel=ap1$eigen$qevpea)
plotnScree(ns1) #Gráfico de sedimentación. 

