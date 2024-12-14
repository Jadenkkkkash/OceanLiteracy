anos <- seq(2000, 2024)  # Anos de 2010 a 2020
artigos <- c(0, 0, 2, 0, 0, 2, 1, 2, 8, 3, 4, 11, 3, 14, 24, 17, 14, 24, 28, 48, 47, 64, 93, 87, 54)  # Quantidade de artigos

dados <- data.frame(anos, artigos)

media_historica <- mean(dados$artigos)

dados$anomalia <- dados$artigos - media_historica

dados$anomalia_acumulada <- cumsum(dados$anomalia)

print(dados)


plot(dados$anos, dados$anomalia_acumulada, type="o", col="blue",
     xlab="Anos", ylab="Anomalia Acumulativa",
     main="Anomalia Acumulativa de Artigos ao longo dos Anos")


install.packages("openxlsx")
library(openxlsx)

write.xlsx(dados, file = "C:/Users/Windows/Downloads", SheetName= "Anomalias")


## REALIZANDO TESTE DE MANN KENDALL PARA CADA PONTO


install.packages("Kendall")
install.packages("zoo")

library(Kendall)
library(zoo)



anomalias <- dados$anomalia


resultado_mk <- MannKendall(anomalias)
resultado_mk


### a partir daqui não deu certo o código

dados2 <- dados[, c("anos", "anomalia_acumulada")]



resultados_mk2 <- rollapply(dados2, width = 3, FUN = function(x) {
  test <- MannKendall(x)
  return(test$sl)  # Valor p da estatística de Mann-Kendall
}, by = 1, align = "center")




##### RECORTANDO O GRÁFICO PARA ANALISAR AS TENDENCIAS 

## rever daqui para baixo pois não está batendo os resultados

anos_2015 <- seq(2015, 2024)
artigos_2015 <- c(17, 14, 24, 28, 48, 47, 64, 93, 87, 54)

dados_2015 <- data.frame(anos_2015, artigos_2015)

dados_2015$anomalia <- dados_2015$artigos_2015 - media_historica

dados_2015$anomalia_acumulada <- cumsum(dados_2015$anomalia)


anomalias_2015 <- dados_2015$anomalia_acumulada


resultado_mk <- MannKendall(anomalias_2015)
resultado_mk


##CLIMATE LITERACY

anos <- seq(2000, 2024)  # Anos de 2010 a 2020
artigos_CL <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 6, 1, 3, 2, 3, 11, 9, 8, 13, 8, 8)  # Quantidade de artigos

dados <- data.frame(anos, artigos_CL)

media_historica <- mean(dados$artigos_CL)

dados$anomalia <- dados$artigos_CL - media_historica

dados$anomalia_acumulada <- cumsum(dados$anomalia)

print(dados)


plot(dados$anos, dados$anomalia_acumulada, type="o", col="blue",
     xlab="Anos", ylab="Anomalia Acumulativa",
     main="Anomalia Acumulativa de Artigos ao longo dos Anos")


install.packages("openxlsx")
library(openxlsx)

write.xlsx(dados, file = "C:/Users/vitor/Downloads", SheetName= "Anomalias")


## REALIZANDO TESTE DE MANN KENDALL PARA CADA PONTO


install.packages("Kendall")
install.packages("zoo")

library(Kendall)
library(zoo)



anomalias <- dados$anomalia


resultado_mk <- MannKendall(anomalias)
resultado_mk


### a partir daqui não deu certo o código

dados2 <- dados[, c("anos", "anomalia_acumulada")]



resultados_mk2 <- rollapply(dados2, width = 3, FUN = function(x) {
  test <- MannKendall(x)
  return(test$sl)  # Valor p da estatística de Mann-Kendall
}, by = 1, align = "center")




### OCEAN LITERACY

##CLIMATE LITERACY

anos <- seq(2000, 2024)  # Anos de 2010 a 2020
artigos_OL <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 3, 2, 2, 3, 3, 18, 10, 15, 20, 21, 13)  # Quantidade de artigos

dados <- data.frame(anos, artigos_OL)

media_historica <- mean(dados$artigos_OL)

dados$anomalia <- dados$artigos_OL - media_historica

dados$anomalia_acumulada <- cumsum(dados$anomalia)

print(dados)


plot(dados$anos, dados$anomalia_acumulada, type="o", col="blue",
     xlab="Anos", ylab="Anomalia Acumulativa",
     main="Anomalia Acumulativa de Artigos ao longo dos Anos")


install.packages("openxlsx")
library(openxlsx)

write.xlsx(dados, file = "C:/Users/vitor/Downloads", SheetName= "Anomalias")


## REALIZANDO TESTE DE MANN KENDALL PARA CADA PONTO


install.packages("Kendall")
install.packages("zoo")

library(Kendall)
library(zoo)



anomalias <- dados$anomalia


resultado_mk <- MannKendall(anomalias)
resultado_mk


### a partir daqui não deu certo o código

dados2 <- dados[, c("anos", "anomalia_acumulada")]



resultados_mk2 <- rollapply(dados2, width = 3, FUN = function(x) {
  test <- MannKendall(x)
  return(test$sl)  # Valor p da estatística de Mann-Kendall
}, by = 1, align = "center")
