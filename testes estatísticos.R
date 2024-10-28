install.packages("trend")
install.packages("changepoint")


library (trend)
library(openxlsx)
library(changepoint)


##TESTE DE MANN-KENDALL

dados <- read.xlsx ("C:/Users/vitor/Downloads/Word_Dynamics.xlsx")

resultado_OL <- mk.test(dados$Ocean_Literacy)

View (resultado_OL)

cat("Estatística S:", resultado_OL$statistic, "\n")
cat("Valor Z:", resultado_OL$statistic$Z, "\n")
cat("Valor-p:", resultado_OL$p.value, "\n")
cat("Tendência:", ifelse(resultado_OL$estimates[1] > 0, "Crescente", "Decrescente"), "\n")


## PONTO DE MUDANÇA

cp_result <- cpt.mean(dados$Ocean_Literacy, method = "PELT")
summary(cp_result)
plot(cp_result, main="Análise de Ponto de Mudança")


ponto_mudanca <- cpts(cp_result)
cat("Ponto de mudança (índice):", ponto_mudanca, "\n")

ano_mudanca <- dados$Year[ponto_mudanca]
cat("Ano do ponto de mudança:", ano_mudanca, "\n")


# Detectar um único ponto de mudança mais significativo na média

cp_result <- cpt.mean(dados$Ocean_Literacy, method = "BinSeg", Q = 1)
ponto_mudanca_1 <- cpts(cp_result)
ano_mudanca_1 <- dados$Year[ponto_mudanca_1]



## Análise de Regressão Temporal com Variáveis Dummy


dados$Evento_2014 <- ifelse(dados$Year == 2014, 1, 0)


View (dados)


## Teste de de Pettitt

pettitt_test <- pettitt.test(dados$Ocean_Literacy)
indice_mudanca_pettitt_OL <- pettitt_test$estimate
ano_mudanca_pettitt_OL <- dados$Year[indice_mudanca_pettitt_OL]


View(pettitt_test)

cat("Ano do ponto de mudança mais significativo:", ano_mudanca_pettitt_OL, "\n")
cat("Valor-p do teste de Pettitt:", pettitt_test$p.value, "\n")


