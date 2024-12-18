# # 
#AULA ANÁLISE DE Árvore de Decisão, Bagging, Boosting
#1º passo: carregar as bibliotecas
# Bibliotecas disponíveis para implementar uma árvore de decisão: ctree, rpart, tree, etc.

library(caret)
library(randomForest)
library(readxl)
library(rpart)
library(rpart.plot)
library(tree)
library(tidyverse)

#2º PASSO: ABRIR O R DENTRO DA PASTA ONDE ESTÃO A ROTINA E A BASE DE DADOS
setwd("~/Aula_softwares/aula_PPGOAM_2024/Análise de agrupamento")

setwd("C:/Users/Windows/Downloads")

#3ºPASSO: ABRIR O ARQUIVO COM A BASE DE DADOS
Classificacao <- read_excel("Regressao_Classificacao.xlsx", sheet = "classificacao")


Classificacao <- read_excel("DE_BIN_copy.xlsx")
View(Classificacao)

#4ºPASSO: Excluir as linhas com dados ausentes

dados <- na.omit(Classificacao)

#5ºPASSO: Coloca os dados de branqueamento como fatores

dados$Bleaching_level <- as.factor(dados$Bleaching_level)# dados$Classificacao1 <- as.factor(dados$Classificacao1)
# dados$Classes_M <- as.factor(dados$Classes_M)


dados$Ocean.Literacy <- as.factor(dados$Ocean.Literacy)
dados$Climate.Literacy <- as.factor(dados$Climate.Literacy)
dados$Climate.Change <- as.factor(dados$Climate.Change)
dados$Environmental.Education <- as.factor(dados$Environmental.Education)
dados$Marine.Education <- as.factor(dados$Marine.Education)
dados$Climate.Change.Education <- as.factor(dados$Climate.Change.Education)
dados$Education <- as.factor(dados$Education)
dados$Citizen.Science <- as.factor(dados$Citizen.Science)
dados$Global.Warming <- as.factor(dados$Global.Warming)
dados$Marine.Science.Education <- as.factor(dados$Marine.Science.Education)


#6ºPASSO: ver a estrutura do banco de dados

str(dados)

#7ºPASSO: separa os dados em conjunto de treinamento e conjunto teste
set.seed(123)
samp <-  dados$Bleaching_level %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.dados <- dados[samp, ]
test.dados <- dados[-samp, ]


samp <-  dados %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.dados <- dados[samp, ]
test.dados <- dados[-samp, ]

trainIndex <- createDataPartition(dados$Ocean.Literacy, p = 0.7, list = FALSE)
trainData <- dados[trainIndex, ]
testData <- dados[-trainIndex, ]


#8ºPASSO: checar as dimensões
dim(train.dados)
dim(test.dados)

#9ºPASSO: Construir o modelo
# min_n - Quantidade mínima de observações dentro de um nó para se considerar dividir em duas folhas novas. 
# Quanto menor, maior risco de overfitting.
# tree_depth - Profundidade: quanto mais profunda a árvore for, maior risco de overfitting.
# cost_complexity - Parâmetro de complexidade: limite mínimo de ganho de informação que a divisão tem que fornecer para concretizar a criação das folhas.

modelo_tree <- decision_tree(
  min_n = tune(),
  tree_depth = tune(),
  cost_complexity = tune() 
)


modelo_tree <- rpart(Education ~ Ocean.Literacy + Climate.Literacy + Climate.Change + 
                       Environmental.Education + Marine.Education + 
                       Climate.Change.Education +  Citizen.Science + 
                       Global.Warming + Marine.Science.Education, 
                     data = trainData, method = "class",
                     control = rpart.control(maxdepth = 15, # Aumentando a profundidade
                                             minsplit = 2,  # Permitindo mais divisões
                                             cp = 0.0001))  # Menor valor de cp para menos poda



# Visualizar a árvore de decisão mais profunda
rpart.plot(modelo_tree)

# Prever com o conjunto de teste
predicoes <- predict(modelo_tree, testData, type = "class")

# Certificar que as predições e a variável real sejam fatores
testData$Education <- as.factor(testData$Education)

# Caso as predições estejam como character, converta para fator também
predicoes <- factor(predicoes, levels = levels(testData$Education))

# Avaliar o modelo com matriz de confusão
confusao <- confusionMatrix(predicoes, testData$Education)

# Imprimir o resultado
print(confusao)


png("arvore_decisao.png", width = 2000, height = 1500, res = 300)




#Modelo Random Forest com a função rpart
#nome_do_modelo <- rpart(Variavel_Resposta ~ ., data = train.dados, control = controle) aqui estamos prevendo a variavel resposta a partir de todas as variáveis preditoras, por isso o ponto. O símbolo "~" significa previsão
blanch.tree <- rpart(Bleaching_level ~ .,  data = train.dados)

printcp(blanch.tree) # mostra is resultados

plotcp(blanch.tree) #resultados da validação
# CP: Valores do parâmetro de complexidade para cada nó.
# nsplit: Número de divisões realizadas.
# rel error: Erro relativo da árvore.
# xerror: Erro relativo calculado via validação cruzada.
# xstd: Desvio padrão do erro de validação cruzada.

#Definindo parâmetros de controle no modelo
controle <- rpart.control(minsplit = 400,  # Número mínimo de observações para dividir um nó. Entre 1 e 2%, para N <100, usar 5.
                          cp = 0.01,     # Parâmetro de complexidade para poda
                          maxdepth = 6)  # Profundidade máxima da árvore. Testar valores entre log2(n) e 2 × log2(n).
#Executar novamente com o ncontrole
blanch.tree <- rpart(Bleaching_level ~ .,  data = train.dados, control= controle)

printcp(blanch.tree) # mostra is resultados
#Plotando a árvore de decisão

rpart.plot(blanch.tree, 
           cex = 0.7, 
           extra = 4, #extra = 0: Exibe apenas o nome da classe.; extra = 1: Exibe o número de observações em cada nó.; extra = 2: Mostra o percentual de observações.; extra = 4: Adiciona a probabilidade de cada classe.
           type = 2,  #type = 0: Mostra apenas os rótulos das folhas.; type = 1: Mostra divisões nos nós internos e os rótulos das folhas.;type = 2: Mostra apenas os rótulos das folhas, mas dentro das caixas de decisão.
           box.palette = "RdYlGn")

#10ºPASSO: Fazer as predições 
pred_test1 <- predict(blanch.tree, newdata = test.dados, type= "class")

pred_test1

#11ºPASSO:  Desempenho do modelo ###
# Matriz de confusão e medidas
library(caret)
confusionMatrix(table(pred_test1,test.dados$Bleaching_level))

#No Information Rate (NIR): Proporção da classe mais frequente no conjunto de dados (usada como baseline).
#P-Value: Verifica se o modelo é significativamente melhor que a taxa de acerto do acaso (baseado no NIR).
#Accuracy: Proporção de previsões corretas. 
# Kappa: Uma medida de concordância ajustada para o acaso. Varia de -1 a 1, onde 1 é concordância perfeita. 
# Para cada classe, são fornecidas métricas que avaliam o desempenho específico:
# Sensitivity (Recall): Capacidade de identificar corretamente os exemplos positivos.
# Specificity: Capacidade de identificar corretamente os exemplos negativos.
# Precision: Proporção de previsões corretas entre todas as previsões positivas.
# F1 Score: Média harmônica de Precision e Recall.
# Balanced Accuracy: Média de Sensitivity e Specificity.
# Prevalence: Proporção de observações reais de uma classe.
# Detection Rate: Taxa de detecção correta de cada classe.

#########################
#OPÇÃO PARA ÁRVORE DE DECISÃO - REGRESSÃO
regressao <- read_excel("Regressao_Classificacao.xlsx", sheet = "regressao")
View(regressao)
dados <- na.omit(regressao)
#Dividir os dados em conjunto treino e teste
set.seed(123)
library(caret)
library(stats)
library(factoextra)
library(tidyverse)
library(rpart)
library(rpart.plot)

#1º PASSO: Importando o conjunto de dados
Regressao <- read_excel("Regressao_Classificacao.xlsx", sheet = "regressao")

#2º PASSO: Retira as linhas com dados ausentes
dadosR <- na.omit(Regressao)

#separa os dados em conjunto de treinamento e conjunto teste

amostrastreinamento <-  dadosR$Percent_Bleaching %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.dataR  <- dados[amostrastreinamento, ]
test.dataR <- dados[-amostrastreinamento, ]
#checar as dimensões
dim(train.dataR)
dim(test.dataR)

# Construindo o modelo
Regmodel <- rpart(Percent_Bleaching ~ ., data = train.dataR, method = "anova")
printcp(Regmodel) # mostra os resultados do modelo


#Definindo parâmetros de controle no modelo
controle <- rpart.control(minsplit = 400,  # Número mínimo de observações para dividir um nó. Entre 1 e 2%, para N <100, usar 5.
                          cp = 0.01,     # Parâmetro de complexidade para poda
                          maxdepth = 6)  # Profundidade máxima da árvore. Testar valores entre log2(n) e 2 × log2(n).

#Construindo o modelo com os parÂmetros de controle
Regmodel <- rpart(Percent_Bleaching ~ ., data = train.dataR, method = "anova", control = controle)

printcp(Regmodel)  # mostra os resultados do modelo
#Plotando o resultado
rpart.plot(Regmodel,
           type = 2, #type = 2: Mostra apenas os rótulos das folhas, mas dentro das caixas de decisão.
           cex = 0.7, 
           extra = 1, #exibir número de obs. para cada nó terminal
           box.palette = "auto")

#identificar o melhor valor de cp a ser usado
best <- Regmodel$cptable[which.min(Regmodel$cptable[,"xerror"]),"CP"]

#produzir uma árvore podada com base no melhor valor cp
pruned_tree <- prune(Regmodel, cp=best)
rpart.plot(pruned_tree,
           type = 2,
           cex = 0.7, 
           extra = 101, # Exibe média, desvio padrão e % de observações
           box.palette = "auto")
#verificar o modelo
validacao <- predict(pruned_tree, test.dataR)

library(MLmetrics)
#Verificar precisão da previsão com as métricas MSE, MAE, RMSE e R-quadrado.
mse = MSE(validacao,test.dataR$Percent_Bleaching) #erro quadrático médio
mae = MAE(validacao,test.dataR$Percent_Bleaching) #erro médio absoluto
rmse = RMSE(validacao,test.dataR$Percent_Bleaching) #Raiz do erro quadrático médio
r2 = R2(validacao,test.dataR$Percent_Bleaching, form = "traditional") #R2

cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)

# Random Forest
library(randomForest)
# Bibliotecas disponíveis para implementar o Random Forest: random forest
modelo_rf <- rand_forest(
  min_n = tune(),
  mtry = tune(),
  trees = tune()
)
# min_n – Qtd mínima de observações no nó para poder dividir.
# mtry – Quantidade de variáveis (colunas) sorteadas por árvore. Tem que testar via cross-validation, pois é afetado pela razão entre variáveis boas e ruído.
# trees – Número de árvores (amostras bootstrap) para treinar. Não afeta muito o overfitting.
# obs: random forest não usa CP. Ele permite que as árvores cresçam indeterminadamente, condicionadas apenas pelo min_n.
#1ºPASSO: ABRIR O ARQUIVO COM A BASE DE DADOS
Classificacao <- read_excel("Regressao_Classificacao.xlsx", sheet = "classificacao")
View(Classificacao)

#2ºPASSO: Excluir as linhas com dados ausentes

dados <- na.omit(Classificacao)

#3ºPASSO: Coloca os dados de branqueamento como fatores

dados$Bleaching_level <- as.factor(dados$Bleaching_level)# dados$Classificacao1 <- as.factor(dados$Classificacao1)
# dados$Classes_M <- as.factor(dados$Classes_M)

#4ºPASSO: ver a estrutura do banco de dados

str(dados)

#5ºPASSO: separa os dados em conjunto de treinamento e conjunto teste
set.seed(123)
samp <-  dados$Bleaching_level %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data <- dados[samp, ]
test.data <- dados[-samp, ]

#6ºPASSO: checar as dimensões
dim(train.data)
dim(test.data)

#7ºPASSO: Construindo o modelo

BL_rf <- randomForest(Bleaching_level ~ ., data = train.data, ntree = 1000, mtry = 4, importance =TRUE) #MODELO DE CLASSIFICAÇÃO

print(BL_rf)
# OOB - estimate of  error rate: mede o desempenho do modelo em prever observações não utilizadas durante a construção de cada árvore individual no conjunto.
# Matriz de confusão:
# As linhas representam as classes verdadeiras.
# As colunas representam as classes preditas.
# class.error: Taxa de erro para cada classe individual.


#saídas do Random Forest

#Importância das variáveis
# o parâmetro "type" especifica o tipo de medida de importância: 1 = diminuição média na Acurácia, 2 = diminuição média na impureza do nó.
# MeanDecreaseAccuracy:
importance(BL_rf, type = 1)

# MeanDecreaseGini: diminuição total nas impurezas do nó da divisão na variável, calculada em média para todas as árvores
importance(BL_rf, type = 2)
#plota ambos os índices
varImpPlot(BL_rf, main = "Importância das Variáveis")

# Quantas vezes cada variável explicativa foi utilizada na construção das árvores
varUsed(BL_rf, count = T)

# Predições #
pred_test <- predict(BL_rf, newdata = test.dados, type= "class") #Constrói o modelo de validação

pred_test #mostra os resultados do modelo

confusionMatrix(pred_test , test.dados$Bleaching_level) #plota os resultados da matriz de confusão

plot(BL_rf)

#VERIFICAR O MODELO DE REGRESSÃO
#separando dados para construir o modelo
Regressao <- read_excel("Regressao_Classificacao.xlsx", sheet = "regressao")
#separa os dados em conjunto de treinamento e conjunto teste
dados <- na.omit(Regressao)

amostrastreinamento <-  dados$Percent_Bleaching %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- dados[amostrastreinamento, ]
test.data <- dados[-amostrastreinamento, ]
#checar as dimensões
dim(train.data)
dim(test.data)
BP_rf <- randomForest(Percent_Bleaching ~ ., data = train.data, ntree = 1000, mtry = 4, importance =TRUE) #MODELO DE REGRESSÃO

#verificar o modelo

validacao_rf <- predict(BP_rf, test.data)

library(MLmetrics)
#Verificar precisão da previsão com as métricas MSE, MAE, RMSE e R-quadrado.
mse = MSE(validacao_rf,test.data$Percent_Bleaching) #erro quadrático médio
mae = MAE(validacao_rf,test.data$Percent_Bleaching) #erro médio absoluto
rmse = RMSE(validacao_rf,test.data$Percent_Bleaching) #Raiz do erro quadrático médio
r = R2(validacao_rf,test.data$Percent_Bleaching, form = "traditional") #R2

cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse, "\n", "R-squared:", r2)
