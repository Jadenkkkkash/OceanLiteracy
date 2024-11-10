#Script para aula de Análise de Dados Meteoceanográficos no R - 05/11/2024
#MULTIVARIADAS CLÁSSICAS
#comando para ir para a pasta de trabalho
setwd("C:/Users/DELLNOTE/Documents")
#instalar bibliotecas
install.packages("devtools")
#instalar bibliotecas a partir do Github
library(devtools) #coman
install_github("millerlp/oceanwaves")

install.packages("factoextra")

#Bibliotecas que serao utilizadas
# install.packages("corrr")
library('corrr')
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("FactoMineR")
library("FactoMineR")

library(dplyr)# biblioteca de manipulação de dados
library(tidyverse) # Para manipular os dados
library(esquisse) #é uma biblioteca e uma ferramenta de visualização de dados em R que permite criar visualizações de dados detalhadas usando o pacote ggplot2
library(ggplot2) #para criar gráficos
library(factoextra) # Para criar alguns gráficos
library(stats) # Para fazer análises estatísticas gerais
library(FactoMineR) #Análise exploratória de dados
library(corrr) #Para fazer análise de correlação
library(ggcorrplot) #para visualizar gráficos a partir da matriz de correlação


#PRIMEIRA COISA A FAZER: COLOCAR NA PASTA DO R
setwd("~/Aula_softwares/aula_PPGOAM_2024")

install.packages("readxl")

#Análise de PCA
#importar os dados - DADOS METEOROLÓGICOS EXCEL
library(readxl)
library(readxl)




AMClassicas <- read_excel("AMClassicas.xlsx", 
                          sheet = "PCA_CCA")
View(AMClassicas)

dados <- na.omit(AMClassicas) #extraindo os NA

#resumo dos dados
str(dados) #verificando as classes de dados presentes na planilha


dadosV <- dados[,-1] #Devemos ter uma matriz somente com as variáveis numéricas
# excluindo a primeira coluna, por isso -1

# remover essas colunas:
# tem que retirar todos os dados qualitativos, só pode ficar dado numérico
new <- rm[,"Ponto", "granulo"]

Pontos <- dados[,1] #extraindo o nome dos pontos



#dizendo que o esse vetor é um fator, precisamos disso para executar
dados$Ponto <- as.factor(dados$Ponto)

##### NORMALIZANDO OS DADOS
library(stats)
#RODANDO A PCA

#normaliza os dados
dadoN <- scale(dadosV)

#analise de componentes principais
zc.pca <- prcomp(dadosV, scale = TRUE)


# se ja normalizou.... 
zc.pca <- prcomp(dadoN, scale = FALSE)


####### CONTRIBUIÇÃO DAS COMPONENTES
summary(zc.pca)

#com o summary: desvio padrão, variação de proporção e proporção acumulada (>80% explica bem o local)
#### é um resultado latente, então vai ser resultado de todos os dados analisados, ou seja, é de acordo com os dados que eu possuo
## variaveis que mais contribuem para a variância do local; busco quem ajuda a entender o ambiente
## pelo menos entender até 80%


## se até o PC4 já tem 80%, se coloca o número 4 ao invés do 7
zc.pca$rotation[, 1:7]


## os maiores valores absolutos mostram quais mais contribuiem para a componente principal
## nos dados AMClassicas:
##### PC 1 silte, argila, CU...
##### PC 2 areia fina contribui positivamente para ambas, areia média contribui positivo na primeira e negativo na segunda

#### as variaveis que mais contribuem para a primeira componente equivalem a 0.428 (variância de proporção), então são mais importantes
## ploto a segunda componente x terceira componente OU primeira componente x segunda componente.....

## se num artigo: eu faço PC1 X PC2, se a PC3 x PC4 identificar pelo menos 10% da amostra, aí plotamos também PC3 x Pc4
## num mestrado ou doutorado, realizar PC1 x PC2, PC3 x Pc4, PC5 x Pc6....

### extrai a informação da tabela e escreve no resultado. decida se quer utilizar as figuras se achar necessário

## tabela de contribuição

#gráfico da pca

library(factoextra)
# fviz_pca_ind(): gráfico de indivíduos/amostras
# fviz_pca_var(): gráfico de variáveis
# fviz_pca_biplot(): gráfico de indivíduos e variáveis
fviz_pca_var(zc.pca, col.var = "black")

## para dim 3 e dim 4
fviz_pca_var(zc.pca, col.var = "black")


### grafico para mostrar o quanto cada componente contribui
p <- fviz_eig(zc.pca,barfill = "white", barcolor = "black",main = , xlab = "Dimensões", ylab = "Porcentagem de variância explicada (%)",) #porcentagem explicada por cada uma das dimensões
p + theme_classic(base_size = 15)

#cos2 - Cos2 é chamado de cosseno quadrado (coordenadas quadradas) e corresponde à qualidade de representação das variáveis. Mostra a importância de um componente principal para uma determinada observação (vetor de variáveis originais).
#################

library(ggplot2)
library(devtools)
install_github("vqv/ggbiplot")

install.packages("ggbiplot")
library(ggbiplot)

fviz_pca_var(zc.pca,
             col.var = "contrib", # Cor por contribuições para o PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Evitar sobreposição de texto
)
#contribuição das variáveis a cada dimensão
#plotar variaveis e individuos
fviz_pca_biplot(zc.pca)

res.ind <- get_pca_ind(zc.pca) #fornece os resultado quanto aos indivíduos/as observações
res.ind$contrib        # Contribuições para as CPs

#Plot do biplot
resultado <- fviz_pca_biplot(zc.pca, geom=c("point", "text"), 
                             label = "var", 
                             col.var = "contrib",
                             mean.point = FALSE,
                             fill.ind = dados$Ponto, col.ind = "white",
                             pointshape = 21, pointsize = 3
)+
  labs(fill = "Pontos") 
resultado
escore <- head(zc.pca$x, 30)

parameter <- par(mfrow = c(2, 2))
# Contribuições das variáveis para PC1
fviz_contrib(zc.pca, choice = "var", axes = 1, ggtheme = theme_classic(base_size = 16), ylim = c(0, 100), cex = 1, cex.lab = 1.8, col.lab = "black")
# Contribuições das variáveis para PC1
fviz_contrib(zc.pca, choice = "var", axes = 2, ggtheme = theme_classic(base_size = 16), ylim = c(0, 100),cex = 1, cex.lab = 1.8, col.lab = "black")
# Contribuições das variáveis para PC1
fviz_contrib(zc.pca, choice = "var", axes = 3,ggtheme = theme_classic(base_size = 16), ylim = c(0, 100),cex = 1, cex.lab = 1.8, col.lab = "black")
# Contribuições das variáveis para PC1
fviz_contrib(zc.pca, choice = "var", axes = 4,ggtheme = theme_classic(base_size = 16), ylim = c(0, 100),cex = 1, cex.lab = 1.8,col.lab = "black")






library(corrplot)
#plotando a contribuição das observações em cada uma das dimensões
fviz_contrib(zc.pca, choice = "ind",top = 30, axes = 1, ggtheme = theme_classic(base_size = 14), cex = 1, cex.lab = 1.8, col.lab = "black")
fviz_contrib(zc.pca, choice = "ind",top = 30, axes = 2, ggtheme = theme_classic(base_size = 14), cex = 1, cex.lab = 1.8, col.lab = "black")
fviz_contrib(zc.pca, choice = "ind",top = 30, axes = 3, ggtheme = theme_classic(base_size = 14), cex = 1, cex.lab = 1.8, col.lab = "black")
fviz_contrib(zc.pca, choice = "ind",top = 30, axes = 4, ggtheme = theme_classic(base_size = 14), cex = 1, cex.lab = 1.8, col.lab = "black")




################################################################################


# ANÁLISE DE Correlação Canônica
library(stats)
install.packages("psych")
library(psych)
install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(corrplot)
install.packages("CCA")
library(CCA)
install.packages("DT")
library(DT)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("CCP")
library(CCP)
install.packages("candisc")
library(candisc)

AMClassicas <- read_excel("AMClassicas.xlsx", 
                          sheet = "PCA_CCA")
View(AMClassicas)

dados <- na.omit(AMClassicas) #extraindo os NA
dadosN <- scale(dados[ ,2:20]) #normalizando o conjunto de dados
# Identificando os dois grupos de variáveis
sed <- dadosN[ ,1:9] # grupo de sedimentologia
geoq <- dadosN[ ,10:19] # grupo de geoquímica

#Observando as correlações dentro de cada grupo e entre todas as variáveis
#Dentro do grupo Sedimento
S <- cor(sed,use = "pairwise.complete.obs",method = c("pearson"))
corrplot(S, method = 'circle',insig='blank',tl.cex = 0.8,tl.col = 'black',col = COL2('RdBu', 10))
#Dentro do grupo Geoquímica
GQ <- cor(geoq,use = "pairwise.complete.obs",method = c("pearson"))
corrplot(GQ, method = 'circle',insig='blank',tl.cex = 0.8,tl.col = 'black',col = COL2('RdBu', 10))

#Entre todas as variáveis
QA <- cor(dadosN,use = "pairwise.complete.obs",method = c("pearson"))
corrplot(QA, method = 'circle',insig='blank',tl.cex = 0.8,tl.col = 'black',col = COL2('RdBu', 10))

#Análise de Correlação Canônica: Estrutura matcor(X, Y) X = grupo 1 e Y grupo 2
#Existem diversas funções que fazem a Análise de correlação canônica, como por exemplo: cancor, matcor, CCorA, cc e outras.
#Exemplo com a matcor

resultado <- cancor(X, Y)

# Análise de correlação canônica
resultado <- cancor(sed, geoq) 
print(resultado)

# Ver as Correlacoes canônicas
print(resultado)  # mostra os resultados gerais
summary(resultado) #  mostra os resultados gerais e os coeficientes canonicos brutos

#Interpretação dos parâmetros de saída
#CanR: Cada par de variáveis canônicas (um do conjunto X e um do conjunto Y) gera um escore canônico. O escore canônico é uma nova variável que combina as variáveis originais de forma a capturar a máxima variação em ambas as matrizes. 
#O valor do CanR indica a força da relação entre os dois conjuntos de variáveis.
# CanRSQ: Indica quão bem a variável canônica correspondente (gerada a partir do conjunto X) explica a variância da variável canônica correspondente do conjunto Y. Um valor mais alto significa que uma maior proporção da variância em Y é explicada pela combinação de variáveis em X.
#Eigen:Indica a quantidade de variância que cada par de variáveis canônicas explica. Um autovalor maior indica que a combinação canônica correspondente é mais informativa e tem uma maior capacidade de explicar a variação nos dados.
#percent:Indica a fração da variância total que é explicada pelo par canônico correspondente. Por exemplo, se um par canônico tem um valor de 50% no percent, isso significa que essa combinação de variáveis canônicas explica metade da variância total do conjunto de dados.
#cum : soma das variâncias explicadas pelos pares canônicos

#Identificando os pares de eixos canônicos
summary(resultado) 
#Os eixos canônicos correspondem aos pares canônicos (ex: o 1º eixo canônico de X é associado ao 1º eixo canônico de Y).
#Para cada par canônico, você pode visualizar as correlações e os coeficientes para entender a relação.
# Coeficientes canônicos: os coeficientes canônicos mostram como as variáveis contribuem para cada eixo
coeficientes <- resultado$coef
coeficientes
#library(ggplot2)

######ALTERNATIVA ######
#Para plotar a biblioteca CCA tem mais opções. Nela é a função cc que faz a Análise de Correlação Canônica

cc2 <- cc(sed, geoq)
print(cc2)
cc2$cor # Mostra a correlação canônica. O equivalente ao CanR da cancor
cc2$xcoef #Mostra os coeficientes canônicos no grupo x. No nosso caso o grupo sedimento. 
cc2$ycoef #Mostra os coeficientes canônicos no grupo y. No nosso caso o grupo geoquímico. 

#plotando os eixos canônicos
plt.cc(cc2,d1=1,d2=2,type="v", var.label = TRUE, ind.names = NULL)


#################################


#Análise de Correspondência
## tem que transformar em dados categoricos
### não precisa normalizar


library(FactoMineR)
library(factoextra)
install.packages("FSAdata")
library(FSAdata)
data("WalleyeErie2")
dado.ac <- na.omit(WalleyeErie2)

View(dado.ac)

#Colocar as variáveis categóricas como fator
dado.ac$sex <- as.factor(dado.ac$sex)
dado.ac$mat <- as.factor(dado.ac$mat)
dado.ac$loc <- as.factor(dado.ac$loc)

#criar a tabela de contingência
tabela <- table(dado.ac$mat,dado.ac$sex)
#Para uma pequena tabela de contingência, você pode usar o teste qui-quadrado para avaliar se há uma dependência significativa entre categorias de linha e coluna:
chisq.res <- chisq.test(tabela)
chisq.res

#Outra opção é usar a função CA do pacote FactoMineR

CA(X, ncp = 2, graph = TRUE) 
#onde X é uma tabela de contingência; ncp é o número de dimensões;graph: um valor lógico. Se TRUE, um gráfico é exibido.

res.ca <- CA(tabela, ncp = 2, graph = TRUE)
res.ca

res.ca$eig
res.ca$col
res.ca$coord
res.ca$ind

## cos2 se for pequeno, ajuda a entender pouco  (se mantem praticamente constante, independente do que ocorra existe pouca variância)
## inertia o quanto contribuiu de fato



####Interpretando os resultados
# "eig" (autovalores): Os autovalores indicam a quantidade de variância explicada por cada dimensão (eixo) da análise. Um autovalor maior sugere que a dimensão explica mais variância nos dados.
# "var" (variáveis): Esse componente mostra os resultados das variáveis. Você encontrará:
#  "coord": Coordenadas das variáveis nos eixos. Quanto mais longe do ponto de origem, mais relevante a variável.
# "contrib": Contribuição das variáveis para os eixos. Indica a importância de cada variável na construção dos eixos.
# "ind" (indivíduos): Resultados relacionados aos indivíduos na análise:
# # "coord": Coordenadas dos indivíduos nos eixos. Similar às variáveis, a distância do centro indica relevância.
# "contrib": Contribuição de cada indivíduo para os eixos.
# "quanti" e "quali": Se você estiver trabalhando com variáveis quantitativas e qualitativas, esses parâmetros ajudam a interpretar como cada tipo de variável se relaciona com os eixos.





#Análise de Correspondência Múltipla - ACM/MCA

#A ACM pode ser executada com várias funções, como a MCA() function do pacote FactoMineR, a dudi.mca() do pacote ade4, e a função epMCA() do pacote ExPosition 
install.packages("ade4")
install.packages("ExPosition")
library("FactoMineR")
library("factoextra")
library(ade4)
library(ExPosition)


tabela2 <- dado.ac[, c("loc","sex","mat")] #extraindo as colunas das variáveis categóricas
str(tabela2)
mca_resultado <- MCA(tabela2, graph = TRUE)
mca_resultado
mca_resultado$eig  #Mostra os autovalores para cada uma das dimensões principais (eixos) da MCA. Os autovalores indicam a quantidade de variação explicada por cada uma das dimensões.


mca_resultado$var
## femeas imaturas são individuos que mais contribuem para variabilidade; os locais 1 e 3 são as condições principais, não interessando se é femea ou macho, maduro ou imaturo

## autovalores e contribuição são os mais utilizados

mca_resultado$var #Mostar informações sobre as variáveis do seu conjunto de dados. 
#coord - As coordenadas das variáveis
#contrib - mostra a contribuição de cada variável para cada dimensão
#cos2 - Mostra o quanto a variável é representada em cada uma das dimensões. Valores próximos de 1 indicam que a variável está bem representada pela dimensão, enquanto valores próximos de 0 indicam uma má representação.
#v.test - mostra os resultados do teste que verifica se as coordenadas das variáveis nas dimensões são significativamente diferentes de zero
#eta2 -  mostra a proporção da variabilidade nas dimensões principais explicada pelas variáveis.
mca_resultado$ind #Mostra informações sobre os indivíduos

mca_resultado$svd #Mostra as matrizes resultantes da SVD e os autovalores (valores singulares) que são usados para construir as dimensões.

  
#para plotar use as funções do pacote factoextra
fviz_screeplot(mca_resultado, addlabels = TRUE, ylim = c(0, 50), xlab = "Dimensões", ylab = "Porcentagem da variância explicada (%)",linecolor = "#FC4E07",
               barcolor = "#2E9FDF", barfill = "#2E9FDF") #plota as dimensões

fviz_mca_var(mca_resultado, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)

## percebemos que femeas imaturas são as que mais contribuem no componente 1 e no componente 2 locais 1 e 3



fviz_mca_biplot (mca_resultado) #Mostra individuos e variáveis

tabelaconjunta <- prop.table(table(dado.ac$mat,dado.ac$sex))

tabelaconjunta

# fviz_mca_ind(): Gráfico de indivíduos
# fviz_mca_var(): Gráfico de variáveis
# fviz_mca_biplot(): Biplot de indivíduos e variáveis
# fviz_mca(): Um alias de fviz_mca_biplot()


#######



#Análise de Redundância
install.packages("vegan")
library(vegan)
#Você precisa de dois conjuntos de dados: as variáveis dependentes e as variáveis independentes.
#variáveis dependentes: dados de abundância ou presença de espécies.
#variáveis independentes: dados ambientais.

AMClassicas <- read_excel("AMClassicas.xlsx", 
                          +     sheet = "RDA_CCoA")

##pegamos através do import dataset


View(AMClassicas)
AMClassicas <- na.omit(AMClassicas)
plancton <- AMClassicas[ ,5:15]
ambiente <- AMClassicas[ ,1:4]
ambiente_scale <- scale(ambiente) #é necessário normalizar os dados ambientais
plot(ambiente$SST, plancton$Copepod) #verificar a relação entre as variáveis ambientais e as biológicas

### RDA só pode ser feito se for normalizado e se a planilha de dados biológicos não apresentar outliers 

cor(AMClassicas)
PM <- cor(AMClassicas,use = "pairwise.complete.obs",method = c("pearson"))
library(corrplot)
corrplot(PM, method = 'number',insig='blank',tl.cex = 0.8,tl.col = 'black')

boxplot(plancton)
plancton <- scale(plancton)

#Convertendo em dataframe
ambiente <- as.data.frame (ambiente)
plancton <- as.data.frame (plancton)

#Executando a RDA
resultado_rda <- rda(plancton ~ SST + Salinity + ChlaSurf + SampleDepth_m, data = ambiente)

## cmoo clorofila não foi significativa, pode ser retiRada

resultado_rda <- rda(plancton ~ SST + Salinity + SampleDepth_m, data = ambiente)

#### unconstrained são a quantidade que não são explicadas, no caso dessa amostra é muito maior
#### inertia é autovalor



#Ver o resultado da RDA 
summary(resultado_rda)
#Constrained: Refere-se à variação explicada pelas variáveis ambientais.
#Unconstrained: Refere-se à variação não explicada pelas variáveis ambientais (variabilidade residual ou erro).

####eigenvalue é o valor dos autovalores das "constrained" (variaveis que são explicadas)


#Plotando os resultados da RDA
plot(resultado_rda)
#### a variavel que mais ta ligada com a primeira variavel é SST, negativa mas é a que mais que contribui. 


#Verificando os resíduos do modelo
resíduos <- residuals(resultado_rda)
plot(resíduos)
## residuos não podem depender do valor das variaveis
### nesse caso, não temos um caso homogeneo, consigo identificar as observações, são esses pontos que estão atrapalhando os dados (os pontos do lado de fora), identificando podemos retirar quando temos vários dados, arrancar 20 dados não vai ser muita coisa caso tivermos 600,


#Caso os resíduos apresentem heterocedasticidade ou "variância não constante", isso pode indicar problemas no modelo que precisam ser investigados.

