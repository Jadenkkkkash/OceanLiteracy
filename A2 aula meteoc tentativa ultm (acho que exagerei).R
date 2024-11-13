install.packages("openxlsx")
install.packages("dplyr")
install.packages("stringr")
install.packages("dplyr")
install.packages("factoextra")



library(openxlsx)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(factoextra)
library (FactoMineR)


DE_KEYW <- read.xlsx("C:/Users/vitor/Downloads/DE_KEYW.xlsx")

df2 <- read.xlsx("C:/Users/vitor/Downloads/DE_BIN.xlsx")


palavras_chave <- c("Ocean Literacy", "Climate Literacy", "Climate Change", "Environmental Education", "Marine Education", "Climate Change Education", "Education", "Citizen Science", "Global Warming", "Marine Science Education")


n_rows <- nrow(DE_KEYW)
n_keywords <- length(palavras_chave)


results <- matrix(0, nrow = n_rows, ncol = n_keywords)
colnames(results) <- colnames(df2)


for (i in 1:n_rows) {
  for (j in 1:n_keywords) {
    keyword <- palavras_chave[j]
    
    # Verifica se a palavra-chave está presente em qualquer coluna da linha i
    results[i, j] <- ifelse(any(grepl(keyword, DE_KEYW[i, ], ignore.case = TRUE)), 1, 0)
  }
}


results_df <- as.data.frame(results)


write.xlsx(results_df, "C:/Users/vitor/Downloads", sheetName = "OL", overwrite = TRUE)

    
###### MCA

results_df[] <- lapply(results_df, factor)

mca_result <- MCA(results_df, graph = FALSE)


summary(mca_result)

fviz_mca_ind(mca_result, 
             repel = TRUE, 
             col.ind = "darkred") 


artigos_coords <- mca_result$ind$coord  # Coordenadas dos artigos


dist_matrix <- dist(artigos_coords)


print(as.matrix(dist_matrix))



cluster_result <- hclust(dist(artigos_coords), method = "ward.D2")


plot(cluster_result, labels = rownames(results_df), main = "Dendrograma de Agrupamento dos Artigos",
     xlab = "Artigos", ylab = "Distância", sub = "")


##numero de clusters

num_clusters <- 3
clusters <- cutree(cluster_result, k = num_clusters)

# Adicionando os clusters ao data frame original
results_df$Cluster <- clusters

# Visualizando os artigos com 
o grupo correspondente
print(results_df)




##### TABELA DAS DISTANCIAS

dist_table <- as.data.frame(as.matrix(dist_matrix))
rownames(dist_table) <- rownames(results_df)
colnames(dist_table) <- rownames(results_df)


print(dist_table)


### clusters

cluster_result <- hclust(dist(artigos_coords), method = "ward.D2")
clusters <- cutree(cluster_result, k = 3)  # Ajuste k para o número desejado de clusters

# Adicionando os clusters ao dataframe original
results_df$Cluster <- clusters

# Exibindo a tabela com os artigos e seus respectivos clusters
print(results_df)

write.xlsx(results_df, "C:/Users/vitor/Downloads", sheetName = "OL", overwrite = TRUE)



# Ordenando a tabela de distâncias para identificar os pares mais próximos
dist_table_long <- as.data.frame(dist_table)  # Transformando a matriz em formato longo
colnames(dist_table_long) <- c("Artigo_1", "Artigo_2", "Distância")

# Ordenando pela menor distância
dist_table_long <- dist_table_long[order(dist_table_long$Distância), ]

# Exibindo os pares mais próximos (as distâncias mais baixas)
head(dist_table_long, 10)  # Mostra os 10 pares de artigos mais próximos






#### MAIS COISA

# Transformando a tabela longa em uma matriz de distâncias
dist_matrix <- as.matrix(xtabs(Distância ~ Artigo_1 + Artigo_2, data = dist_table_long))

# Realizando o agrupamento hierárquico com a matriz de distâncias
cluster_result <- hclust(dist(dist_matrix), method = "ward.D2")

# Definindo o número de clusters desejados, ajuste conforme necessário
k <- 3  # Número de clusters (ajuste conforme necessário)
clusters <- cutree(cluster_result, k = k)

# Criando uma tabela para associar os artigos com seus clusters
# Supondo que os artigos estão nomeados nas linhas da matriz de distâncias
artigos <- rownames(dist_matrix)

# Criando o dataframe com os clusters atribuídos
result_with_clusters <- data.frame(Artigo = artigos, Cluster = clusters)

