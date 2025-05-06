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
library(readxl)



OL_CL <- read_excel("C:/Users/Windows/Downloads/OL_CL_2025_1.xlsx")

max_cols <- OL_CL %>%
  pull(DE) %>%
  str_count(";") %>%
  max() + 1

OL_CL_separado <- OL_CL %>%
  separate(DE, into = paste0("DE_", 1:max_cols), sep = ";", remove = TRUE)


DE_KEYW <- OL_CL_separado %>%
  select(starts_with("DE_"))


write.xlsx(DE_KEYW, "C:/Users/Windows/Downloads/", sheetName = "OL", overwrite = TRUE)




palavras_chave <- c("Ocean Literacy", "Climate Literacy", "Climate Change", "Environmental Education", "Climate Change Education", "Marine Education", "Citizen Science", "Education", "Science Communication",  "Global Warming")


n_rows <- nrow(DE_KEYW)
n_keywords <- length(palavras_chave)


results <- matrix(0, nrow = n_rows, ncol = n_keywords)
colnames(results) <- c(
  "Ocean Literacy", "Climate Literacy", "Climate Change", 
  "Environmental Education", "Climate Change Education", 
  "Marine Education", "Citizen Science", "Education", 
  "Science Communication", "Global Warming"
)

for (i in 1:n_rows) {
  for (j in 1:n_keywords) {
    keyword <- palavras_chave[j]
    
    # Verifica se a palavra-chave está presente em qualquer coluna da linha i
    results[i, j] <- ifelse(any(grepl(keyword, DE_KEYW[i, ], ignore.case = TRUE)), 1, 0)
  }
}


results_df <- as.data.frame(results)


write.xlsx(results_df, "C:/Users/Windows/Downloads/", sheetName = "OL", overwrite = TRUE)


###### MCA

results_df[] <- lapply(results_df, factor)

mca_result <- MCA(results_df, graph = FALSE)


summary(mca_result)

View(mca_result$var$coord)
head(mca_result$var$contrib, n = 20)

mca_coord <- mca_result$var$coord

write.xlsx(mca_coord, "C:/Users/vitor/Downloads", sheetName = "OL", overwrite = TRUE)

res.mca <- mca_result


# calculando a MCA dim 6

mca_result <- MCA(results_df, ncp = 10)
mca_result$var$contrib[,6]
mca_result$var$coord[,6] 


head(mca_result$var$coord[, 1:6], 20)        
head(mca_result$var$contrib[, 1:6], 20)    
head(mca_result$var$cos2[, 1:6], 20)        


#### GRAFICOS


fviz_mca_var(mca_result, repel = TRUE)



## salve graficos
library(ggplot2)
ggsave("grafico_mca.png", width = 10, height = 8, dpi = 300)




fviz_mca_var(mca_result, axes = c(1,2),  repel = TRUE)
  iz_mca_var(mca_result, axes = c(1, 6), repel = TRUE)



## GRAFICO SO AS VARIAVEIS
fviz_mca_var(mca_result, choice = "var", axes = c(1, 2), repel = TRUE)


##SO INDIVIDUOS
fviz_mca_ind(mca_result, axes = c(1, 2), repel = TRUE)


## MISTO (CATEGORIA + INDIVIDUO)

fviz_mca_biplot(mca_result, repel = TRUE)





## ESCOLHER MELHORES REPRRESENTAÇÕES (VER AUTOVALOR)

eig.val <- get_eigenvalue(mca_result)
print(eig.val)



######PLANILHA

# Coordenadas das categorias
coords_var <- mca_result$var$coord

# Contribuições das categorias para as dimensões
contrib_var <- mca_result$var$contrib

# Cos2 (qualidade da representação)
cos2_var <- mca_result$var$cos2

# Vtest
v.test <- mca_result$var$v.test

# Coordenadas dos indivíduos
coords_ind <- mca_result$ind$coord



write.xlsx(v.test, "C:/Users/Windows/Downloads/", sheetName = "OL", overwrite = TRUE)
