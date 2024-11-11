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



data <- read.xlsx("C:/Users/vitor/Downloads/dados_C1_university.xlsx")
colnames(data)


### PARA FAZER RP
dados_limpos <- data %>%
  distinct(RP, TI, .keep_all = TRUE)


output_file_path <- "C:/Users/vitor/Downloads"
write.xlsx(dados_limpos, output_file_path, sheetName = "OL", overwrite = TRUE)


##### PARA FAZER DE (palavras-chave)
dados_limpos_nova <- dados_limpos %>%
  distinct(TI, DE, .keep_all = TRUE)

write.xlsx(dados_limpos_nova, output_file_path, sheetName = "OL", overwrite = TRUE)



df <- dados_limpos_nova


dados_separados <- df %>%
  separate(DE, into = c("Palavra1", "Palavra2", "Palavra3", "Palavra4", "Palavra5", "Palavra6", "Palavra7", "Palavra8", "Palavra9", "Palavra10", "Palavra11", "Palavra12", "Palavra13", "Palavra14", "Palavra15", "Palavra16", "Palavra17"), 
           sep = ";", fill = "right")


df_keyw <- dados_separados %>%
  select(Palavra1:Palavra17)


palavras_chave <- c("Ocean Literacy", "Climate Literacy", "Climate Change", "Environmental Education", "Marine Education", "Climate Change Education", "Education", "Citizen Science", "Global Warming", "Marine Science Education")

df2 <- as.data.frame(matrix(0, nrow = nrow(df), ncol = length(palavras_chave)))
colnames(df2) <- palavras_chave

df2


for (keyword in palavras_chave) {
  df2[[keyword]] <- apply(df_keyw, 1, function(row) ifelse(keyword %in% row, 1, 0))
}


pca_result <- PCA(df2, scale.unit = FALSE, graph = FALSE)
fviz_pca_biplot(pca_result, repel = TRUE)


summary(pca_result)

