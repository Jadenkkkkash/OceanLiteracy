install.packages("bibliometrix")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("stringr")


library(bibliometrix)
library(openxlsx)
library(dplyr)
library(stringr)


biblioshiny()


## CARREGAR O ARQUIVO

file_path <- " ##adiciona o arquivo##"
data <- read.xlsx(file_path, sheet = 1)


# VERIFICANDO AS COLUNAS
colnames(data)


## RETIRAR AS LINHAS COM NOME DE ARTIGO REPETIDO



data_unique <- data %>%
  distinct(TI, .keep_all = TRUE)


##REMOVER OS PARENTESES ORCID AUTORES SCOPUS

remove_parentheses <- function(text) {
  gsub("\\s*\\([^\\)]+\\)", "", text)
}

data_unique$AF <- sapply(data_unique$AF, remove_parentheses)

data_unique[data_unique == ""] <- NA
data_unique[data_unique == "NULL"] <- NA

##CONFERE

head(data_unique$AF, 10)


## RETIRAR CAPSLOCK, COLOCAR PRIMEIRA LETRA EM CAIXA ALTA

colnames(data_unique)
data_unique$TI <- str_to_title(data_unique$TI)
data_unique$SO <- str_to_title(data_unique$SO)
data_unique$DE <- str_to_title(data_unique$DE)
data_unique$CR <- str_to_title(data_unique$CR)

head(data_unique$TI)
head(data_unique$SO)
head(data_unique$DE)
head(data_unique$CR)


## NOVAMENTE RETIRAR ARTIGOS REPETIDOS NO TITULO (CAPS ATIVO)

colnames(data_unique)

data_uninew <- data_unique %>%
  distinct(TI, .keep_all = TRUE)


## CARREGAR O ARQUIVO PARA BAIXAR

output_file_path <- "##Local que deseja adicionar o arquivo##"
write.xlsx(data_uninew, output_file_path, sheetName = "tratado Wos scopus", overwrite = TRUE)


######### ATÉ AQUI OS DADOS FORAM TRATADOS #########


### separando keywords que contenham ocean literacy, climate/climate change literacy e energy literacy

#definindo palavras-chave

head(data_uninew$DE)


palavras_chave_OL <- c("Ocean Literacy", "Marine Education")
palavras_chave_CL <- c("Climate Literacy", "Climate Change Education")
palavras_chave_EL <- c("Energy Literacy", "Energy Education" )

padrao_OL <- paste0("(^|; )(", paste(palavras_chave_OL, collapse = "|"), ")(;|$)")
padrao_CL <- paste0("(^|; )(", paste(palavras_chave_CL, collapse = "|"), ")(;|$)")
padrao_EL <- paste0("(^|; )(", paste(palavras_chave_EL, collapse = "|"), ")(;|$)")



data_filtrada_OL <- data_uninew %>%
  filter(str_detect(DE, padrao_OL))

data_filtrada_CL <- data_uninew %>%
  filter(str_detect(DE, padrao_CL))

data_filtrada_EL <- data_uninew %>%
  filter(str_detect(DE, padrao_EL))


write.xlsx(data_filtrada_OL, output_file_path, sheetName = "OL", overwrite = TRUE)
write.xlsx(data_filtrada_CL, output_file_path, sheetName = "CL", overwrite = TRUE)
write.xlsx(data_filtrada_EL, output_file_path, sheetName = "EL", overwrite = TRUE)


## Criação da planilha o OL com CL

palavras_chave_OL_CL <- c("Ocean Literacy", "Cultura Oceânica","Climate Literacy", "Climate Change Literacy")

padrao_OL <- paste0("(^|; )(", paste(palavras_chave_OL_CL, collapse = "|"), ")(;|$)")

OL_CL <- data_uninew %>%
  filter(str_detect(DE, padrao_OL_CL))

