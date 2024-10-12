install.packages("tidyverse")
install.packages("readxl")
install.packages("openxlsx")


library(tidyverse)
library(readxl)
library(openxlsx)

# Ler a planilha do Excel
dados <- read_excel("C:/Users/vitor/Downloads/OL CL meteoc.xlsx")

# Exibir os primeiros dados
head(dados)

# Suponha que você tenha as colunas: "Autores", "Instituicoes", e outras como "Ano", "Título"
# Primeiro, separamos os autores e instituições em listas, mantendo as outras colunas
dados_separados <- dados %>%
  mutate(
    AU = str_split(AU, ";"),
    C1 = str_split(C1, ";")
  )

head(dados_separados)

dados_com_erro <- dados_separados %>%
  filter(map_int(AU, length) != map_int(C1, length))


print(dados_com_erro)

equalize_lengths <- function(autores, instituicoes) {
  max_len <- max(length(autores), length(instituicoes))
  autores <- c(autores, rep(NA, max_len - length(autores)))
  instituicoes <- c(instituicoes, rep(NA, max_len - length(instituicoes)))
  list(AU = autores, C1 = instituicoes)
}


dados_corrigidos <- dados_separados %>%
  rowwise() %>%
  mutate(equalized = list(equalize_lengths(AU, C1))) %>%
  unnest_wider(equalized, names_sep = "_") %>%
  select(-AU, -C1) %>%
  rename(AU = equalized_AU, C1 = equalized_C1)


# Agora, expandimos as listas de autores e instituições, repetindo as demais colunas
dados_longos <- dados_corrigidos %>%
  unnest(cols = c(AU, C1))

# Ver o resultado
head(dados_longos)

view(dados_longos)

# Se precisar exportar para um novo arquivo CSV ou Excel
write_csv(dados_longos, "caminho/para/planilha_processada.csv")

# Se preferir salvar em um arquivo Excel
library(writexl)
write_xlsx(dados_longos, "caminho/para/planilha_processada.xlsx")


## LIMPAR TUDO PORQUE DEU RUIM

rm(list = ls())



##REVENDO CODIGO

dados <- read_excel("C:/Users/vitor/Downloads/OL CL meteoc.xlsx")

dados_C1 <- dados %>%
  select(-AU)

dados_AU <- dados %>%
  select(-C1)

print(dados_C1)
print (dados_AU)

dados_sep_AU <- dados_AU %>%
  mutate(
    AU = str_split(AU, ";"),
  )


dados_sep_C1 <- dados_C1 %>%
  mutate(
    C1 = str_split(C1, ";")
  )


view(dados_sep_AU)
view(dados_sep_C1)

dados_fim_AU <- dados_sep_AU %>%
  unnest(cols = c(AU))

dados_fim_C1 <- dados_sep_C1 %>%
  unnest(cols = c(C1))


view(dados_fim_AU)


## baixar planilhas

file_path_local <- "C:/Users/vitor/Downloads"


write.xlsx(dados_fim_AU, file_path_local, sheetName = "dados AU", overwrite = TRUE)
write.xlsx(dados_fim_C1, file_path_local, sheetName = "dados C1", overwrite = TRUE)



### RESOLVENDO NOMES DIFERENTES DE INSTITUIÇÕES

dados_fim_C1 <- dados_fim_C1 %>%
  mutate(
    C1 = str_split(C1, ","),
  )


view(dados_fim_C1)
head(dados_fim_C1)

