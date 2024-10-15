install.packages("dplyr")
install.packages("tibble")


library(openxlsx)
library (dplyr)
library(tibble)

df_CL <- read.xlsx ("C:/Users/UFES/Downloads/CL_dados_C1_university.xlsx")
df_OL <- read.xlsx ("C:/Users/UFES/Downloads/OL_dados_C1_university.xlsx")

contagem_universidades_CL <- df_CL %>%
  group_by(Nome_Universidade) %>%
  summarise(Quantidade = n()) %>%
  arrange(desc(Quantidade))

contagem_universidades_OL <- df_OL %>%
  group_by(Nome_Universidade) %>%
  summarise(Quantidade = n()) %>%
  arrange(desc(Quantidade))

View(contagem_universidades_OL)
View(contagem_universidades_CL)

file_path_local <- "C:/Users/UFES/Downloads"
write.xlsx(contagem_universidades_CL, file_path_local, sheetname= "contCL", overwrite = TRUE)
write.xlsx(contagem_universidades_OL, file_path_local, sheetname= "contCL", overwrite = TRUE)


## interseção entre as universidades
universidades_comuns <- intersect(contagem_universidades_CL$Nome_Universidade, contagem_universidades_OL$Nome_Universidade)



universidades_comuns <- tibble(universidades_comuns)



View (universidades_comuns)



