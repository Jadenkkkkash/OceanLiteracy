install.packages("tidyverse")
install.packages("readxl")
install.packages("openxlsx")


library(tidyverse)
library(readxl)
library(openxlsx)

df<- read_excel("C:/Users/UFES/Downloads/dados_C1_university.xlsx")
file_path_local <- "C:/Users/UFES/Downloads"

head(df)
view(df)

library(dplyr)


df <- df %>% arrange(TI)

print(df)

df$iguais_consecutivos <- c(FALSE, df$TI[-1] == df$TI[-nrow(df)] & df$Nome_Universidade[-1] == df$Nome_Universidade[-nrow(df)])

df_limpo <- df %>% filter(!df$iguais_consecutivos)

view(df_limpo)

write.xlsx(df_limpo, file_path_local, sheetName = "dados TI", overwrite = TRUE)
