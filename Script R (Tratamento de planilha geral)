install.packages("dplyr")
install.packages("stringr")
install.packages("openxlsx")


library(dplyr)
library(stringr)
library(openxlsx)

file_path <- "C:/Users/DELL/Downloads/OL CL aula meteoc.xlsx"
  file_path_local <- "C:/Users/DELL/Downloads"
  df <- read.xlsx(file_path, sheet = 1)


# Criando uma nova coluna com a parte da string após a última vírgula
df <- df %>%
  mutate(apos_ultima_virgula = str_trim(str_extract(RP, "[^,]+$")))

#apagar tudo após ;
df <- df %>%
  mutate(frase_limpa = str_extract(apos_ultima_virgula, "^[^;]*"))

#primeira letra da palavra em capslock
df <- df %>%
  mutate(frase_limpa = str_to_title(tolower(frase_limpa)))




write.xlsx(df, file_path_local, sheetName = "tabela com países", overwrite = TRUE)


# PRIMEIRO PASSO PARA CONSEGUIR UNIVERSIDADE, INSTITUTO ETC
df$local <- sapply(df$RP, function(x) {
  if (grepl(";", x)) {
    trimws(sub("^[^;]*;", "", x))
  } else {NA
  }
})



# apagar após a 2 virgula

df$instituto <- sapply(df$local, function(x) {
  if (lengths(gregexpr(",", x)) >= 2) {
    sub("^(([^,]*,){2}).*", "\\1", x)
  } else {
    x
  }
})


# apagar segunda virgula

df$instituto <- sapply(df$instituto, function(x) {
  if (lengths(gregexpr(",", x)) >= 2) {
    gsub("(.*?,.*?),", "\\1", x)
  } else {
    x
  }
})





# universidade
df$universidade <- sapply(df$local, function(x) {
  if (grepl(",", x)) {
    trimws(sub("^[^,]*,", "", x))
  } else {NA
  }
})

# retirar itens após universidade
df$universidade <- sapply(df$universidade, function(x) {
  if (grepl(",", x)) {
    trimws(sub(",.*$", "", x))
  } else {x
  }
})





# Exibir o resultado
print(df)



write.xlsx(df, file_path_local, sheetName = "com instituições", overwrite = TRUE)

