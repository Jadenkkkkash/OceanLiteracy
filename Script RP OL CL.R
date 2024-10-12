install.packages("openxlsx")
install.packages("dplyr")
install.packages("stringr")
install.packages("dplyr")


library(openxlsx)
library(dplyr)
library(stringr)
library(dplyr)



data <- read.xlsx("C:/Users/vitor/Downloads/dados_C1_university.xlsx")
colnames(data)


palavras_chave_OL <- c("Ocean Literacy", "Marine Education")
palavras_chave_CL <- c("Climate Literacy", "Climate Change Education")


padrao_OL <- paste0("(^|; )(", paste(palavras_chave_OL, collapse = "|"), ")(;|$)")
padrao_CL <- paste0("(^|; )(", paste(palavras_chave_CL, collapse = "|"), ")(;|$)")


data_filtrada_OL <- data %>%
  filter(str_detect(DE, padrao_OL))

data_filtrada_CL <- data %>%
  filter(str_detect(DE, padrao_CL))


output_file_path <- "C:/Users/vitor/Downloads"
write.xlsx(data_filtrada_OL, output_file_path, sheetName = "OL", overwrite = TRUE)
write.xlsx(data_filtrada_CL, output_file_path, sheetName = "CL", overwrite = TRUE)


## REALIZAR MAPAS PRA OL CL


df_unique_OL <- data_filtrada_OL %>%
  distinct(TI, RP, .keep_all = TRUE)

df_unique_CL <- data_filtrada_CL %>%
  distinct(TI, RP, .keep_all = TRUE)



country_distribution_OL <- df_unique_OL %>%
  group_by(RP) %>%
  summarise(Publications = n()) %>%
  arrange(desc(Publications))  # Ordenar por número de publicações

country_distribution_CL <- df_unique_CL %>%
  group_by(RP) %>%
  summarise(Publications = n()) %>%
  arrange(desc(Publications))  # Ordenar por número de publicações



country_distribution_OL <- country_distribution_OL %>%
  mutate(RP = case_when(
    RP == "United States" ~ "USA",
    RP == "United Kingdom" ~ "UK",
    TRUE ~ RP
  ))

country_distribution_CL <- country_distribution_CL %>%
  mutate(RP = case_when(
    RP == "United States" ~ "USA",
    TRUE ~ RP
  ))



# Visualizar os dados
print(country_distribution_OL)
print(country_distribution_CL)

view(country_distribution_OL)


# Gráfico de barras para a distribuição de publicações por país
ggplot(country_distribution, aes(x = reorder(RP, -Publications), y = Publications)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribuição Geográfica de Publicações", x = "País", y = "Número de Publicações") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


file_path_local <- "C:/Users/vitor/Downloads"
write.xlsx(country_distribution_OL, file_path_local, sheetname= "RP", overwrite = TRUE)
write.xlsx(country_distribution_CL, file_path_local, sheetname= "RP", overwrite = TRUE)



max_publications_OL <- max(country_distribution_OL$Publications, na.rm = TRUE)
max_publications_CL <- max(country_distribution_CL$Publications, na.rm = TRUE)



world_map <- map_data("world")

map_data_OL <- world_map %>%
  left_join(country_distribution_OL, by = c("region" = "RP"))

map_data_CL <- world_map %>%
  left_join(country_distribution_CL, by = c("region" = "RP"))


# Criar o mapa
ggplot(data = map_data_OL, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Publications), color = "black") +
  coord_fixed(1.3) +
  labs(title = "Distribuição Geográfica de Publicações", fill = "Número de Publicações") +
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50")+
  theme(legend.position = "bottom")

ggplot(data = map_data_CL, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Publications), color = "black") +
  coord_fixed(1.3) +
  labs(title = "Distribuição Geográfica de Publicações", fill = "Número de Publicações") +
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "darkblue", limits = c(0, max_publications+5), na.value = "grey50")+
  theme(legend.position = "bottom")

ggsave("mapa_publicacoes_alta_resolucao.png", plot = last_plot(), width = 15, height = 10, dpi = 300)

