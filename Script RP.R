install.packages("ggplot2")
install.packages("dplyr")
install.packages("maps")
install.packages("ggmap")
install.packages("openxlsx")


library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)
library(openxlsx)

df <- read.xlsx("C:/Users/UFES/Downloads/dados_C1_university.xlsx")


df_unique <- df %>%
  distinct(TI, RP, .keep_all = TRUE)



country_distribution <- df_unique %>%
  group_by(RP) %>%
  summarise(Publications = n()) %>%
  arrange(desc(Publications))  # Ordenar por número de publicações


country_distribution <- country_distribution %>%
  mutate(RP = case_when(
    RP == "United States" ~ "USA",
## RP == "United Kingdom" ~ "UK", ver se usa isso
    TRUE ~ RP
  ))

# Visualizar os dados
print(country_distribution)
view(country_distribution)


# Gráfico de barras para a distribuição de publicações por país
ggplot(country_distribution, aes(x = reorder(RP, -Publications), y = Publications)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribuição Geográfica de Publicações", x = "País", y = "Número de Publicações") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


write.xlsx(country_distribution, file_path_local, sheetname= "RP", overwrite = TRUE)

max_publications <- max(country_distribution$Publications, na.rm = TRUE)


world_map <- map_data("world")

map_data <- world_map %>%
  left_join(country_distribution, by = c("region" = "RP"))


# Criar o mapa
ggplot(data = map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Publications), color = "black") +
  coord_fixed(1.3) +
  labs(title = "Distribuição Geográfica de Publicações", fill = "Número de Publicações") +
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "darkblue", limits = c(0, max_publications+5), na.value = "grey50")+
  theme(legend.position = "bottom")

ggsave("mapa_publicacoes_alta_resolucao.png", plot = last_plot(), width = 15, height = 10, dpi = 300)

