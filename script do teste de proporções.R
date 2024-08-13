#Script para entender se há subrepresentação de candidatos negros e pardos entre os eleitos em comparação com sua participação nas candidaturas

# Carregar pacotes necessários
library(readxl)
library(dplyr)
library(ggplot2)
library(effects)

# Carregar os dados
file_path <- "C:/Users/User/OneDrive/Documentos/metodos/classificacao.xlsx"
dados_classificacao <- read_excel(file_path, sheet = "Câmara dos Deputados")

# Renomear a variável 'Classificação da ocupação'
dados_classificacao <- dados_classificacao %>%
  rename(Classificacao_Ocupacao = `Classificação da ocupação`)

# Limpar dados
dados_classificacao <- dados_classificacao %>%
  mutate(`Situação de totalização` = ifelse(`Situação de totalização` == "#NULO#", "Não eleito", `Situação de totalização`),
         Grupo_Racial = case_when(
           `Cor/raça` %in% c("Branca", "Amarela") ~ "Brancos",
           `Cor/raça` %in% c("Preta", "Parda", "Indígena") ~ "Não Brancos",
           TRUE ~ "Outros"
         ))

# Remover entradas categorizadas como "Outros"
dados_classificacao <- dados_classificacao %>% filter(Grupo_Racial != "Outros")

#criar variável eleito
dados_classificacao <- dados_classificacao %>%
  mutate(Eleito = ifelse(`Situação de totalização` == "Eleito por média" | `Situação de totalização` == "Eleito por QP", 1, 0))

# Filtrar candidatos negros e pardos
dados_negros_pardos <- dados_classificacao %>%
  filter(`Cor/raça` %in% c("Preta", "Parda"))

# Calcular a participação nas candidaturas
total_candidatos <- nrow(dados_classificacao)
total_negros_pardos <- nrow(dados_negros_pardos)
participacao_candidaturas <- total_negros_pardos / total_candidatos

# Calcular a participação entre os eleitos
total_eleitos <- sum(dados_classificacao$Eleito)
total_eleitos_negros_pardos <- sum(dados_negros_pardos$Eleito)
participacao_eleitos <- total_eleitos_negros_pardos / total_eleitos

# Exibir as proporções
participacao_candidaturas
participacao_eleitos

# Teste de proporções
prop_test <- prop.test(c(total_eleitos_negros_pardos, total_negros_pardos - total_eleitos_negros_pardos),
                       c(total_eleitos, total_candidatos - total_eleitos))
print(prop_test)

# Criar um data frame com as proporções calculadas
proporcoes <- data.frame(
  Categoria = c("Candidatos", "Eleitos"),
  Proporcao = c(participacao_candidaturas, participacao_eleitos)
)

# Adicionar uma coluna para identificar o grupo
proporcoes$Grupo <- "Negros e Pardos"

# Visualizar as proporções com um gráfico de barras
ggplot(proporcoes, aes(x = Categoria, y = Proporcao, fill = Grupo)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Proporção de Candidatos e Eleitos Negros e Pardos",
       x = "Categoria",
       y = "Proporção") +
  theme_minimal() +
  theme(legend.position = "none")
