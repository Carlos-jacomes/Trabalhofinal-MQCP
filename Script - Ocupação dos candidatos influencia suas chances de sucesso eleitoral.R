#Script para testar a hipótese de que a ocupação dos candidatos influencia suas chances de sucesso eleitoral

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

#Calcular as Taxas de Sucesso Eleitoral por Ocupação
taxa_sucesso_ocupacao <- dados_classificacao %>%
  group_by(Classificacao_Ocupacao) %>%
  summarise(Eleitos = sum(Eleito),
            Total = n()) %>%
  mutate(Taxa_Sucesso = Eleitos / Total)

print(taxa_sucesso_ocupacao)

#Testar a Associação Utilizando uma Regressão Logística
# Reajustar o modelo logístico com o novo nome da variável Classificacao_Ocupacao
modelo_logistico <- glm(Eleito ~ Classificacao_Ocupacao, data = dados_classificacao, family = binomial)

# Resumo do modelo logístico
summary(modelo_logistico)

# Visualizar as taxas de sucesso por ocupação
ggplot(taxa_sucesso_ocupacao, aes(x = Classificacao_Ocupacao, y = Taxa_Sucesso)) +
  geom_bar(stat = "identity", position = "dodge", fill = "black", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Taxa de Sucesso Eleitoral por Ocupação dos candidatos à Câmara dos Deputados nas eleições de 2022",
       x = "Ocupação",
       y = "Taxa de Sucesso (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

