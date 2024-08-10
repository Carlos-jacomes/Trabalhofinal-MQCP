#Script do tratamento dos dados acerca do seguinte tópico: Sub-representação 
de candidaturas sob a perspectiva de raça e classe

# Carregar pacotes necessários
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Carregar os dados
file_path <- "C:/Users/User/OneDrive/Documentos/metodos/classificacao.xlsx"
dados_classificacao <- read_excel(file_path, sheet = "Câmara dos Deputados")

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

# Análise exploratória inicial
# Distribuição de candidatos por grupo racial e situação de totalização
race_election_status <- dados_classificacao %>%
  group_by(Grupo_Racial, `Situação de totalização`) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = `Situação de totalização`, values_from = Count, values_fill = list(Count = 0))

# Visualização
ggplot(dados_classificacao, aes(x = Grupo_Racial, fill = `Situação de totalização`)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  labs(title = "Eleições de 2022: Distribuição de Candidatos à Câmara dos 
Deputados por Grupo Racial e Situação de Totalização",
       x = "Grupo Racial",
       y = "Número de Candidatos")

# Distribuição de candidatos por classificação da ocupação e situação de totalização
occupation_election_status <- dados_classificacao %>%
  group_by(`Classificação da ocupação`, `Situação de totalização`) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = `Situação de totalização`, values_from = Count, values_fill = list(Count = 0))

# Visualização
ggplot(dados_classificacao, aes(x = `Classificação da ocupação`, fill = `Situação de totalização`)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  labs(title = "Distribuição de Candidatos à Câmara dos Deputados nas eleições de 
2022 por Classificação da Ocupação e Situação de Totalização",
       x = "Classificação da Ocupação",
       y = "Número de Candidatos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Análise cruzada entre grupo racial e ocupação
race_occupation_election_status <- dados_classificacao %>%
  group_by(Grupo_Racial, `Classificação da ocupação`, `Situação de totalização`) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = `Situação de totalização`, values_from = Count, values_fill = list(Count = 0))

# Verificar os valores na coluna Situação de totalização
print(table(dados_classificacao$`Situação de totalização`))

# Calcular taxas de sucesso eleitoral
success_rates <- dados_classificacao %>%
  group_by(Grupo_Racial, `Classificação da ocupação`) %>%
  summarise(Eleitos = sum(`Situação de totalização` == "Eleito por média" | `Situação de totalização` == "Eleito por QP"),
            Total = n(), .groups = 'drop') %>%
  mutate(Taxa_Sucesso = Eleitos / Total)

# Verificar os dados de sucesso calculados
print(success_rates)

# Visualizar taxas de sucesso
ggplot(success_rates, aes(x = `Classificação da ocupação`, y = Taxa_Sucesso, fill = Grupo_Racial)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Taxa de Sucesso Eleitoral por Grupo Racial e Ocupação dos 
candidatos à Câmara dos Deputados nas eleições de 2022",
       x = "Classificação da Ocupação",
       y = "Taxa de Sucesso") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))