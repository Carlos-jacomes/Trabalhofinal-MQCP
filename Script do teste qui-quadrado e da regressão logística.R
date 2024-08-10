Script do teste qui-quadrado e da regressão logística

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

#Criar uma variável indicando se o candidato foi eleito
dados_classificacao <- dados_classificacao %>%
  mutate(Eleito = ifelse(`Situação de totalização` == "Eleito por média" | `Situação de totalização` == "Eleito por QP", 1, 0))

# Verificar os níveis da classificação da ocupação
unique(dados_classificacao$`Classificação da ocupação`)

#Calcular as taxas de sucesso eleitoral por classe social
taxa_sucesso_classe <- dados_classificacao %>%
  group_by(`Classificação da ocupação`) %>%
  summarise(Eleitos = sum(Eleito),
            Total = n()) %>%
  mutate(Taxa_Sucesso = Eleitos / Total)

print(taxa_sucesso_classe)

# Visualização
ggplot(taxa_sucesso_classe, aes(x = `Classificação da ocupação`, y = Taxa_Sucesso)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Taxa de Sucesso Eleitoral por Classificação da Ocupação dos 
candidatos à Câmara dos Deputados nas eleições de 2022",
       x = "Classificação da Ocupação",
       y = "Taxa de Sucesso") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Teste de Qui-quadrado # Criar uma tabela de contigência
tabela_contingencia <- table(dados_classificacao$`Classificação da ocupação`, dados_classificacao$Eleito)
print(tabela_contingencia)

# Teste de qui-quadrado
teste_qui2 <- chisq.test(tabela_contingencia)
print(teste_qui2)

# Regressão logística
modelo_logistico <- glm(Eleito ~ `Classificação da ocupação`, data = dados_classificacao, family = binomial)
 
# Resumo do modelo
summary(modelo_logistico)

# Reajustar o modelo logístico com o novo nome da variável
modelo_logistico <- glm(Eleito ~ Classificacao_Ocupacao, data = dados_classificacao, family = binomial)

# Gerar os efeitos preditos
effects_logistic <- allEffects(modelo_logistico)

# Plotar os efeitos preditos
plot(effects_logistic)

