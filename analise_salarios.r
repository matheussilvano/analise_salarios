# analise_salarios.R

# Pacotes necessários
library(dplyr)
library(ggplot2)

# Leitura da base de dados
base <- read.csv("dados_salarios.csv")

# 1) Amostra
set.seed(19092005)  # Dia, mês e ano de nascimento
base1 <- base[sample(nrow(base), 300), ]

# 2) Estatísticas descritivas da variável salario_USD
estatisticas_salario <- base1 %>%
  summarise(
    media = mean(salario_USD, na.rm = TRUE),
    mediana = median(salario_USD, na.rm = TRUE),
    percentil_5 = quantile(salario_USD, 0.05, na.rm = TRUE),
    percentil_25 = quantile(salario_USD, 0.25, na.rm = TRUE),
    percentil_75 = quantile(salario_USD, 0.75, na.rm = TRUE),
    percentil_95 = quantile(salario_USD, 0.95, na.rm = TRUE),
    minimo = min(salario_USD, na.rm = TRUE),
    maximo = max(salario_USD, na.rm = TRUE)
  )

print("Estatísticas descritivas de salario_USD:")
print(estatisticas_salario)

# 3) Média e mediana por ano
salario_por_ano <- base1 %>%
  group_by(ano) %>%
  summarise(
    media = mean(salario_USD, na.rm = TRUE),
    mediana = median(salario_USD, na.rm = TRUE)
  )

print("Média e mediana de salario_USD por ano:")
print(salario_por_ano)

# 4) Análise por experiência (agrupando SE + EX)
base1 <- base1 %>%
  mutate(experiencia_mod = ifelse(experiencia %in% c("SE", "EX"), "SE_EX", experiencia))

salario_por_experiencia <- base1 %>%
  group_by(experiencia_mod) %>%
  summarise(
    media = mean(salario_USD, na.rm = TRUE),
    mediana = median(salario_USD, na.rm = TRUE)
  )

print("Média e mediana de salario_USD por experiência (com SE + EX juntos):")
print(salario_por_experiencia)

# 5) Gráfico com as medianas por experiência
ggplot(salario_por_experiencia, aes(x = experiencia_mod, y = mediana)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Mediana de salário por nível de experiência",
    x = "Nível de Experiência",
    y = "Mediana do Salário (USD)"
  ) +
  theme_minimal()
