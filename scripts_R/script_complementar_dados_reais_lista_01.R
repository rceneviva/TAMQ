
# ============================================================
# Script Complementar – Análise com Dados Reais (Aula #02)
# Curso de Pós-Graduação em Políticas Públicas
# Objetivo: Praticar regressão e inferência causal com dados reais
# ============================================================

# Carregar pacotes necessários
library(ggplot2)
library(AER)
library(ivpack)
library(sandwich)
library(Matching)
library(stargazer)
library(dplyr)
library(readr)

# ============================================================
# Exercício 5 – Bolsa Família e Desempenho Escolar
# Dataset disponível em: https://www.kaggle.com/datasets/alexandrejose/brazil-bolsa-familia-education
# ============================================================

# Passo 1: Baixe o arquivo .csv manualmente do link acima (Kaggle)
# Passo 2: Após baixar o arquivo, salve-o no seu diretório de trabalho e atualize o caminho abaixo

# Caminho do arquivo (substitua conforme o local onde salvou o arquivo)
# Exemplo: "C:/Users/seunome/Downloads/bolsa_familia.csv"
caminho_arquivo <- "bolsa_familia.csv"

# Ler os dados
dados_bf <- read_csv(caminho_arquivo)

# Verificar as primeiras linhas
head(dados_bf)

# Selecionar variáveis relevantes
dados_bf <- dados_bf %>%
  filter(idade >= 6 & idade <= 14) %>%
  select(aprovado, bolsafamilia, idade, sexo, escola_id)

# Estimar regressão simples e com controles
modelo_bf1 <- lm(aprovado ~ bolsafamilia, data = dados_bf)
modelo_bf2 <- lm(aprovado ~ bolsafamilia + idade + sexo, data = dados_bf)

# Comparar os dois modelos
stargazer(modelo_bf1, modelo_bf2, type = "text",
          column.labels = c("Sem Controles", "Com Controles"),
          title = "Impacto do Bolsa Família sobre Aprovação Escolar")

# Visualização
ggplot(dados_bf, aes(x = as.factor(bolsafamilia), y = aprovado)) +
  geom_boxplot() +
  labs(x = "Recebe Bolsa Família", y = "Taxa de Aprovação",
       title = "Desempenho Escolar por Grupo")

# ============================================================
# Exercício 7 – ATT com Pareamento (dados wage1)
# Dataset disponível no pacote wooldridge
# ============================================================

# Instalar pacote caso ainda não tenha
# install.packages("wooldridge")
library(wooldridge)

# Carregar base de dados wage1
data("wage1", package = "wooldridge")
dados_wage <- wage1

# Verificar dados
head(dados_wage)

# ATT: comparar quem concluiu ensino médio (hsgrad == 1) com pareamento
att_out <- Match(Y = dados_wage$wage, Tr = dados_wage$hsgrad,
                 X = dados_wage$exper, M = 1)

summary(att_out)

# Comparar com regressão múltipla
modelo_att <- lm(wage ~ hsgrad + exper + tenure + age, data = dados_wage)
summary(modelo_att)

# ============================================================
# Exercício 9 – IV com Educação e Salário (Angrist & Krueger)
# Arquivo: https://economics.mit.edu/files/379
# ============================================================

# Passo 1: Baixe o arquivo AK91.dta e converta para CSV, ou use haven::read_dta()
# Exemplo abaixo com CSV
# caminho_ak <- "AK91.csv"

# Exemplo com haven (descomente se preferir)
# install.packages("haven")
# library(haven)
# dados_ak <- read_dta("AK91.dta")

# Exemplo genérico:
# dados_ak <- read_csv(caminho_ak)

# Digamos que dados_ak já contenha as variáveis: wage, education, qob (quarter of birth)
# Estimar OLS
# modelo_ols <- lm(log(wage) ~ education, data = dados_ak)
# summary(modelo_ols)

# Estimar IV com quarter of birth como instrumento
# modelo_iv_ak <- ivreg(log(wage) ~ education | qob, data = dados_ak)
# summary(modelo_iv_ak)

# Comparar robustez
# library(lmtest)
# coeftest(modelo_iv_ak, vcov. = vcovHC(modelo_iv_ak, type = "HC1"))

# ============================================================
# Fim do Script Complementar
# ============================================================
