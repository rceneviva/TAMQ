
# ============================================================
# Script Base Didático – Aula #02: RCTs e Regressão
# Curso de Pós-Graduação em Políticas Públicas
# Objetivo: Executar e interpretar exercícios com dados simulados
# ============================================================

# ============================================================
# Etapa 1: Instalar e carregar pacotes necessários
# ============================================================
# ATENÇÃO: Execute as linhas abaixo apenas uma vez para instalar os pacotes.
# Depois, mantenha-as comentadas ou remova-as se os pacotes já estiverem instalados.

# install.packages(c("ggplot2", "AER", "ivpack", "sandwich", "Matching", "stargazer", "dplyr", "readr"))

# Carregar os pacotes para uso
library(ggplot2)      # Para gráficos
library(AER)          # Para regressão com variável instrumental
library(ivpack)       # Ferramentas auxiliares para IV
library(sandwich)     # Erros padrão robustos
library(Matching)     # Estimação de ATT via pareamento
library(stargazer)    # Geração de tabelas de regressão
library(dplyr)        # Manipulação de dados
library(readr)        # Leitura de dados

# ============================================================
# Etapa 2: Regressão Linear Simples com dados simulados
# ============================================================

# Vamos simular dados simples: uma variável x e um resultado y
# Suponha: y = 2 + 3*x + erro aleatório

set.seed(123)  # Para reprodutibilidade
n <- 200       # Número de observações

x <- rnorm(n)                 # Gera x com distribuição normal padrão
erro <- rnorm(n)              # Erro aleatório também normal
y <- 2 + 3 * x + erro         # Relação linear com ruído

dados1 <- data.frame(x = x, y = y)

# Estimar modelo de regressão linear simples
modelo1 <- lm(y ~ x, data = dados1)
summary(modelo1)

# Visualizar os dados e a linha de regressão
ggplot(dados1, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Regressão Linear Simples", x = "Variável explicativa (x)", y = "Resultado (y)")

# ============================================================
# Etapa 3: Simulação de um RCT simples – ATE e ITT
# ============================================================

set.seed(456)
n <- 400
tratamento <- rbinom(n, 1, 0.5)     # Atribuição aleatória (0 = controle, 1 = tratamento)
erro <- rnorm(n, 0, 5)
Y <- 50 + 5 * tratamento + erro     # Efeito médio do tratamento = 5

dados2 <- data.frame(tratamento = tratamento, Y = Y)

# Estimar o ATE via diferença de médias
media_trat <- mean(dados2$Y[dados2$tratamento == 1])
media_ctrl <- mean(dados2$Y[dados2$tratamento == 0])
ATE <- media_trat - media_ctrl
print(paste("ATE estimado (diferença de médias):", round(ATE, 2)))

# Estimar ITT via regressão
modelo_itt <- lm(Y ~ tratamento, data = dados2)
summary(modelo_itt)

# Visualização do efeito
ggplot(dados2, aes(x = as.factor(tratamento), y = Y)) +
  geom_boxplot(fill = c("lightblue", "lightgreen")) +
  labs(x = "Grupo (0 = Controle, 1 = Tratamento)",
       y = "Resultado (Y)",
       title = "Comparação entre Grupos de Tratamento")

# ============================================================
# Etapa 4: Não conformidade – ITT vs TOT (Instrumental Variable)
# ============================================================

set.seed(789)
n <- 500
Z <- rbinom(n, 1, 0.5)  # Atribuição aleatória (instrumento)
D <- ifelse(Z == 1, rbinom(n, 1, 0.85), rbinom(n, 1, 0.10))  # Nem todos cumprem a atribuição

erro <- rnorm(n, 0, 5)
Y <- 50 + 5 * D + erro

dados3 <- data.frame(Z = Z, D = D, Y = Y)

# ITT: efeito da atribuição aleatória
modelo_itt2 <- lm(Y ~ Z, data = dados3)
summary(modelo_itt2)

# TOT: efeito do tratamento real usando IV
modelo_iv <- ivreg(Y ~ D | Z, data = dados3)
summary(modelo_iv)

# ============================================================
# Etapa 5: Tabela comparativa com stargazer
# ============================================================

stargazer(modelo1, modelo_itt, modelo_iv, type = "text",
          title = "Modelos Estimados",
          column.labels = c("Regressão Simples", "ITT", "TOT"),
          keep.stat = c("n", "rsq", "adj.rsq"))

# ============================================================
# Fim do Script Base
# ============================================================
