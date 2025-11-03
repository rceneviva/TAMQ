
# Aula #02 - Laboratório: RCTs e Regressão
# Objetivo: simular um experimento aleatorizado (RCT), estimar diferença de médias e usar regressão linear

# 1. Configuração inicial
set.seed(123)  # Reprodutibilidade
n <- 200       # Tamanho da amostra

# 2. Gerando dados simulados
# Variável de tratamento: alocação aleatória
tratamento <- rbinom(n, 1, 0.5)

# Variável de controle: nota inicial (baseline)
nota_inicial <- rnorm(n, mean = 50, sd = 10)

# Efeito do tratamento verdadeiro
efeito_tratamento <- 5

# Variável de resultado (nota final)
# Apenas os tratados recebem o efeito adicional
nota_final <- 50 + 0.6 * nota_inicial + efeito_tratamento * tratamento + rnorm(n, mean = 0, sd = 5)

# 3. Criando o data.frame
dados <- data.frame(
  tratamento = tratamento,
  nota_inicial = nota_inicial,
  nota_final = nota_final
)

# 4. Análise Descritiva
summary(dados)
table(dados$tratamento)

# 5. Diferença de médias (estimador não ajustado)
mean(nota_final[tratamento == 1]) - mean(nota_final[tratamento == 0])

# 6. Regressão linear simples: nota_final ~ tratamento
modelo1 <- lm(nota_final ~ tratamento, data = dados)
summary(modelo1)

# 7. Regressão linear com controle por nota_inicial
modelo2 <- lm(nota_final ~ tratamento + nota_inicial, data = dados)
summary(modelo2)

# 8. Visualização
library(ggplot2)

ggplot(dados, aes(x = tratamento, y = nota_final)) +
  geom_boxplot(fill = c("lightblue", "lightgreen")) +
  labs(x = "Grupo (0 = controle, 1 = tratamento)", y = "Nota Final", title = "Distribuição da Nota Final por Grupo") +
  theme_minimal()

# 9. Conclusão
# Compare os coeficientes dos modelos 1 e 2.
# O que muda quando controlamos pela nota_inicial?

Install R (>= 3.4.0) on your system. For Windows users, Writing R Path to the registry is recommended in the installation.

Install languageserver in R.

install.packages("languageserver")