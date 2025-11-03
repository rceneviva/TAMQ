
# Aula #02 - Laboratório: RCTs e Regressão (Versão Estendida)
# Objetivo: simular um RCT, estimar efeitos médios, heterogeneidade, ITT, TOT, ATE e ATT

# 1. Configuração inicial
set.seed(123)
n <- 500

# 2. Geração de dados
# Covariável: nota inicial
nota_inicial <- rnorm(n, mean = 50, sd = 10)

# Heterogeneidade: nível de proficiência (baixo ou alto)
proficiencia_alta <- ifelse(nota_inicial > 50, 1, 0)

# Atribuição aleatória ao tratamento
Z <- rbinom(n, 1, 0.5)  # Random assignment (instrumento)

# Não conformidade: nem todos os sorteados recebem o tratamento
recebeu_tratamento <- ifelse(Z == 1, rbinom(n, 1, 0.85), rbinom(n, 1, 0.10))

# Efeitos heterogêneos: efeito maior para proficiência alta
efeito_base <- 3
efeito_hetero <- 4  # efeito adicional para alunos com proficiência alta

# Gerar resultado (nota final)
nota_final <- 45 + 0.5 * nota_inicial + 
              efeito_base * recebeu_tratamento +
              efeito_hetero * (recebeu_tratamento * proficiencia_alta) +
              rnorm(n, 0, 5)

# 3. Criar data.frame
dados <- data.frame(
  Z = Z,
  tratamento = recebeu_tratamento,
  nota_inicial = nota_inicial,
  proficiencia_alta = proficiencia_alta,
  nota_final = nota_final
)

# 4. Estimativa ITT (intention-to-treat)
itt_model <- lm(nota_final ~ Z, data = dados)
summary(itt_model)

# 5. Estimativa TOT via variável instrumental
# Estimação de TOT (compliance-adjusted effect)
# Etapa 1: prever tratamento a partir do instrumento Z
library(AER)
iv_model <- ivreg(nota_final ~ tratamento | Z, data = dados)
summary(iv_model)

# 6. Regressão com efeitos heterogêneos
het_model <- lm(nota_final ~ tratamento * proficiencia_alta + nota_inicial, data = dados)
summary(het_model)

# 7. Estimar ATE e ATT
# ATE estimado diretamente do modelo com tratamento
ate_model <- lm(nota_final ~ tratamento + nota_inicial, data = dados)
summary(ate_model)

# ATT: média do efeito entre os tratados
library(Matching)
att_out <- Match(Y = dados$nota_final, Tr = dados$tratamento, X = dados$nota_inicial, M = 1)
summary(att_out)

# 8. Visualização: efeitos heterogêneos
library(ggplot2)

ggplot(dados, aes(x = as.factor(tratamento), y = nota_final, fill = as.factor(proficiencia_alta))) +
  geom_boxplot() +
  labs(x = "Tratamento", y = "Nota Final", fill = "Proficiência Alta",
       title = "Efeito Heterogêneo do Tratamento por Proficiência") +
  theme_minimal()

# 9. Conclusão
# Compare ITT vs TOT, avalie presença de heterogeneidade e discuta diferenças entre ATE e ATT.
