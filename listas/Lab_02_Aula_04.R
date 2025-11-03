
################################################################################
# Lista de Exercícios 03 – Resolução completa em R
# Curso: Tópicos Especiais de Métodos Quantitativos (UFABC)
# Conteúdo: Simulação, Regressão Linear Múltipla, Amostragem de Survey,
#           Dados Longitudinais, Diferenças‑em‑Diferenças e Teste de Hausman
# Autor: (insira seu nome)
# Data: (atualize quando rodar)
################################################################################

# ------------------------------
# Pacotes necessários
# ------------------------------
# ggplot2 -> gráficos
# stargazer -> tabelas de regressão (atenção: 'stargazer', não 'starrgazer')
# survey  -> análise com pesos de desenho
# fixest  -> modelos lineares com efeitos fixos
# plm     -> painel com FE/RE + teste de Hausman
# did     -> estimador Callaway & Sant’Anna
# lme4    -> curvas de crescimento (efeitos aleatórios)
# car     -> diagnóstico VIF
# tidyverse -> data wrangling

if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("survey", quietly = TRUE))   install.packages("survey")
if (!requireNamespace("fixest", quietly = TRUE))   install.packages("fixest")
if (!requireNamespace("plm", quietly = TRUE))      install.packages("plm")
if (!requireNamespace("did", quietly = TRUE))      install.packages("did")
if (!requireNamespace("lme4", quietly = TRUE))     install.packages("lme4")
if (!requireNamespace("stargazer", quietly = TRUE))install.packages("stargazer")
if (!requireNamespace("car", quietly = TRUE))      install.packages("car")

library(tidyverse)
library(survey)
library(fixest)
library(plm)
library(did)
library(lme4)
library(stargazer)
library(car)
library(ggplot2)

# Reprodutibilidade geral
set.seed(12345)

################################################################################
# Exercício 1 – População sintética e regressão linear múltipla
################################################################################
N <- 10000
pop <- tibble(
  sexo         = rbinom(N, 1, 0.5),
  escolaridade = rnorm(N, 12, 3),
  experiencia  = pmax(rnorm(N, 10, 5), 0),
  erro         = rnorm(N, 0, 4)
) |> mutate(
  salario = 800 + 150 * escolaridade + 60 * experiencia - 120 * sexo + erro
)

mod_pop <- lm(salario ~ sexo + escolaridade + experiencia, data = pop)
stargazer(mod_pop, type = "text", title = "Exercício 1 – OLS População" )

# Diagnósticos
ggplot(pop, aes(sample = resid(mod_pop))) + stat_qq() + stat_qq_line()
ggplot(pop, aes(x = fitted(mod_pop), y = resid(mod_pop))) +
  geom_point(alpha = .3) + geom_hline(yintercept = 0, lty = 2)
print(car::vif(mod_pop))

################################################################################
# Exercício 2 – Amostragem estratificada
################################################################################
n <- 1200
amostra <- pop |> group_by(sexo) |> sample_frac(size = n/N) |> ungroup()

Nh <- pop |> count(sexo, name = "Nh")
nh <- amostra |> count(sexo, name = "nh")
pesos <- amostra |> left_join(Nh, by = "sexo") |> left_join(nh, by = "sexo") |>
  mutate(peso = Nh/nh)

desenho <- svydesign(ids = ~1, strata = ~sexo, weights = ~peso, data = pesos)
print(svymean(~salario, desenho))
print(mean(pop$salario))

################################################################################
# Exercício 3 – Regressão ponderada survey
################################################################################
mod_survey <- svyglm(salario ~ sexo + escolaridade + experiencia, design = desenho)
stargazer(mod_pop, mod_survey, type="text", column.labels = c("Pop", "Survey"))

################################################################################
# Exercício 4 – Painel, FE, RE, Hausman
################################################################################
N_id <- 1000; T_per <- 5
panel <- expand_grid(id = 1:N_id, t = 1:T_per) |>
  mutate(alpha_i = rnorm(N_id)[id],
         beta_t  = 5 * t,
         grupo   = rbinom(N_id, 1, 0.5)[id],
         D       = ifelse(grupo==1 & t>=3, 1, 0),
         u_it    = rnorm(n(),0,4),
         Y       = 200 + alpha_i + beta_t + 25*D + u_it)

mod_FE <- feols(Y ~ D | id + t, data = panel)
summary(mod_FE)

panel_p <- pdata.frame(panel, index = c("id","t"))
mod_RE <- plm(Y ~ D + factor(t), data = panel_p, model = "random")
summary(mod_RE)

mod_FE_plm <- plm(Y ~ D + factor(t), data = panel_p, model = "within")
print(phtest(mod_FE_plm, mod_RE))

mod_lmer <- lmer(Y ~ t + D + (t|id), data = panel, REML = FALSE)
summary(mod_lmer)

################################################################################
# Exercício 5 – DiD escalonado
################################################################################
panel <- panel |> group_by(id) |>
  mutate(g = ifelse(any(D==1), min(t[D==1]), 9999)) |> ungroup()

att <- att_gt(yname="Y", tname="t", idname="id", gname="g",
              xformla=~1, data=panel)
summary(att)
ggdid(att)
