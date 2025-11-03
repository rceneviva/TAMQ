# ===============================================================
# UFABC – Programa de Pós‑graduação em Políticas Públicas (PPGPP)
# Tópicos Anvaçados de Métodos Quantitativos (TAMQ)
# Lista de Exercícios n.º 02 – Gabarito / Script de Resolução no R
# Autor: Ricardo Ceneviva 
# Data: 08‑Jul‑2025
# ---------------------------------------------------------------


# Este script resolve passo a passo os quatro blocos da Lista 02:
# 1) Regressão Multivariada
# 2) Metodologia de Surveys
# 3) Estudos Longitudinais
# 4) Indicadores & Medidas Sintéticas

# ---------------------------------------------------------------

# • Comentários explicam cada bloco de código.
# • set.seed garante reprodutibilidade.
# • Gráficos: ggplot2
# * Tabelas: stargazer
# • Dados reais baixados via APIs ou download helpers.
# • Script compatível com Windows, macOS e Linux.
# ===============================================================


# ---------------------------------------------------------------
# 0. Pacotes -----------------------------------------------------
# ---------------------------------------------------------------


# Função utilitária para instalar (se necessário) e carregar pacotes
pkg <- c("tidyverse",   # wrangling & ggplot2
         "car",         # VIF & BP test
         "lmtest",      # Breusch–Pagan test
         "sandwich",    # robust SEs
         "stargazer",   # export tables
         "survey",      # complex survey designs
         "plm",         # panel models
         "fixest",      # fast FE/RE & robust
         "did",         # staggered DiD (Sun & Abraham)
         "psych",       # PCA utilities
         "basedosdados" # wrapper for basedosdados (alt.)
)
install_if_missing <- function(pkgs){
  for(p in pkgs){ if(!requireNamespace(p, quietly = TRUE)) install.packages(p) }
}
install_if_missing(pkg)
# Carregar todos de uma vez
lapply(pkg, library, character.only = TRUE)

# Para APIs (basedosdados) recomenda‑se configurar token BD_API_KEY em .Renviron
# e reiniciar sessão. Exemplo:
# usethis::edit_r_environ()  # adicione BD_API_KEY="sua‑chave‑secreta"

# Definir tema ggplot2 global para gráficos clean
theme_set(theme_minimal())


# ---------------------------------------------------------------
# 1. Regressão Multivariada -------------------------------------
# ---------------------------------------------------------------
set.seed(12345)
# ---------- (B1) População sintética ---------------------------
N <- 10000
pop <- tibble(
  escolaridade = rpois(N, lambda = 9),          # anos de estudo ≈ 0‑20
  experiencia  = pmax(rpois(N, 15) - 5, 0),     # anos de experiência
  sexo         = sample(c("M", "F"), N, replace = TRUE),
  u            = rnorm(N, 0, 500)               # erro idiossincrático
) %>% 
  mutate(salario = 800 + 450*escolaridade + 90*experiencia + if_else(sexo=="M",400,0) + u)


# Ajustar modelo OLS simples
# modelos simples: salário e escolaridade
mod_01 <- lm(salario ~ escolaridade, data = pop)
summary(mod_01)

# modelos salario como função da escolaridade e experiencia
mod_02 <- lm(salario ~ escolaridade + experiencia, data = pop)
summary(mod_02)

# modelo completo com todas as variáveis explicativas
mod_pop <- lm(salario ~ escolaridade + experiencia + sexo, data = pop)
summary(mod_pop)

# Tabela formato relatório/artigo:

stargazer(mod_01, mod_02, mod_pop, 
          type = "text", 
          title = "Exercício 1 – OLS População")


# ---------- (B2) Diagnósticos fundamentais ---------------------
# QQ‑plot
ggplot(pop, aes(sample = resid(mod_pop))) +
  stat_qq() + stat_qq_line() +
  labs(title = "QQ‑plot dos resíduos – População sintética")

# Resíduos vs Ajustados
ggplot(pop, aes(x = fitted(mod_pop), y = resid(mod_pop))) +
  geom_point(alpha = .3) + geom_hline(yintercept = 0, lty = 2) +
  labs(title = "Resíduos vs Valores Ajustados")

# Heterocedasticidade (Breusch–Pagan)
lmtest::bptest(mod_pop)  # p‑valor > 0?  se <0.05 => hetero

# Multicolinearidade (VIF)
car::vif(mod_pop)


# ---------- (I1) Interação & não‑linearidade -------------------
mod_int <- lm(salario ~ escolaridade*sexo + experiencia + I(experiencia^2), data = pop)

stargazer(mod_01, mod_02, mod_pop, mod_int,
          type = "text", 
          title = "Exercício 1, modelo c/ Interação e termo quadrático") 



# ---------- (I2) Erros robustos (HC3) --------------------------
coeftest(mod_int, vcov = vcovHC(mod_int, type = "HC3"))


# ---------- (A) Dados reais – PNAD‑C 2024‑T4 -------------------
# Exemplo de pipeline (executar somente se possuir chave BD)
# Note: pode levar tempo & memória; adapte o filtro conforme necessidade.
# if(Sys.getenv("BD_API_KEY")!=""){
#   library(basedosdados)
#   pnad <- read_sql("SELECT ano, trimestre, sexo, idade, anos_estudo, renda_do_trabalho_principal
#                     FROM `basedosdados.br_ibge_pnadc.microdados`
#                     WHERE ano = 2024 AND trimestre = 4 LIMIT 100000")
#   pnad <- pnad %>% filter(renda_do_trabalho_principal > 0)
#   pnad <- pnad %>% mutate(sexo = if_else(sexo==1, "M","F"))
#   mod_pnad <- svyglm(renda_do_trabalho_principal ~ anos_estudo + idade + sexo,
#                      design = svydesign(ids = ~1, weights = ~1, data = pnad))
#   summary(mod_pnad)
# }



# ---------------------------------------------------------------
# 2. Metodologia de Surveys -------------------------------------
# ---------------------------------------------------------------




set.seed(12345)
# ---------- (B1) População & amostra simulada ------------------
N_pop <- 20000
pop_svy <- tibble(
  renda = exp(rnorm(N_pop, 8, .5)),           # distribuição log‑normal
  idade = sample(18:80, N_pop, TRUE),
  sexo  = sample(c("M","F"), N_pop, TRUE, prob=c(.47,.53))
) %>%
  mutate(satisfacao = sample(1:5, N_pop, TRUE, prob = c(.05,.15,.3,.35,.15)))

# Amostra estratificada
n <- 1200
strata_n <- prop.table(table(pop_svy$sexo))*n
set.seed(12345)
idx_F <- sample(which(pop_svy$sexo=="F"), size = strata_n["F"])
idx_M <- sample(which(pop_svy$sexo=="M"), size = strata_n["M"])

amostra <- pop_svy[c(idx_F, idx_M), ]
# pesos = N_pop / n no estrato
amostra <- amostra %>%
  group_by(sexo) %>%
  mutate(peso = n()/n(),  # placeholder
         peso = (sum(sexo==sexo[1]))/n()) %>% ungroup()

# Design survey
svy_des <- svydesign(ids = ~1, strata = ~sexo, weights = ~peso, data = amostra)

# ---------- (B2) Média ponderada -------------------------------
svymean(~satisfacao, svy_des)
mean(amostra$satisfacao) # não ponderada

# ---------- (I1) Regressão com pesos ---------------------------
mod_svy <- svyglm(satisfacao ~ log(renda) + idade + sexo, design = svy_des)
stargazer(mod_svy, type = "text", title = "Regressão – Satisfação (design ponderado)")

# ---------- (I2) Pós‑stratificação (raking) --------------------
# metas populacionais
pop_totals <- data.frame(sexo = c("F","M"), Freq = c(0.55, 0.45))
svy_raked <- rake(design = svy_des, sample.margins = list(~sexo), population.margins = list(pop_totals))
svymean(~satisfacao, svy_raked)

# ---------- (A) Dados reais – LAPOP 2023 -----------------------
# Placeholder: indicar como baixar e exemplo de leitura
# lapop <- haven::read_dta("Brazil_2023.dta")
# lapop_des <- svydesign(ids=~psu, strata=~stratum, weights=~weight, data=lapop)
# svyby(~conf_congreso, ~quintil_renda, lapop_des, svymean)



# ---------------------------------------------------------------
# 3. Estudos Longitudinais -------------------------------------
# ---------------------------------------------------------------
set.seed(12345)
# ---------- (B1) Painel sintético ------------------------------
T_per <- 6
n_id  <- 1200
ids   <- 1:n_id
panel <- expand_grid(id = ids, t = 1:T_per) %>%
  mutate(alpha = rnorm(n_id)[id],              # efeito fixo individual
         gamma = rnorm(T_per, 0, 50)[t])      # choques sazonais

# Atribuir tratamento (40% a partir t>=4)
trt_ids <- sample(ids, size = 0.4*n_id)
panel <- panel %>%
  mutate(D = if_else(id %in% trt_ids & t>=4, 1, 0),
         u = rnorm(n()),
         Y = 1500 + alpha*200 + gamma + 250*D + u)

# ---------- (B2) Efeitos fixos -------------------------------
fe_mod <- feols(Y ~ D | id + t, data = panel)
summary(fe_mod)

# ---------- (I1) Hausman -------------------------------------
re_mod <- plm(Y ~ D, data = panel, index = c("id","t"), model = "random")
hausman <- phtest(re_mod, plm(fe_mod))
hausman # p‑valor pequeno => FE preferível

# ---------- (I2) DiD escalonado ------------------------------
diD <- did::att_gt(yname = "Y", tname = "t", idname = "id", gname = "D",
                   data = panel)
summary(diD)
# Gráfico de trajetórias ATT
agg <- aggte(diD, type = "dynamic")
plot(agg)

# ---------- (A) Dados reais – CAGED --------------------------
# Exemplo (demorado):
# if(Sys.getenv("BD_API_KEY")!=""){
#   caged <- read_sql("SELECT ano, mes, municipio, saldo_emprego
#                      FROM `basedosdados.br_me_caged.municipio_mes`")
#   # construir variável de tratamento e painel, depois FE ou did::att_gt
# }



# ---------------------------------------------------------------
# 4. Indicadores & Medidas Sintéticas ---------------------------
# ---------------------------------------------------------------
set.seed(12345)
Mun <- 500
muni <- tibble(
  id_mun = 1:Mun,
  renda_pc = rnorm(Mun, 1000, 300),
  analf   = rnorm(Mun, 8, 3),
  saneam  = rnorm(Mun, 70, 10),
  mort_inf= rnorm(Mun, 15, 4),
  desemp  = rnorm(Mun, 12, 3)
) %>%
  mutate(across(-id_mun, scale))  # padronizar z‑score



# ---------- (B2) Índice simples -------------------------------
muni <- muni %>% mutate(IMVS = rowMeans(across(renda_pc:desemp)))

# Quintis
muni <- muni %>% mutate(quintil = ntile(IMVS, 5))

# ---------- (I1) PCA -----------------------------------------
pca <- psych::principal(muni %>% select(renda_pc:desemp), nfactors = 1, rotate = "none")
muni <- muni %>% mutate(IMVS_PCA = as.numeric(scale(pca$scores)))

# Comparação densidades
bind_rows(
  muni %>% select(IMVS) %>% mutate(tipo = "Média") ,
  muni %>% select(IMVS_PCA) %>% rename(IMVS = IMVS_PCA) %>% mutate(tipo = "PCA")
) %>%
  ggplot(aes(x = IMVS, fill = tipo)) +
  geom_density(alpha = .4) + labs(title = "Distribuição IMVS – simples vs PCA")

# ---------- (I2) Validação externa ----------------------------
# Simular homicídios correlacionados
muni <- muni %>% mutate(homic = IMVS_PCA*0.7 + rnorm(Mun))
cor.test(muni$IMVS_PCA, muni$homic)

# ---------- (A) Dados reais – IDHM & Censo --------------------
# download.file("https://www.ipea.gov.br/idh/download/IDHM_Municipios.csv", "idhm.csv")
# idhm <- readr::read_csv("idhm.csv")
# ... seguir etapas para construir IMVS_PCA_2010 & 2022




# ---------------------------------------------------------------
# 5. Sessão e exportação ---------------------------------------
# ---------------------------------------------------------------
# Salvar sumário de sessão para reprodutibilidade
sink("session_info.txt")
sessionInfo()
sink()

# Fim do script -------------------------------------------------
