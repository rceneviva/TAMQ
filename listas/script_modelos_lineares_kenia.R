
# -------------------------------------------
# Script Base: Modelos Lineares em R
# Estudo de Caso: Distribuição de Livros Didáticos no Quênia
# Base: kenya_books_sim.csv (simulada)
# -------------------------------------------

# 1. Instalar (se necessário) e carregar os pacotes
# Esses pacotes serão utilizados para análise estatística e visualização
pacotes <- c("tidyverse", "car", "lmtest", "sandwich", "ggplot2", "AER", "glmnet")
pacotes_instalar <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]
if(length(pacotes_instalar)) install.packages(pacotes_instalar)
lapply(pacotes, library, character.only = TRUE)

# 2. Carregar a base de dados simulada
# Certifique-se de que o arquivo 'kenya_books_sim.csv' está no diretório de trabalho
dados <- read.csv("kenya_books_sim.csv")

# 3. Visualizar as primeiras linhas do dataset
head(dados)

# 4. Verificar a estrutura das variáveis
# Isso ajuda a identificar se alguma variável foi importada com classe incorreta
str(dados)

# 5. Estatísticas descritivas iniciais
summary(dados)

# 6. Verificar presença de dados faltantes
colSums(is.na(dados))

# 7. Visualização da relação entre livros e nota média
ggplot(dados, aes(x = livros_por_aluno, y = nota_media)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(title = "Livros por aluno e nota média",
       x = "Livros por aluno",
       y = "Nota média na prova")

# 8. Estimar modelo de regressão linear simples
# Modelo: nota_media ~ livros_por_aluno
modelo_simples <- lm(nota_media ~ livros_por_aluno, data = dados)
summary(modelo_simples)

# 9. Estimar modelo de regressão múltipla com controles
# Acrescentamos variáveis explicativas relevantes com base na literatura
modelo_multiplo <- lm(nota_media ~ livros_por_aluno + escolaridade_pais +
                        alimentacao_escolar + professores_por_aluno, data = dados)
summary(modelo_multiplo)

# 10. Diagnóstico visual dos resíduos
# Estes gráficos ajudam a verificar pressupostos importantes do modelo linear
par(mfrow = c(2, 2))
plot(modelo_multiplo)

# 11. Testes estatísticos de diagnóstico
# Verificar heterocedasticidade, autocorrelação e multicolinearidade
bptest(modelo_multiplo)        # Teste de Breusch-Pagan
dwtest(modelo_multiplo)        # Teste de Durbin-Watson
vif(modelo_multiplo)           # Fatores de inflação de variância

# 12. Identificar observações influentes
# Observações que têm grande impacto na estimação dos coeficientes
influencePlot(modelo_multiplo)

# 13. Exportar resultados de forma organizada
# Apresenta os resultados do modelo em formato legível
stargazer::stargazer(modelo_multiplo, type = "text")

# Fim do Script
