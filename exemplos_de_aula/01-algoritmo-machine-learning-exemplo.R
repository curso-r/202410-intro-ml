
# Pacotes -----------------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(skimr)

# Carregando dados --------------------------------------------------------

data("diamonds")
# aqui o codigo pode ser tão simples quanto "ler" ou
# pode ser uma coisa mais complicada, várias linhas de processamento etc

# Análises exploratórias (opcional) ---------------------------------------

skim(diamonds)

plot(diamonds$x, log(diamonds$price))

diamonds_sem_0 <- diamonds |>
  filter(x > 0) |>
  mutate(log_price = log(price))

plot(diamonds_sem_0$x, log(diamonds_sem_0$price))

# Especificacao -----------------------------------------------------------

# aqui falamos qual "tipo" de algoritmo vai ser executado. definimos:
# se é classificação ou regressão (o Y é número ou é categoria?)
# o tipo de modelo que vai ser ajustado:
# regressao linear (formula a*x+b e evoluções)
# arvore

# ao final dessa sessão tem que ter um objeto "especificacao" (ou similar)

especificacao <- linear_reg()

# Ajuste do modelo (algoritmo propriamente dito) --------------------------

# eu vou pegar X, Y e especificacamo, mandar juntar tudo e vai sair
# "modelo"

modelo <- fit(especificacao, log_price ~ x, data = diamonds_sem_0)

# Exportar o "modelo" (a f ajustada) para que possa ser usado no c --------

saveRDS(modelo, "exemplos_de_aula/meu_primeiro_modelo.rds")

# Relatorio de saida do modelo (opcional) ---------------------------------

y_chapeu <- predict(modelo, new_data = diamonds_sem_0)$.pred
y <- diamonds_sem_0$log_price
x <- diamonds_sem_0$x

## graficos

plot(x, y)
lines(x, y_chapeu, col = "red")

plot(x, exp(y))
lines(x, exp(y_chapeu), col = "red")

## metricas de erro

mean(abs(exp(y_chapeu)-exp(y)))

