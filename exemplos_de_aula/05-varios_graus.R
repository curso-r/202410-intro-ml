
set.seed(29102024)

# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(tidymodels)

# Dados -------------------------------------------------------------------

data("diamonds")

plot(diamonds$x, log(diamonds$price))

diamonds_sem_0 <- diamonds |>
  filter(x > 0) |>
  mutate(log_price = log(price)) |>
  sample_n(100) |>
  select(x, log_price)

plot(diamonds_sem_0$x, diamonds_sem_0$log_price)

# Separacao treino e teste ------------------------------------------------

treino_e_teste <- initial_split(diamonds_sem_0)

treino <- training(treino_e_teste)
teste <- testing(treino_e_teste)

# Especificacao -----------------------------------------------------------

regressao_linear <- linear_reg()


# recipe ------------------------------------------------------------------
# recipes são rotinas do tidymodels que permitem que você aplique passos
# de pré-processamento aos seus algoritmos de machine learning
# exemplo: remover NA, criar graus de polinomio (ajuda mto!),
# criar transformação de variáveis

receita_cria_grau <- recipe(log_price ~ ., data = treino) |>
  step_poly(x, degree = 6)

# juice(prep(receita)) mostra pra voce uma prévia da base que sai
# do pré-processamento a partir da base de treino

juice(prep(receita_cria_grau))


# workflow ----------------------------------------------------------------

# workflow é um "modelo ampliado" que tem tanto a f em si, quanto
# os passos de pré-processamento, que podem ser super complicados
# e importantes.

meu_fluxo <- workflow() |>
  add_recipe(receita_cria_grau) |>
  add_model(regressao_linear)


# ajustando um workflow ---------------------------------------------------

modelo <- fit(meu_fluxo, data = treino)

# calculando erro ---------------------------------------------------------

rmse_vec(treino$log_price, predict(modelo, new_data = treino)$.pred)
rmse_vec(teste$log_price, predict(modelo, new_data = teste)$.pred)

# ajustar varios polinomios -----------------------------------------------

graus <- 1:9

vetor_erro_treino <- numeric(length(graus))
vetor_erro_teste <- numeric(length(graus))

for(grau in graus){

  print(grau)

  receita_cria_grau <- recipe(log_price ~ ., data = treino) |>
    step_poly(x, degree = grau)

  meu_fluxo <- workflow() |>
    add_recipe(receita_cria_grau) |>
    add_model(regressao_linear)

  modelo <- fit(meu_fluxo, data = treino)

  vetor_erro_treino[grau] <- rmse_vec(treino$log_price, predict(modelo, new_data = treino)$.pred)
  vetor_erro_teste[grau] <- rmse_vec(teste$log_price, predict(modelo, new_data = teste)$.pred)

}

plot(graus, vetor_erro_treino, type = "l", col = "blue", ylim = c(0.225, 0.28))
lines(graus, vetor_erro_teste, type = "p", col = "red")

