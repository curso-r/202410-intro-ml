set.seed(29012024)


# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(tidymodels)

# Dados -------------------------------------------------------------------

# pegar dados de um SQL executando uma query
data("diamonds")


diamonds_sem_0 <- diamonds |>
  filter(x > 0) |>
  mutate(log_price = log(price)) |>
  sample_n(50000) |>
  select(x, log_price)

# Separação inicial -------------------------------------------------------

treino_e_teste <- initial_split(diamonds_sem_0)

treino <- training(treino_e_teste)
# é o que eu vou usar para escolher a f e a complexidade dela (exemplo: grau do polinomio)
# posso separar mais ainda outro pedaço aqui dentro (vou precisar fazer isso mesmo)
# a essa base se dá o nome de base de validação (pode ser uma ou várias)

# ########## ME ESQUEÇAAAAAAAAAA ################
teste <- testing(treino_e_teste)
# essa base eu nao vou mexer NUNCA, até ter escolhido a f direitinho
# quero usar essa aqui pra NO FINAL (IMPORTANTE!!!!!) eu ter um último cheiro
# sobre se teve overfitting ou não
# ########## ME ESQUEÇAAAAAAAAAA ################

# Definição do modelo -----------------------------------------------------

regressao_linear <- linear_reg()

receita_cria_grau <- recipe(log_price ~ ., data = treino) |>
  step_poly(x, degree = tune())
# tune() indica para o tidymodels que eu quero que um certo passo
# de pré-processamento ou característica do algoritmo de regressão (depois)
# vai ser escolhido por validação cruzada

# tune() diz pro tidymodels que grau é pra ser um hiperparametro

meu_fluxo <- workflow() |>
  add_recipe(receita_cria_grau) |>
  add_model(regressao_linear)

# Validação cruzada -------------------------------------------------------
# nesse passo vou mandar o R estimar o erro de predição associado
# a cada grau do polinômio para que eu construa a curva do erro de predição
# (no slide é erro de teste)

# Passo 1: mandar o R construir os folds

folds <- vfold_cv(treino, v = 5)
# no slide é K, K é mais comum nos livros, mas no tidymodels é v

# o processo de calcular o erro de predição para vários valores diferentes
# dos hiperparametros tem o nome de "tunagem" (tunagem implica que a gente vai escolher o minimo)
tunagem <- tune_grid(
  meu_fluxo,
  resamples = folds,
  grid = tibble(degree = 1:20),
  metrics = metric_set(
    rmse, mae, mape
  )
)

autoplot(tunagem, metric = "rmse")

# essa aqui é o melhor hiperparametro:
tunagem |>
  select_best(metric = "rmse")

tunagem |>
  select_best(metric = "mae")

tunagem |>
  select_best(metric = "mape")

tunagem |>
  collect_metrics() |>
  filter(.metric == "rmse", degree <= 10) |>
  ggplot(aes(x = degree, y = mean)) +
  geom_point(col = "red") +
  geom_line(col = "red") +
  theme_minimal() +
  labs(x = "Grau do polinômio", y = "Erro de predição")
