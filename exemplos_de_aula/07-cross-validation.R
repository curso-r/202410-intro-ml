set.seed(20251105)

# Pacotes -----------------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(ISLR)

# Dados -------------------------------------------------------------------

data("Hitters")

Hitters_tibble <- as_tibble(Hitters)
Hitters_tibble$League <- as.character(Hitters_tibble$League)

# Initial split -----------------------------------------------------------

treino_e_teste <- initial_split(Hitters_tibble)

treino <- training(treino_e_teste)
teste <- testing(treino_e_teste)

# Modelo ------------------------------------------------------------------

preproc <- recipe(Salary ~ ., treino) |>
  step_impute_median(Salary) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_poly(HmRun, degree = tune())

modelo <- linear_reg(
  penalty = tune()
  # penalty é o lambda dos slides
) |>
  set_engine("glmnet")

meu_fluxo <- workflow() |>
  add_recipe(preproc) |>
  add_model(modelo)

# Validação cruzada -------------------------------------------------------

folds <- vfold_cv(treino, v = 5)

tunagem <- tune_grid(
  meu_fluxo,
  folds,
  grid = grid_regular(levels = 10, penalty(c(-2, 1.6)), degree(c(1, 3))),
  metrics = metric_set(rmse, mae, mape)
)

# plota o grafico
autoplot(tunagem, metric = "rmse")

# pega os numeros
show_best(tunagem, metric = "mape")

# fixa um unico resultado
select_best(tunagem, metric = "mape")


# Verificar se nao teve overfit mesmo e eventualmente fazer a apre --------

meu_fluxo_finalizado <- meu_fluxo |>
  finalize_workflow(
    select_best(tunagem, metric = "mape")
  )

fit_semifinal <- last_fit(meu_fluxo_finalizado, treino_e_teste,
                          metrics = metric_set(rmse, mae, mape))

fit_semifinal |>
  collect_metrics()

# Modelo final (para produção) --------------------------------------------

modelo_final <- fit(meu_fluxo_finalizado, Hitters_tibble)

saveRDS(modelo_final, "modelo_hitters.rds")

# predict(modelo_final, new_data = as.data.frame(Hitters_tibble[2:3,]))

