
# Pacote ------------------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(pROC)


# Base de dados -----------------------------------------------------------

data(credit_data)

dados <- credit_data

# initial split -----------------------------------------------------------

set.seed(1)

split_inicial <- initial_split(dados)

treino <- training(split_inicial)
teste <- testing(split_inicial)

# receitas ----------------------------------------------------------------

receita_regressao <- recipe(Status ~ ., data = treino) |>
  #step_naomit(everything()) |>
  step_unknown(Home, Marital, Job) |>
  step_impute_median(Assets, Debt, Income) |>
  step_impute_knn(Assets, Debt, Income, neighbors = tune()) |>
  step_dummy(all_nominal_predictors()) #|>
  #step_poly(Assets, Debt, Amount, Income, degree = tune())

receita_arvore <- recipe(Status ~ ., data = treino) |>
  step_zv(all_nominal_predictors()) |>
  step_impute_median(Assets, Debt, Income) |>
  step_unknown(Home, Marital, Job)

# modelos -----------------------------------------------------------------

modelo_arvore <- decision_tree(
  min_n = tune(),
  tree_depth = tune(),
  cost_complexity = tune(),
  "classification") |>
  set_engine(
    "rpart"
  )

modelo_regressao <- logistic_reg(
  penalty = tune()
) |>
  set_engine("glmnet")

modelo_random_forest <- rand_forest(
  mode = "classification",
  mtry = tune()
) |>
  set_engine("ranger")

# workflows ---------------------------------------------------------------

workflow_arvore <- workflow() |>
  add_recipe(receita_arvore) |>
  add_model(modelo_arvore)

workflow_regressao <- workflow() |>
  add_recipe(receita_regressao) |>
  add_model(modelo_regressao)

workflow_random_forest <- workflow() |>
  add_recipe(receita_arvore) |>
  add_model(modelo_random_forest)


# tunagem -----------------------------------------------------------------


grid_arvore <- grid_regular(
  tree_depth(c(10, 15)),
  min_n(c(10, 30)),
  cost_complexity(c(-10, -3)),
  levels = 5
)

controle <- control_grid(verbose = TRUE, allow_par = TRUE)

metricas <- metric_set(roc_auc, accuracy, sensitivity)

reamostras <- vfold_cv(treino, 3)

tunagem_arvore <- tune_grid(
  workflow_arvore,
  reamostras,
  grid = grid_arvore,
  control = controle,
  metrics = metricas
)

tunagem_regressao <- tune_grid(workflow_regressao, reamostras)
tunagem_random_forest <- tune_grid(workflow_random_forest, reamostras, control = controle)

autoplot(tunagem_arvore, metric = "roc_auc")
autoplot(tunagem_regressao)

autoplot(tunagem_random_forest)

# finalizando modelo ------------------------------------------------------

workflow_arvore_final <- workflow_arvore |>
  finalize_workflow(
    select_best(tunagem_arvore)
  )

workflow_regressao_final <- workflow_regressao |>
  finalize_workflow(
    select_best(tunagem_regressao)
  )

workflow_random_forest <- workflow_random_forest |>
  finalize_workflow(
    select_best(tunagem_random_forest, metric = "accuracy")
  )

# ultimo ajuste -----------------------------------------------------------

last_fit_arvore <- last_fit(workflow_arvore_final, split_inicial)

last_fit_regressao <- last_fit(workflow_regressao_final, split_inicial)

last_fit_rf <- last_fit(workflow_random_forest, split_inicial)

collect_metrics(last_fit_arvore)
collect_metrics(last_fit_regressao)
collect_metrics(last_fit_rf)

# curvas ROC --------------------------------------------------------------

collect_predictions (last_fit_arvore) |>
  roc_curve(Status, .pred_bad) |>
  autoplot()

bind_rows(
  collect_predictions(last_fit_arvore) |>
    roc_curve(Status, .pred_bad) |>
    mutate(modelo = "Árvore"),
  collect_predictions(last_fit_regressao) |>
    roc_curve(Status, .pred_bad) |>
    mutate(modelo = "Regressão"),
  collect_predictions(last_fit_rf) |>
    roc_curve(Status, .pred_bad) |>
    mutate(modelo = "RF")
) |>
  ggplot(aes(x = 1-specificity, y = sensitivity, color = modelo)) +
  geom_line() +
  theme_bw()
