# objetivo: ajustar uma regressao logistica no R
# com regularizacao


# Pacotes -----------------------------------------------------------------

library(tidymodels)
library(ISLR)


# Base dados --------------------------------------------------------------

data(credit_data)

dados_numericos <- credit_data |>
  select(
    Status, Seniority, Time, Age, Expenses, Income,
    Assets, Debt, Amount, Price
  ) |>
  drop_na()

dados <- credit_data  #drop_na()
# isso aqui nao é muito legal! na proxima aula vamos ver como
# melhorar para nao precisar manipular desse jeito antes de
# modelar

skimr::skim(dados)

# Analises iniciais -------------------------------------------------------

# nao vou fazer hoje


# split inicial -----------------------------------------------------------

credit_initial_split <- initial_split(dados)

treino <- training(credit_initial_split)
teste <- testing(credit_initial_split)

# receita -----------------------------------------------------------------

receita <- recipe(Status ~ ., data = treino) |>
  #step_naomit(everything()) |>
  step_unknown(Home, Marital, Job) |>
  #step_impute_median(Assets, Debt, Income) |>
  step_impute_knn(Assets, Debt, Income, neighbors = tune()) |>
  step_dummy(all_nominal_predictors()) |>
  step_poly(Assets, Debt, Amount, Income, degree = tune())

# esse comando abaixo é útil para que eu veja os passos sendo executados

# prep(receita) |>
#   juice() |>
#   skimr::skim()

# modelo ------------------------------------------------------------------

modelo <- logistic_reg(
  engine = "glmnet",
  penalty = tune()
)

# workflow ----------------------------------------------------------------

meu_fluxo <- workflow() |>
  add_recipe(receita) |>
  add_model(modelo)

# cv ----------------------------------------------------------------------

reamostras <- vfold_cv(treino, v = 5)

# tunagem -----------------------------------------------------------------

metricas <- metric_set(mn_log_loss, accuracy, roc_auc)

tunagem <- tune_grid(
  meu_fluxo,
  resamples = reamostras,
  metrics = metricas,
  control = control_grid(verbose = TRUE),
  # esse comando é novo e controla a tunagem
  # em particular verbose=TRUE manda ele
  # imprimir na tela conforme vai ajustando
  # modelos
  grid = grid_regular(levels = 5, penalty(c(-4, -2)), degree(range = c(1, 5)), neighbors())
)

autoplot(tunagem)

show_best(tunagem, metric = "accuracy")

# graficos ----------------------------------------------------------------

autoplot(tunagem)


# finalizar workflow ------------------------------------------------------

workflow_final <- meu_fluxo |>
  finalize_workflow(
    select_best(tunagem, metric = "accuracy")
  )


# ultimo fit --------------------------------------------------------------

ultimo_modelo <- last_fit(workflow_final, credit_initial_split,
                          metrics = metricas)
# ajustar o modelo na base de teste e coletar as predicoes

collect_metrics(ultimo_modelo)

collect_predictions(ultimo_modelo) |>
  roc_curve(Status, .pred_bad) |>
  autoplot()

# modelo final

# antes teria que fazer o de treino/teste pra ver se nao overfitou...

modelo_final <- fit(workflow_final, dados)

modelo_final |> pull_workflow_fit() |> vi()

dados_com_previsao <- dados_numericos |>
  bind_cols(
    predict(modelo_final, new_data = dados_numericos, type = c("prob")),
    predict(modelo_final, new_data = dados_numericos, type = c("class"))
  )

dados_com_previsao |>
  mutate(prob_grafico = ifelse(Status == "good", .pred_good, .pred_bad)) |>
  ggplot(aes(x = .pred_good, fill = Status)) +
  geom_density(alpha = 0.5)
