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
# isso aqui nao é muito legal! na proxima aula vamos ver como
# melhorar para nao precisar manipular desse jeito antes de
# modelar

# Analises iniciais -------------------------------------------------------

# nao vou fazer hoje


# split inicial -----------------------------------------------------------

credit_initial_split <- initial_split(dados_numericos)

treino <- training(credit_initial_split)
teste <- testing(credit_initial_split)

# receita -----------------------------------------------------------------

receita <- recipe(Status ~ ., data = treino) |>
  step_normalize(all_numeric_predictors())

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

metricas <- metric_set(mn_log_loss)

tunagem <- tune_grid(
  meu_fluxo,
  resamples = reamostras,
  metrics = metricas,
  grid =grid_regular(penalty(c(-12, 0)))
)


# graficos ----------------------------------------------------------------

autoplot(tunagem)


# finalizar workflow ------------------------------------------------------

workflow_final <- meu_fluxo |>
  finalize_workflow(
    select_best(tunagem)
  )

# modelo final

# antes teria que fazer o de treino/teste pra ver se nao overfitou...

modelo_final <- fit(workflow_final, dados_numericos)

dados_com_previsao <- dados_numericos |>
  bind_cols(
    predict(modelo_final, new_data = dados_numericos, type = c("prob")),
    predict(modelo_final, new_data = dados_numericos, type = c("class"))
  )

dados_com_previsao |>
  mutate(prob_grafico = ifelse(Status == "good", .pred_good, .pred_bad)) |>
  ggplot(aes(x = .pred_good, fill = Status)) +
  geom_density(alpha = 0.5)
