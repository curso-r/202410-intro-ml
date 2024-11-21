# Pacotes -----------------------------------------------------------------

library(tidymodels)

# Base de dados -----------------------------------------------------------

data(concrete, package = "modeldata")
nrow(concrete)
glimpse(concrete)

# initial split?

# exercício 0 -------------------------------------------------------------
# Defina uma 'recipe' que normalize todas as variáveis explicativas.
# Dicas: recipe(), step_normalize(), all_numeric_predictors().

receita <- recipe(compressive_strength ~ ., x = concrete)

# exercício 1 -------------------------------------------------------------
# Defina uma especificação de f que caracterize uma regressão linear
# (mode 'regression'). Especifique também que você deseja tunar a 'penalty' e
# a 'mixture'.
# Dicas: linear_reg(), set_engine(), set_mode(), tune().

modelo_arvore <- decision_tree(
    mode = "regression",
    tree_depth = tune()
    # tune() indica que é um hiperparametro que deve ficar
    # sem definição exata até irmos testar na base validação (ou bases no caso de
    # cv/cross-validation)
)

# exercício 2 -------------------------------------------------------------
# Defina um 'workflow' que junte a receita do ex. 0 e o modelo do ex. 1.
# Dicas: workflow(), add_model(), add_recipe().

meu_fluxo <- workflow() |>
  add_recipe(receita) |>
  add_model(modelo_arvore)

# exercício 3 -------------------------------------------------------------
# Crie um objeto que represente a estratégia de reamostragem do tipo K-Fold cross
# validation com 5 folds.
# Dica: vfold_cv().

bases_validacao <- vfold_cv(concrete, v = 5)

# exercício 4 -------------------------------------------------------------
# Defina um grid de hiperparâmetros que você irá testar tanto de 'penalty' quanto
# de 'mixture'.
# Dica: grid_regular(), penalty(), mixture().

# exercício 5 -------------------------------------------------------------
# Execute a tunagem do modelo usando os objetos criados nos exercícios anteriores.
# Dica: tune_grid()

tunagem <- tune_grid(
  meu_fluxo, resamples = bases_validacao
)

autoplot(tunagem)

show_best(tunagem) |> View()



# exercício 6 -------------------------------------------------------------
# Visualize os resultados dos modelos ajustados e atualize o workflow com os
# parâmetros do melhor modelo.
# Dica: autoplot(), collect_metrics(), show_best(), select_best(), finalize_workflow().


# desafio 1 ---------------------------------------------------------------
# Qual hiperparâmetro tem maior efeito no resultado do modelo? Justifique
# a sua afirmativa com um gráfico.


# exercício 7 -------------------------------------------------------------
# Ajuste o modelo na base de treino e verifique o desempenho na base de teste.
# Dica: last_fit(split = ______initial_split), collect_metrics()


# exercício 8 -------------------------------------------------------------
# Ajuste o modelo final para a base inteira salve-o em um arquivo .rds.
# Dica: fit(), saveRDS().

## final. o que saiu do modelo?

meu_fluxo_finalizado <- meu_fluxo |>
  finalize_workflow(
    select_best(tunagem)
  )
# aqui o tune() deixa de ser tune()

modelo_final <- fit(meu_fluxo_finalizado, concrete)

base_original_com_previsoes <- concrete |>
  mutate(
    forca_proposta = predict(modelo_final, new_data = concrete)$.pred
  )

base_original_com_previsoes |>
  ggplot(aes(x = age, y = compressive_strength)) +
  geom_point()
