library(vip)

modelo <- fit(telco_wf, telco_train)

modelo |>
  pull_workflow_fit() |>
  vi()

arvore_wf <- workflow() %>%
  add_model(
    decision_tree(
      "classification",
      min = 30, tree_depth = 20, cost_complexity = -1
    )
  ) %>%
  add_recipe(telco_recipe)

modelo_arvore <- fit(arvore_wf, telco_train)

modelo_arvore |>
  pull_workflow_fit() |>
  vip(n = 20)
