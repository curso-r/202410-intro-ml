
set.seed(291024)

# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(tidymodels)

# Dados -------------------------------------------------------------------

data("diamonds")

plot(diamonds$x, log(diamonds$price))

diamonds_sem_0 <- diamonds |>
  filter(x > 0) |>
  mutate(log_price = log(price)) |>
  sample_n(5)

plot(diamonds_sem_0$x, log(diamonds_sem_0$price))

# Especificacao -----------------------------------------------------------

especificacao <- linear_reg()

# Ajuste ------------------------------------------------------------------

modelo <- fit(especificacao, log_price ~ x + I(x^2) + I(x^3) + I(x^4), data = diamonds_sem_0)

modelo

# Métricas ----------------------------------------------------------------

y_chapeu <- predict(modelo, new_data = diamonds_sem_0)$.pred

rmse_vec(diamonds_sem_0$log_price, y_chapeu)
mae_vec(diamonds_sem_0$log_price, y_chapeu)

# Relatorio de saida do modelo (opcional) ---------------------------------

y_chapeu2 <- predict(modelo, new_data = tibble(x = sort(diamonds_sem_0$x)))$.pred
y <- diamonds_sem_0$log_price
x <- diamonds_sem_0$x

## graficos

plot(x, y)
lines(sort(x), y_chapeu2, col = "red")

saveRDS(modelo, "exemplos_de_aula/polinomio_grau8.rds")
