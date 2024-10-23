# Pacotes -----------------------------------------------------------------

library(tidymodels)

# Lê modelo ---------------------------------------------------------------

modelo_ajustado <- readRDS("exemplos_de_aula/modelo_ajustado.rds")

# Aplica o modelo

predict(modelo_ajustado, new_data = tibble(x = 10))
