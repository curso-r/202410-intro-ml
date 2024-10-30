
# Carregar pacotes --------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(skimr)

# Dados novos -------------------------------------------------------------

dados_novos <- tibble(
  x = 5.84, log_price = 7.9
)

# Ler o modelo ajustado ---------------------------------------------------

modelo <- readRDS("exemplos_de_aula/polinomio_grau8.rds")

# Aplicar o modelo --------------------------------------------------------

predict(modelo, new_data = dados_novos)

rmse_vec(dados_novos$log_price, predict(modelo, new_data = dados_novos)$.pred)
mae_vec(dados_novos$log_price, predict(modelo, new_data = dados_novos)$.pred)
