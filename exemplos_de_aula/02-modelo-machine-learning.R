
# Carregar pacotes --------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(skimr)

# Dados novos -------------------------------------------------------------

dados_novos <- tibble(
  x = 6.12
)

# Ler o modelo ajustado ---------------------------------------------------

modelo <- readRDS("exemplos_de_aula/meu_primeiro_modelo.rds")

# Aplicar o modelo --------------------------------------------------------

predict(modelo, new_data = dados_novos)
