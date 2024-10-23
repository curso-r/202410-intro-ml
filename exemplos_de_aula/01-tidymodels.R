# Pacotes -----------------------------------------------------------------

# install.packages("tidymodels")

library(tidymodels)
library(skimr)

# Carregando dados --------------------------------------------------------

data("diamonds")

# Analise exploratória ----------------------------------------------------

diamonds_sem_x_0 <- diamonds |>
  filter(x > 0)

skim(diamonds)

qplot(x, price, data = diamonds_sem_x_0)
qplot(x, log(price), data = diamonds_sem_x_0)

# nossa missão:
# ao invés de fazer lm(log(price) ~ x)
# vou usar o tidymodels para isso e vamos entender a sintaxe básica

# todo "modelo" de machine learning pode ser "generalizado" nos componentes:
# 1. pré-processamento
# 2. especificação: tipo de modelo
# outro dia vamos estudar o 1, aqui vamos estudar o 2. especificação


# especificacao -----------------------------------------------------------

# especificacao_modelo <- linear_reg() |>
#   set_engine("lm") |>
#   set_mode("regression")

especificacao_modelo <- decision_tree() |>
  set_engine("rpart") |>
  set_mode("regression")

# vantagem do tidymodels: fica muito prático usar uma mesma sintaxe
# para ajustar qualquer tipo de modelo.

# isso é bom pra comparar modelos diferentes
# é bom pro caso da gente conseguir encaixar vários
# modelos diferentes no mesmo contexto

# ajuste do modelo --------------------------------------------------------

dados_modelo <- diamonds_sem_x_0 |>
  mutate(log_price = log(price)) |>
  select(x, log_price)

modelo_ajustado <- fit(especificacao_modelo, log_price ~ x, data = dados_modelo)
# dentro desse objeto ^ está a curva colorida que desenhamos nos slides
# a linha acima é a aplicaçaõ do algoritmo de machine learning
# que pega os dados e faz a melhor rota

# usos possíveis do modelo:

# 1. salvar e usar em outro código:
saveRDS(modelo_ajustado, "exemplos_de_aula/modelo_ajustado.rds")
# agora vou usar no outro script


# 2. analisar o resultado do modelo

dados_modelo_com_previsao <- dados_modelo |>
  mutate(
    log_price_chapeu = predict(modelo_ajustado, dados_modelo)$.pred
  )

dados_modelo_com_previsao |>
  ggplot(aes(x = x , y = log_price)) +
  geom_point() +
  geom_line(aes(y = log_price_chapeu), color = 'red')


