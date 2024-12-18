library(rpart)
library(magrittr)
library(ggplot2)
library(tidyverse)

x <- runif(100, -1, 1)
y <- sin(x*3) + rnorm(100, sd = 0.1)

dados <- tibble(x, y)

dados %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()

# random forest -----------------------------------------------------------

arvores <- list()
trees <- 200
tree_depth <- 4
mtry <- 1

n <- nrow(dados)

# em que a gente faz um monte de modelos:
for(tree in 1:trees){
  amostra <- dados[sample.int(n, n, replace = TRUE),]
  arvores[[tree]] <- rpart(
    y ~ x,
    data = amostra,
    control = rpart::rpart.control(maxdepth = tree_depth))
}

# formula de "juntar" os modelos
preve_um_monte_de_arvores <- function(x, lista_arvores){
  previsoes <- 0
  for(ii in seq_along(lista_arvores)){
    previsoes <- previsoes + predict(lista_arvores[[ii]], tibble(x = x))
  }
  previsoes <- previsoes/length(lista_arvores)

  tibble(y = previsoes, x = x)
}

dados %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(
    data = preve_um_monte_de_arvores(x, arvores),
    #data = tibble(y = predict(arvores[[101]], tibble(x)), x = x),
    color = "red", size = 2
  )

# codigo xgboost ----------------------------------------------------------

arvores <- list()
trees <- 1000
tree_depth <- 7
mtry <- 1

n <- nrow(dados)

parametros <- rpart::rpart.control(maxdepth = tree_depth)

# em que a gente faz um monte de modelos:

arvores[[1]] <- rpart(y ~ x, data = dados, control = parametros)

lr <- 0.1

modelo_xgboost <- function(x, lista_arvores){
  previsoes <- 0
  for(ii in seq_along(lista_arvores)){
    previsoes <- previsoes + lr*predict(lista_arvores[[ii]], tibble(x = x))
  }

  tibble(y = previsoes, x = x)
}

lambda <- 1
for(tree in 2:trees){

  #amostra <- dados[sample.int(n, n, replace = TRUE),]
  # no boosting nao precisamos fazer bootstrap, só se quisermos

  print(tree)

  indices <- sample.int(n, n, replace = TRUE)

  r <- (y[indices]-modelo_xgboost(x[indices], arvores)$y)/lambda
  # voce precisa otimizar a "derivada" da sua funcao de custo
  # a derivada de (y-y_chapeu)^2 com relacao a y é
  # d(y^2-2y_chapeu * y + y_chapeu^2)/dy_chapeu = 2y-2 * y_chapeu = 2(y-y_chapeu)

  #amostra <- dados[sample.int(n, n, replace = TRUE),]

  arvores[[tree]] <- rpart(
    r ~ x,
    data = dados[indices,],
    control = rpart::rpart.control(maxdepth = tree_depth))
}

dados %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(
    data = modelo_xgboost(x, arvores[1:1000]),
    #data = tibble(y = predict(arvores[[4]], tibble(x)), x = x),
    color = "red", size = 2
  )

sqrt(mean((modelo_xgboost(x, arvores[1:2])$y-y)^2))
