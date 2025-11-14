"
Programa de Pós-Graduação em Engenharia Agricola (PPGEA)
Disciplina: Matemática Aplicada à Engenharia Agrícola
Aula 12 - Sensibilidade, incerteza e Monte Carlo

Autor: Dr. Pedro Alencar
26.11.2025
"

#%% 0. Carregando pacotes necessários ------
library(dplyr) # Para manipulação de dados
library(tidyr) # Para manipulação de dados
library(lubridate) # Para manipulação de datas

library(ggplot2) # Para visualização dos resultados
library(ggforce)


# options(repos = c(CRAN = "https://cran.rstudio.com/"))
# install.packages("lhs")
library(lhs)

# %% 1. Método de Monte Carlo ------

#' O método de Monte Carlo é uma técnica estatística utilizada para modelar
#' a incerteza em sistemas complexos. Ele envolve a geração de múltiplas
#' simulações aleatórias para estimar a distribuição de resultados possíveis.
#' Isso é particularmente útil em engenharia agrícola, onde muitos fatores
#' podem influenciar os resultados, como condições climáticas, variações no
#' solo e práticas de manejo.
#'
#' O Método foi inspirado nos jogos de azar, onde a aleatoriedade é um elemento
#' chave.

# %% 1.1 Exemplo simples de Monte Carlo para estimar o valor de pi ------

# Número de simulações
n_sim <- 1000

monte_carlo_pi <- function(n_sim, set_seed = TRUE) {
  # Gerar pontos aleatórios (x, y) no quadrado unitário
  if (set_seed) {
    set.seed(123)
  } # Para reprodutibilidade

  x <- runif(n_sim, min = 0, max = 1)
  y <- runif(n_sim, min = 0, max = 1)
  # Calcular a distância do ponto (x, y) à origem (0, 0)
  distancia <- sqrt(x^2 + y^2)
  # Contar quantos pontos caem dentro do círculo de raio 1
  dentro_circulo <- sum(distancia <= 1)
  # Estimar o valor de pi
  pi_estimate <- (dentro_circulo / n_sim) * 4
  return(list(pi_estimate = pi_estimate, x = x, y = y))
}

pi_result <- monte_carlo_pi(n_sim)
pi_estimate <- pi_result$pi_estimate
x <- pi_result$x
y <- pi_result$y

print(paste("Estimativa de pi com", n_sim, "simulações:", pi_estimate))

#%% 1.2 plotting --------
theta <- seq(0, pi / 2, length.out = 100)
circle_quarter <- data.frame(
  x_circle = cos(theta),
  y_circle = sin(theta)
)

ggplot(data.frame(x, y), aes(x = x, y = y)) +
  geom_point(alpha = 0.3) +
  geom_path(
    data = circle_quarter,
    aes(x = x_circle, y = y_circle),
    color = "red",
    size = 1
  ) +
  coord_fixed() +
  labs(
    x = "X",
    y = "Y"
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(text = element_text(size = 30))


#%% 1.3 monte carlo convergence --------
# n_sim_values <- seq(100, 10000, by = 100)

s <- 5000 # número de simulações para cada repetição
n <- 1000 # número de repetições para ver a convergência

n_sim_values <- rep(s, n)
pi_estimates <- sapply(n_sim_values, function(i) {
  result <- monte_carlo_pi(i, set_seed = FALSE)
  return(result$pi_estimate)
})

plot(pi_estimates)

# %% 1.4 plotando a convergência da média cumulativa --------
running_mean <- cumsum(pi_estimates) / seq_along(pi_estimates)


ggplot(
  data = data.frame(x = seq(n), mean = running_mean),
  aes(x = x, y = mean)
) +
  geom_line(color = "blue") +
  labs(
    x = "Número de simulações",
    y = "Média cumulativa de pi estimado"
  ) +
  geom_hline(yintercept = pi, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(text = element_text(size = 20))

#%% 2 Aplicação em Engenharia Agrícola ------
# uso de Monte Carlo para estimar incertezas na performance de modelos hidrológicos simples

# crie um dataframe com valores medidos e modelados de vazao.
# compute NSE para todo o conjunto
# agorausando monte carlo, selecione subconjuntos aleatoios e compute o NSE dos subconjuntos
# o obtenha a distribuicao do NSE
# Gerar dados de vazão observada e modelada
set.seed(123)

n <- 100
data <- data.frame(vazao_observada = rnorm(n = n, mean = 50, sd = 10)) |>
  mutate(vazao_modelada = vazao_observada + rnorm(n = n, mean = 0, sd = 5)) #ruido

# Função para calcular o NSE
nse <- function(observado, modelado) {
  1 - sum((observado - modelado)^2) / sum((observado - mean(observado))^2)
}

# Calcular o NSE para todo o conjunto de dados
nse_total <- nse(data$vazao_observada, data$vazao_modelada)
print(paste("NSE total:", round(nse_total, 3)))

#%% 2.1 Monte Carlo para estimar a incerteza do NSE ------

n_subsets <- 1000
subset_size <- 50

set.seed(123)
nse_values <- numeric(n_subsets)
for (i in 1:n_subsets) {
  subset_indices <- sample(1:n, subset_size, replace = TRUE)
  subset_data <- data[subset_indices, ]
  nse_values[i] <- nse(subset_data$vazao_observada, subset_data$vazao_modelada)
}

hist(
  nse_values,
  breaks = 30,
  main = "Distribuição do NSE via Monte Carlo",
  xlab = "NSE",
  col = "lightblue"
)

# %% 2.2 Estatísticas do NSE estimado ------
nse_mean <- mean(nse_values)
nse_sd <- sd(nse_values)

q_97_5 <- quantile(nse_values, 0.975)
q_2_5 <- quantile(nse_values, 0.025)

print(paste("Média do NSE:", round(nse_mean, 3)))
print(paste("Desvio padrão do NSE:", round(nse_sd, 3)))
print(paste(
  "Intervalo de confiança 95% do NSE: [",
  round(q_2_5, 3),
  ", ",
  round(q_97_5, 3),
  "]",
  sep = ""
))

# %% 3 Avaliando incertezas em parâmetros de modelos ------

#' vamos estimar o valor de CN e sua incerteza a partir de dados medidos de
#' precipitação e escoamento.

# selecione dos dados
link <- rdwd::selectDWD(
  "Potsdam",
  res = "daily",
  var = "kl",
  per = "historical",
  current = TRUE
)

# baixe os dados (read=FALSE apenas baixa o arquivo, sem ler)
file <- rdwd::dataDWD(link, read = FALSE)

# leia os dados
clim <- rdwd::readDWD(file, varnames = TRUE) # can happen directly in dataDWD

# selecione dados desejados
data <- clim %>%
  select(MESS_DATUM, RSK.Niederschlagshoehe) %>%
  rename(
    date = MESS_DATUM,
    prec = RSK.Niederschlagshoehe,
  ) |>
  dplyr::mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"))

# %% 3.1 Calcular escoamento diário (simplificado) ------

# Função para calcular escoamento Q a partir de precipitação P e CN
p_to_q <- function(prec, cn, ia = 0.1) {
  S <- (25400 / cn) - 254
  Q <- ifelse(prec > ia * S, (prec - ia * S)^2 / (prec + S * (1 - ia)), 0)
  return(Q)
}
# Gerar dados sintéticos de escoamento com CN variável
set.seed(123)

data$runoff <- NA
for (i in 1:nrow(data)) {
  cn <- rnorm(1, mean = 85, sd = 5) %>%
    pmin(99) %>%
    pmax(70)

  data$runoff[i] <- p_to_q(data$prec[i], cn)
}

# %% 3.2 Estimar incerteza do CN via Monte Carlo ------
# Vamos variar CN entre 30 e 100 e calcular o NSE para cada valor
head(data)
df_nse <- data.frame(cn = integer(), nse = numeric())
for (cn in 30:100) {
  data <- data %>%
    mutate(
      q_est = p_to_q(prec, cn)
    )

  nse_value <- nse(data$runoff, data$q_est)

  df_nse <- rbind(df_nse, data.frame(cn = cn, nse = nse_value))
}
