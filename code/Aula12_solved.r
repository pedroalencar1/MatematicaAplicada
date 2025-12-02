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

library(sensitivity)


# %% 1. Análise de sensibilidade ------------------------------------------

#' A análise de sensibilidade é uma técnica utilizada para determinar como as
#' variações nos parâmetros de entrada de um modelo afetam a saída do modelo.
#' Isso é particularmente importante em engenharia agrícola, onde modelos
#' complexos são frequentemente utilizados para simular processos como o
#' crescimento das plantas, o uso da água e a dinâmica dos nutrientes no solo.
#' A análise de sensibilidade ajuda a identificar quais parâmetros têm o maior
#'
#' Neste exemplo utilizaremos o método de Morris para avaliar a sensibilidade
#' dos parâmetros de um modelo hidrológico simples (SCS-CN) em relação à
#' performance do modelo medida pelo NSE (Nash-Sutcliffe Efficiency).
#' O metodo de Morris consiste em variar sistematicamente os parâmetros de um
#' modelo para avaliar a sensibilidade da saída do modelo em relação a essas
#' variações.

#' Primeiro devemos definir nossa função objetivo
nse <- function(obs, sim) {
  1 -
    sum((obs - sim)^2, na.rm = TRUE) /
      sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
}

# a seguir, a funçao Modelo SCS-CN
scs_runoff <- function(prec, cn = 95, ia_frac = 0.1) {
  storage <- (25400 / cn) - 254 # armazenamento potencial (mm)
  ia <- ia_frac * storage # abstração inicial (mm)
  discharge <- numeric(length(prec))
  for (i in seq_along(prec)) {
    p <- prec[i]
    if (p <= ia) {
      discharge[i] <- 0
    } else {
      discharge[i] <- ((p - ia)^2) / (p - ia + storage)
    }
  }
  discharge <- pmax(0, discharge)

  return(discharge)
}


#%% 1.1 Dados de chuva e "vazão" -----------------------------------------------

df <- read.csv('../Aula 12/estacao_acarau_1974_2024.csv') |>
  arrange(desc(row_number()))

# parâmetros "verdadeiros" para gerar observações

cn_true <- 92
ia_frac_true <- 0.12

dischage_true <- scs_runoff(df$prec, cn = cn_true, ia_frac = ia_frac_true)

# adicionar ruído para simular observações
dischage_obs <- dischage_true *
  abs(rnorm(length(dischage_true), mean = 1, sd = 0.1))

#%% 1.2 Função objetivo para sensibilidade --------------------------
# estimar NSE para simulações

model_nse <- function(X) {
  # garantir que X é matriz
  if (is.null(dim(X))) {
    X <- matrix(X, nrow = 1)
  }

  apply(X, 1, function(prow) {
    # ordem: CN, Ia_frac
    cn = prow[1]
    ia_frac = prow[2]
    sim <- scs_runoff(prec, cn, ia_frac)
    nse(dischage_true, sim)
  })
}

#%% 1.3 Configuração do método Morris via pacote 'sensitivity' ------

factors <- c("cn", "ia_frac")
# intervalos de variação (binf = lower bounds, bsup = upper bounds)
binf <- c(cn = 60, ia_frac = 0.0)
bsup <- c(cn = 99, ia_frac = 0.3)

# Parâmetros do experimento Morris
r <- 100 # número de trajetórias (replicações)
levels <- 10
design <- list(type = "oat", levels = levels)

# Executar Morris (a função morris do pacote fará as chamadas ao model_nse)
cat("Rodando morris() do pacote 'sensitivity' com", r, "trajetórias...\n")
M <- morris(
  model = model_nse,
  factors = factors,
  r = r,
  design = design,
  binf = binf,
  bsup = bsup
)

# Resultados e diagnóstico
print(M)

# O objeto M já inclui mu, mu.star e sigma estimados (com chamadas internas)
# Podemos extrair mu.star e sigma diretamente
mu_star <- M$ee %>% apply(2, function(x) mean(abs(x))) # absolute mean of EE
sigma <- M$ee %>% apply(2, sd)


# montar tabela ordenada por mu*
res <- data.frame(
  parameter = factors,
  mu_star = as.numeric(mu_star[factors]),
  sigma = as.numeric(sigma[factors]),
  row.names = NULL,
  stringsAsFactors = FALSE
)

print(res)

plot(M)

#' interpretaçao dos resultados:
#' Parâmetros com alto mu* e baixo sigma são os mais influentes e têm
#' efeitos quase lineares na saída do modelo.
#' Parâmetros com alto sigma indicam interações ou efeitos não lineares.

# %% 2. Método de Monte Carlo ------

#' O método de Monte Carlo é uma técnica estatística utilizada para modelar
#' a incerteza em sistemas complexos. Ele envolve a geração de múltiplas
#' simulações aleatórias para estimar a distribuição de resultados possíveis.
#' Isso é particularmente útil em engenharia agrícola, onde muitos fatores
#' podem influenciar os resultados, como condições climáticas, variações no
#' solo e práticas de manejo.
#'
#' O Método foi inspirado nos jogos de azar, onde a aleatoriedade é um elemento
#' chave.

# %% 2.1 Exemplo simples de Monte Carlo para estimar o valor de pi ------

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

#%% 2.2 plotting --------
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


#%% 2.3 monte carlo convergence --------
# n_sim_values <- seq(100, 10000, by = 100)

s <- 5000 # número de simulações para cada repetição
n <- 1000 # número de repetições para ver a convergência

n_sim_values <- rep(s, n)
pi_estimates <- sapply(n_sim_values, function(i) {
  result <- monte_carlo_pi(i, set_seed = FALSE)
  return(result$pi_estimate)
})

plot(pi_estimates)

# %% 2.4 plotando a convergência da média cumulativa --------
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

#%% 3 Aplicação em Engenharia Agrícola ------
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

#%% 3.1 Monte Carlo para estimar a incerteza do NSE ------

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

# %% 3.2 Estatísticas do NSE estimado ------
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

# %% 4 Avaliando incertezas em parâmetros de modelos ------

#dados de chuva
df <- read.csv('Aula 12/estacao_acarau_1974_2024.csv') |>
  arrange(desc(row_number()))
head(df)

# dados de vazão obsevados
plot(dischage_obs)

df_q <- data.frame(
  date = df$data,
  prec = df$prec,
  runoff = dischage_obs
) |>
  filter(runoff > 0)

#%% 4.1 Obtendo valores de Ia_frac -----
# Vamos assumir um CN conhecido igual a 90 e avaliar o valor de ia_frac,
# conhecendo a relação entre precipitação e vazão

cn <- 90
storage <- (25400 / cn) - 254 # armazenamento potencial (mm)

#' a equação que relaciona Ia com P e Q para um CN conhecido é:
#' Ia^2 + Ia(q - 2P) - Q(P + S) + P^2 = 0
#'
#' esta é uma equação quadrática em Ia, simples de resolver algebricamente.

solve_eq2 <- function(a = 1, b, c) {
  delta <- b^2 - 4 * a * c
  if (delta >= 0) {
    root1 <- (-b + sqrt(delta)) / (2 * a)
    root2 <- (-b - sqrt(delta)) / (2 * a)
    return(c(root1, root2))
  } else {
    return(c(NA, NA)) # sem solução real
  }
}

for (i in seq_along(df_q$prec)) {
  p <- df_q$prec[i]
  q <- df_q$runoff[i]
  a <- 1
  b <- q - 2 * p
  c <- -q * (p + storage) + p^2

  roots <- solve_eq2(a, b, c)

  df_q$ia_frac_1[i] <- roots[1] / storage
  df_q$ia_frac_2[i] <- roots[2] / storage
}

df_q <- df_q |>
  mutate(
    # remover valores inválidos
    ia_frac_1 = ifelse(ia_frac_1 <= 0 | ia_frac_1 >= 1, NA, ia_frac_1),
    ia_frac_2 = ifelse(ia_frac_2 <= 0 | ia_frac_2 >= 1, NA, ia_frac_2)
  )

#%% 4.2 testar soluções e obter solução real ----

# função SCS-CN para um único par (P, Ia_frac)
scs_runoff_single <- function(p, cn = 90, ia_frac) {
  if (is.na(ia_frac)) {
    return(1e9)
  }

  storage <- (25400 / cn) - 254 # armazenamento potencial (mm)
  ia <- ia_frac * storage # abstração inicial (mm)
  if (p <= ia) {
    discharge <- 0
  } else {
    discharge <- ((p - ia)^2) / (p - ia + storage)
  }
  discharge <- pmax(0, discharge)
  return(discharge)
}

# calcular Q estimado para as duas soluções de Ia_frac
df_q <- df_q |>
  rowwise() |>
  mutate(
    q_test_1 = scs_runoff_single(p = prec, ia_frac = ia_frac_1),
    q_test_2 = scs_runoff_single(p = prec, ia_frac = ia_frac_2)
  ) |>
  ungroup()


# obter ia_frac estimado a partir de P e Q observados
df_q <- df_q |>
  rowwise() |>
  mutate(
    ia_estimado = ifelse(
      abs(q_test_1 - runoff) < abs(q_test_2 - runoff),
      ia_frac_1,
      ia_frac_2
    )
  ) |>
  select(-c(ia_frac_1, ia_frac_2, q_test_1, q_test_2)) |>
  ungroup() |>
  filter(is.na(ia_estimado) == FALSE)

ggplot(df_q) +
  geom_histogram(aes(x = ia_estimado), bins = 30, fill = "lightblue") +
  labs(
    x = "Fraçao de abstração inicial (Ia / S)",
    y = "Frequência",
    title = "Distribuição estimada de Ia_frac a partir de P e Q observados"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 20))

#%% 4.3 ajustar distribuição gamma aos valores estimados de ia_frac ------

library(fitdistrplus)

par_gamma <- fitdistrplus::fitdist(
  df_q$ia_estimado,
  distr = "gamma",
  method = "mle"
)
par_gamma$estimate['shape']
par_gamma$estimate["rate"]

# estimar runoff considerando a incerteza em ia_frac via Monte Carlo
set.seed(123)
n_sim <- 100

# usando apply, simular cada dia n_sim vezes. sortear o valor de ia_frac pela função gamma para cada simulação. salvar os dados em um dataframe com n_sim colunas

df_vazao <- data.frame(
  date = df$data,
  prec = df$prec,
  runoff = dischage_obs
)

ia_frac_sim <- rgamma(
  n = n_sim,
  shape = par_gamma$estimate["shape"],
  rate = par_gamma$estimate["rate"]
)

# simular vazão para cada valor de ia_frac sorteado
q_sim <- sapply(ia_frac_sim, function(ia_frac) {
  scs_runoff(df_vazao$prec, cn = 90, ia_frac = ia_frac)
})

#%% 4.4  obter quantiles das simulações para cada dia ------
# alocar valores como colunas em df_vazao
q_prob <- c(0.025, 0.5, 0.975)
q_quantiles <- apply(q_sim, 1, function(x) {
  quantile(x, probs = q_prob)
})
q_quantiles <- t(q_quantiles)
colnames(q_quantiles) <- c("q2_5", "median", "q97_5")
df_vazao <- cbind(df_vazao, q_quantiles)

head(df_vazao)

#%% 4.5  plotar resultados ------
df_vazao |>
  mutate(
    year = lubridate::year(as.Date(date)),
    month = lubridate::month(as.Date(date))
  ) |>
  filter(year == 2002, month %in% 3:4) |>
  ggplot(aes(x = as.Date(date))) +
  geom_ribbon(aes(ymin = q2_5, ymax = q97_5), fill = "lightblue") +
  geom_point(aes(y = median), color = "blue", size = 1) +
  geom_point(aes(y = runoff), color = "red", size = 2, alpha = 1) +
  labs(
    x = "Data",
    y = "Vazão (mm)",
    title = "Simulações de vazão com incerteza em Ia_frac via Monte Carlo"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 15))

# valores de NSE considerando para as n_sim simulações
nse_values <- apply(q_sim, 2, function(q_est) {
  nse(df_vazao$runoff, q_est)
})

hist(
  nse_values,
  breaks = 30,
  main = "Distribuição do NSE considerando incerteza em Ia_frac",
  xlab = "NSE",
  col = "lightgreen"
)
