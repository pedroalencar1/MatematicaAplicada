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

monte_carlo_pi <- function(n_sim) {
    # Gerar pontos aleatórios (x, y) no quadrado unitário
    set.seed(123) # Para reprodutibilidade
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

# plotting
theta <- seq(0, pi/2, length.out = 100)
circle_quarter <- data.frame(
    x_circle = cos(theta),
    y_circle = sin(theta)
)

ggplot(data.frame(x, y), aes(x = x, y = y)) +
    geom_point(alpha = 0.3) +
    geom_path(data = circle_quarter, aes(x = x_circle, y = y_circle), 
              color = "red", size = 1) +
    coord_fixed() +
    labs(
        x = "X",
        y = "Y"
    ) +
    scale_x_continuous(limits = c(0, 1)) +
        scale_y_continuous(limits = c(0, 1)) +
        theme_minimal() +
        theme(text = element_text(size = 30))


#%% 1.2 Aplicação em Engenharia Agrícola ------
# Exemplo Monte Carlo com Curve Number (SCS-CN)

set.seed(123)

cn_to_q <- function(prec, cn, ia_fac = 0.2) {
  # prec: precipitação (mm)
  # cn: curve number (30-98 typical)
  # ia_fac: fator para precipitação inicial (padrão 0.2 em SCS)
  storage <- (25400 / cn) - 254   # profundidade máxima de retenção S (mm)
  ia <- ia_fac * storage
  discharge <- ifelse(prec <= ia, 0, ((prec - ia)^2) / (prec - ia + storage))
  return(discharge)
}

#-------------- Parâmetros de entrada plausíveis ---------------
N <- 10000                # número de simulações Monte Carlo
shape_p <- 2.5
scale_p <- 10             # P ~ Gamma(shape, scale)
mu_cn <- 75
sd_cn <- 8

#-------------- Monte Carlo simples (amostragem i.i.d.) ---------------
P_mc <- rgamma(N, shape = shape_p, scale = scale_p)
CN_mc <- rnorm(N, mean = mu_cn, sd = sd_cn) %>% pmin(98) %>% pmax(30)
Q_mc <- cn_to_q(P_mc, CN_mc, ia_fac = 0.2)

df_mc <- tibble(P = P_mc, CN = CN_mc, Q = Q_mc)

# Estatísticas resumidas
res_mc <- df_mc %>% summarise(
  mean_Q = mean(Q),
  sd_Q = sd(Q),
  median_Q = median(Q),
  q025 = quantile(Q, 0.025),
  q975 = quantile(Q, 0.975),
  prob_Q_gt_10 = mean(Q > 10)  # prob. de Q > 10 mm (exemplo)
)
print(res_mc)

# Plots básicos
p1 <- ggplot(df_mc, aes(x = Q)) +
  geom_histogram(bins = 60, fill = "skyblue", color = "gray40") +
  labs(x = "Escoamento Q (mm)", title = "Distribuição empírica de Q (Monte Carlo)")

p2 <- ggplot(df_mc, aes(x = P, y = Q)) +
  geom_point(alpha = 0.25, size = 0.8) + labs(title = "P vs Q", x = "Precipitação P (mm)", y = "Q (mm)")

p3 <- ggplot(df_mc, aes(x = CN, y = Q)) +
  geom_point(alpha = 0.25, size = 0.8) + labs(title = "CN vs Q", x = "Curve Number (CN)", y = "Q (mm)")

# Salvar gráficos (opcional)
# ggsave("hist_Q_mc.png", p1, width = 7, height = 4)
print(p1); print(p2); print(p3)

#-------------- Convergência simples (médias para Ns diferentes) ---------------
test_Ns <- c(100, 500, 1000, 2000, 5000, 10000)
conv <- tibble(N = integer(), mean_Q = double(), q975 = double())
for (n in test_Ns) {
  P_tmp <- rgamma(n, shape = shape_p, scale = scale_p)
  CN_tmp <- rnorm(n, mean = mu_cn, sd = sd_cn) %>% pmin(98) %>% pmax(30)
  Q_tmp <- cn_to_q(P_tmp, CN_tmp)
  conv <- bind_rows(conv, tibble(N = n, mean_Q = mean(Q_tmp), q975 = quantile(Q_tmp, 0.975)))
}
print(conv)

ggplot(conv, aes(x = N, y = mean_Q)) + geom_line() + geom_point() +
  labs(title = "Convergência da média de Q com N", x = "Número de simulações (N)", y = "mean(Q)")

#-------------- Latin Hypercube Sampling (LHS) ---------------
N_lhs <- 2000
U <- randomLHS(N_lhs, 2)  # duas variáveis: P e CN

# Transformar U para as distribuições escolhidas
P_lhs <- qgamma(U[,1], shape = shape_p, scale = scale_p)
CN_lhs <- qnorm(U[,2], mean = mu_cn, sd = sd_cn) %>% pmin(98) %>% pmax(30)

Q_lhs <- cn_to_q(P_lhs, CN_lhs)
df_lhs <- tibble(P = P_lhs, CN = CN_lhs, Q = Q_lhs)

# Comparar estatísticas MC vs LHS (mesma ideia)
res_lhs <- df_lhs %>% summarise(mean_Q = mean(Q), sd_Q = sd(Q), prob_Q_gt_10 = mean(Q > 10))
print(bind_rows(MC = res_mc %>% select(mean_Q, sd_Q, prob_Q_gt_10), LHS = res_lhs), .id = "method")

#-------------- Sensibilidade simples: Spearman e PRCC aproximado ---------------
# Correlações de Spearman
spearman_P <- cor(df_lhs$P, df_lhs$Q, method = "spearman")
spearman_CN <- cor(df_lhs$CN, df_lhs$Q, method = "spearman")
cat("Spearman(P,Q) =", round(spearman_P, 3), "\n")
cat("Spearman(CN,Q) =", round(spearman_CN, 3), "\n")

# PRCC aproximado: regressão linear nos ranks
df_ranks <- df_lhs %>% mutate(across(everything(), ~rank(.)))
lm_prcc <- lm(Q ~ P + CN, data = df_ranks %>% rename(Q = Q, P = P, CN = CN))
summary(lm_prcc)
# coeficientes da regressão dos ranks: interpretação como PRCC aproximado (sinais e importância relativa)

#-------------- Probabilidade condicional / risco prático ---------------
limiar <- 20  # mm, exemplo de limiar crítico para escoamento
prob_excede <- mean(df_lhs$Q > limiar)
cat("Probabilidade estimada de Q >", limiar, "mm (LHS):", round(prob_excede, 4), "\n")

# Visualizar CDF empírica
ggplot(df_lhs, aes(x = Q)) +
  stat_ecdf(geom = "step") + labs(title = "CDF empírica de Q (LHS)", x = "Q (mm)", y = "F(Q)")

#-------------- Observações finais ---------------
cat("\nObservações:\n")
cat("- Ajuste as distribuições de P e CN com base em dados locais quando possível.\n")
cat("- Se P e CN forem correlacionados (p.ex. áreas úmidas tendem a ter CN diferentes), modele a correlação no processo de amostragem.\n")
cat("- Para probabilidades de eventos raros (ex.: Q muito grande), considere métodos de amostragem especializada.\n")
