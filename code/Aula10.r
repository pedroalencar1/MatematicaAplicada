"
Programa de Pós-Graduação em Engenharia Agricola (PPGEA)
Disciplina: Matemática Aplicada à Engenharia Agrícola
Aula 10 - Equações não lineares e otimização

Autor: Dr. Pedro Alencar
12.11.2025
"

library(lintr)
lint("Aula10.r", linters = with_defaults(no_tab_linter = NULL))

#%% 0. Carregando pacotes necessários ------
library(dplyr)    # Para manipulação de dados
library(tidyr)    # Para manipulação de dados

library(nleqslv) # Para resolver equações não lineares
library(pracma)  # Para métodos numéricos

library(ggplot2)  # Para visualização dos resultados
library(plotly)  # Para visualização 3D interativa
#%% 1. Equações não lineares ------
#' plot as seguintes equações não lineares:
#' x^2 - 2 = 0
#' exp(-x) - x = 0
#' sin(x) - x/2 = 0

x <- seq(-2, 2, by = 0.05)
y1 <- x^2 - 2
y2 <- exp(-x) - x
y3 <- sin(x) - x / 2

df <- data.frame(x, y1, y2, y3)
df_long <- pivot_longer(df, cols = c(y1, y2, y3),
    names_to = "equation", values_to = "y")

ggplot(df_long, aes(x = x, y = y, color = equation)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Equações não lineares",
       x = "x",
       y = "f(x)",
       color = "Equações") +
  theme_minimal() +
  theme(text = element_text(size = 30))

#%% 2, Resolvendo equações não lineares ------
#' metodo de Newton-Raphson

newton_raphson <- function(f, f_prime, x0, tol = 1e-7, max_iter = 100) {
  x <- x0
  for (i in 1:max_iter) {
    x_new <- x - f(x) / f_prime(x)
    if (abs(x_new - x) < tol) {
      cat("Convergiu em", i, "iterações\n")
      return(x_new)
    }
    x <- x_new
  }
  stop("Não convergiu")
}

#%% soluções dos exemplos ------

# 2.1 Exemplo 1: x^2 - 2 = 0
f1 <- function(x) {
    x^2 - 2
    }
f1_prime <- function(x) {
    2 * x
    }
root1 <- newton_raphson(f1, f1_prime, x0 = -1)
cat("Raiz de x^2 - 2 = 0 é:", root1, "\n")

# 2.2 exemplo 2: exp(-x) - x = 0
f2 <- function(x) {
    exp(-x) - x
    }
f2_prime <- function(x) {
    -exp(-x) - 1
    }
root2 <- newton_raphson(f2, f2_prime, x0 = 1)
cat("Raiz de exp(-x) - x = 0 é:", root2, "\n")

# 2.3 exemplo 3: sin(x) - x/2 = 0
f3 <- function(x) {
    sin(x) - x / 2
}
f3_prime <- function(x) {
    cos(x) - 1 / 2
}
root3 <- newton_raphson(f3, f3_prime, x0 = -10)
cat("Raiz de sin(x) - x/2 = 0 é:", root3, "\n")

#%% 3. Resolvendo equações não lineares com uniroot() ------
#' A função uniroot() do R pode ser usada para encontrar raízes de funções
#' uniroot() requer um intervalo onde a função muda de sinal
#' Exemplo 1: x^2 - 2 = 0
root1_uniroot <- uniroot(f1, c(0, 2))
cat("Raiz de x^2 - 2 = 0 usando uniroot é:", root1_uniroot$root, "\n")
#' Exemplo 2: exp(-x) - x = 0
root2_uniroot <- uniroot(f2, c(0, 1))
cat("Raiz de exp(-x) - x = 0 usando uniroot é:", root2_uniroot$root, "\n")
#' Exemplo 3: sin(x) - x/2 = 0
root3_uniroot <- uniroot(f3, c(1, 2))
cat("Raiz de sin(x) - x/2 = 0 usando uniroot é:", root3_uniroot$root, "\n")

#%% 4. Resolvendo sistemas de equações não lineares com nleqslv() ------
#' A função nleqslv() do pacote nleqslv pode ser usada para resolver
#' sistemas de equações não lineares
#' Exemplo: resolver o sistema
#' x^2 + y^2 - 4 = 0
#' x - y = 0
#' Definindo as funções
system_eq <- function(vars) {
  x <- vars[1]
  y <- vars[2]
  eq1 <- x^2 + y^2 - 4
  eq2 <- x - y
  return(c(eq1, eq2))
}
#' Resolvendo o sistema
initial_guess <- c(1, 1)
solution <- nleqslv(initial_guess, system_eq)
cat("Solução do sistema de equações não lineares é: x =", solution$x[1],
    ", y =", solution$x[2], "\n")

# plotar o sistema de equações em 3D
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
z1 <- outer(x, y, function(x, y) x^2 + y^2 - 4)
z2 <- outer(x, y, function(x, y) x - y)
fig <- plot_ly() %>%
  add_surface(x = ~x, y = ~y, z = ~z1, showscale = FALSE, opacity = 0.5,
    name = "x^2 + y^2 - 4 = 0") %>%
  add_surface(x = ~x, y = ~y, z = ~z2, showscale = FALSE, opacity = 0.5,
    name = "x - y - 1 = 0") %>%
  layout(title = "Sistema de Equações Não Lineares",
         scene = list(xaxis = list(title = "X"),
                      yaxis = list(title = "Y"),
                      zaxis = list(title = "Z"))) %>%
  add_markers(x = solution$x[1], y = solution$x[2], z = 0,
    marker = list(color = "red", size = 5), name = "Solução")
fig

# Checar se a solução é correta
system_eq(solution$x) # deve ser próximo de c(0, 0)

#%% 5. Exercicio 1 ------
#' Encontrar F (infiltração acumulada) para t = 2h na equação de Green–Ampt
#'
#' F = Kt + ΨΔθ ln(1 + F/(ΨΔθ))
#' onde:
#' F = infiltração acumulada (cm)
#' K = condutividade hidráulica saturada (cm/h)
#' Ψ = potencial matricial na frente de umidade (cm)
#' Δθ = diferença entre a umidade volumétrica do solo saturado e a umidade
#' inicial (cm³/cm³)
#' t = tempo (h)
#' 
#' Dados:
#' K = 5 cm/h
#' Ψ = 10 cm
#' Δθ = 0.2 cm³/cm³
#' t = 2 h


# Definindo a função para a equação de Green-Ampt

# plotando a função para visualizar a raiz

#%% 5. Resolvendo a equação de Green-Ampt ------

# Usando o metodo de Newton-Raphson para encontrar F

# Usando uniroot para encontrar F

# Usando nleqslv para encontrar F



#%% 5. Otimização ------

#' exercicio 2: encontrar os minimo e o maximo da função não linear
#' f(x) = sin(2x) - cos(x/2) no intervalo (-2π, 2π)

#' 1. plotando a função


#' 2. Encontrando os pontos críticos (derivada igual a zero)
#' Derivada de f(x)
 
#' Encontrando os pontos críticos usando uniroot


#' Avaliando a função nos pontos críticos para determinar mínimos e máximos


#' 3. Visualizando os resultados



#%% Exercício 3 ------
#' A função de produção agrícola é dada por:
#' P(x, y) = 100x^0.5 * y^0.5
#' onde:
#' P = produção (toneladas)
#' x = quantidade de fertilizante A (kg)
#' y = quantidade de fertilizante B (kg)
#' Sujeito à restrição orçamentária:
#' 20x + 30y = 6000
#' onde:
#' 20 = custo por kg do fertilizante A ($/kg)
#' 30 = custo por kg do fertilizante B ($/kg)
#' 6000 = orçamento total disponível ($)
#' Encontrar a quantidade ótima de fertilizantes A e B para maximizar a produção P
#' usando o método dos multiplicadores de Lagrange
#' 
#' 1. Definindo a função objetivo e a restrição

   
#' 2. Definindo a função Lagrangeana


#' 3. Encontrando os pontos críticos da Lagrangeana


# Visualizando a função de produção e a restrição orçamentária
