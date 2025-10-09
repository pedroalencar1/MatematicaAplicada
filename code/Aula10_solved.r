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
green_ampt <- function(F, K = 5, Psi = 10, Delta_theta = 0.2, t = 2) {
  K * t + Psi * Delta_theta * log(1 + F / (Psi * Delta_theta)) - F
}

# plotando a função para visualizar a raiz
F_values <- seq(0, 100, by = 0.1)
green_ampt_values <- sapply(F_values, green_ampt)
plot(F_values, green_ampt_values, type = "l", col = "blue",
     main = "Equação de Green-Ampt",
     xlab = "F (infiltração acumulada, cm)",
     ylab = "f(F)")
abline(h = 0, col = "red", lty = 2)
#%% 5. Resolvendo a equação de Green-Ampt ------

# Usando o metodo de Newton-Raphson para encontrar F

K <- 5       # cm/h
Psi <- 10    # cm
Delta_theta <- 0.2  # cm³/cm³
t <- 2       # h

green_ampt_prime <- function(F) {
  Psi * Delta_theta / (Psi * Delta_theta + F) - 1
}
F_solution_nr <- newton_raphson(green_ampt, green_ampt_prime, x0 = 10)
cat("A infiltração acumulada F para t = 2h usando Newton-Raphson é:",
     F_solution_nr, "cm\n")

# Usando uniroot para encontrar F
F_solution <- uniroot(green_ampt, c(0, 100))$root
cat("A infiltração acumulada F para t = 2h usando uniroot é:",
    F_solution, "cm\n")

# Usando nleqslv para encontrar F
F_solution_nleqslv <- nleqslv(10, green_ampt)$x
cat("A infiltração acumulada F para t = 2h usando nleqslv é:",
     F_solution_nleqslv, "cm\n")


#%% 5. Otimização ------

#' exercicio 2: encontrar os minimo e o maximo da função não linear
#' f(x) = sin(2x) - cos(x/2) no intervalo (-2π, 2π)

#' 1. plotando a função

nl_func <- function(x) {
  y = sin(2*x) - cos(x/2)
  return(y)
}
x <- seq(-2*pi, 2*pi, by = 0.01)
y <- nl_func(x)
plot(x, y, type = "l", col = "blue",
     main = "Função Não Linear",
     xlab = "x",
     ylab = "f(x)")

#' 2. Encontrando os pontos críticos (derivada igual a zero)
#' Derivada de f(x)
nl_func_prime <- function(x) {
  y_prime = 2*cos(2*x) + (1/2)*sin(x/2)
  return(y_prime)
}       
#' Encontrando os pontos críticos usando uniroot
critical_points <- c()
intervals <- seq(-2*pi, 2*pi, by = 0.1)
for (i in 1:(length(intervals) - 1)) {
  root <- tryCatch({
    uniroot(nl_func_prime, c(intervals[i], intervals[i+1]))$root
  }, error = function(e) { NULL })
  if (!is.null(root)) {
    critical_points <- c(critical_points, root)
  }
}
critical_points

#' Avaliando a função nos pontos críticos para determinar mínimos e máximos

nl_func_prime2 <- function(x) { 
    y_prime2 = -4*sin(2*x) + (1/4)*cos(x/2)
    return(y_prime2)
}

extrema_values <- nl_func(critical_points)
second_derivatives <- nl_func_prime2(critical_points)
extrema_types <- ifelse(second_derivatives > 0, "Min", 
                       ifelse(second_derivatives < 0, "Max", "Inflex"))

extrema <- data.frame(x = critical_points, f_x = extrema_values,
                      type = extrema_types)

extrema <- extrema %>% arrange(f_x)
extrema
#' 3. Visualizando os resultados

ggplot()+
geom_line(aes(x = x, y = y), color = "blue")+
geom_point(data = extrema, aes(x = x, y = f_x, color = type), size = 5)+
geom_text(data = extrema, aes(x = x, y = f_x, label = round(f_x, 2)),
          vjust = -1, color = "black", size = 6)+
labs(title = "Otimização de Função Não Linear",
     x = "x",
     y = "f(x)")+
theme_minimal()+
theme(text = element_text(size = 30))

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
production_func <- function(vars) {
  x <- vars[1]
  y <- vars[2]
  return(100 * (x^0.5) * (y^0.5))
}
constraint_func <- function(vars) {
  x <- vars[1]
  y <- vars[2]
  return(20 * x + 30 * y - 6000)
}   

#' 2. Definindo a função Lagrangeana
lagrange_func <- function(vars) {
  x <- vars[1]
  y <- vars[2]
  lambda <- vars[3]
  L <- production_func(c(x, y)) - lambda * constraint_func(c(x, y))
  return(-L) # Negativo para maximização
}

#' 3. Encontrando os pontos críticos da Lagrangeana
initial_guess <- c(100, 100, 1)
solution <- nleqslv(initial_guess, function(vars) {
  x <- vars[1]
  y <- vars[2]
  lambda <- vars[3]
  dL_dx <- 50 * (y / x)^0.5 - lambda * 20
  dL_dy <- 50 * (x / y)^0.5 - lambda * 30
  dL_dlambda <- -constraint_func(c(x, y))
  return(c(dL_dx, dL_dy, dL_dlambda))
})
optimal_x <- solution$x[1]
optimal_y <- solution$x[2]
max_production <- production_func(c(optimal_x, optimal_y))
cat("Quantidade ótima de fertilizante A (x):", optimal_x, "kg\n")
cat("Quantidade ótima de fertilizante B (y):", optimal_y, "kg\n")
cat("Produção máxima (P):", max_production, "toneladas\n")  

# Visualizando a função de produção e a restrição orçamentária
x <- 0:300
y <- 0:300
z <- outer(x, y, function(x, y) 100 * sqrt(x) * sqrt(y))
y_restriction <- (6000 - 20 * x) / 30
z_restriction <- 100 * sqrt(x) * sqrt(y_restriction)

fig <- plot_ly(x = ~x, y = ~y, z = ~z, type = "surface") %>%
            layout(title = "Função de Produção com Restrição Orçamentária",
                   scene = list(xaxis = list(title = "Fertilizante A (kg)"),
                   yaxis = list(title = "Fertilizante B (kg)"),
                   zaxis = list(title = "Produção (toneladas)"))) %>%
    add_trace(x = x, y = y_restriction, z = z_restriction,
            name = "lines", type = "scatter3d", mode = "lines",
            line = list(color = "black", width = 10), opacity = 1) %>%
  add_markers(x = optimal_x, y = optimal_y, z = max_production,
              marker = list(color = "red", size = 8),
              name = "Ponto Ótimo")

fig
