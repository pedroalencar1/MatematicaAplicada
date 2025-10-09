"
Programa de Pós-Graduação em Engenharia Agricola (PPGEA)
Disciplina: Matemática Aplicada à Engenharia Agrícola
Aula 9 - Equações Diferenciais Ordinárias (EDOs)

Autor: Dr. Pedro Alencar
05.11.2025
"

#%% 0. Carregando pacotes necessários ------
library(dplyr)    # Para manipulação de dados
library(tidyr)    # Para manipulação de dados

library(deSolve)  # Para resolver EDOs

library(ggplot2)  # Para visualização dos resultados

#%% 1. Exemplo simples com deSolve ------

#' Vamos voltar ao exemplo 1 e 2 da primeira parte da aula.
#' A equação diferencia é dada por:
#' x*y' + 2y = 4x^2
#' y(1) = 2
#'
#' Utilizando deSolve, devemos:
#' 1) definir a nossa equação como uma função
#' 2) definir o estado inicial
#' 3) definir os parâmetros (se houver)
#' 4) definir o tempo de simulação
#' 5) rodar a simulação com a função ode()

# 1) definindo a equação como uma função
#' Note que a função deve ter os argumentos: tempo, estado, parâmetros
#' No nosso caso, não há parâmetros
#' A função deve retornar uma lista com a derivada
#' A variável "x" será o tempo na simulação
#' A variável "y" será o estado que queremos simular
#' A equação deve ser rearranjada para isolar y':
#' y' = 4x - (2/x)y (Note que x != 0 é condição necessária)

ode_fun <- function(x, state, parameters) {
  with(as.list(state), {
    dy <- 4 * x - (2 / x) * y # from y' + (2/x)y = 4x
    list(c(dy))
  })
}

# 2) definindo o estado inicial
state <- c(y = 2)        # state vector: variable "y" starts at

# 3) definindo os parâmetros (se houver)
#' No nosso caso, não há parâmetros

# 4) definir o tempo de simulação
#' note que a sequência de tempo não pode conter 0, pois a equação não é
#' definida em x = 0
#' note ainda que o tempo deve começar em 1, pois é a condição inicial
xseq <- seq(1, 10, by = 0.1)

# 5) rodar a simulação com a função ode()
out <- ode(y = state, times = xseq, func = ode_fun)

head(out)

#%% 1.1 plotando resultados ------
ggplot(out, aes(x = xseq, y = y)) +
  geom_line() +
  labs(x = "X", y = "Storage (S)") +
  theme_minimal() +
  theme(text = element_text(size = 30))

#%% 1.2. Avaliando a solução algébrica ------

#' A solução algébrica da equação é dada por:
#' y = x^2 + C/x^2, com C = 1
#' Vamos avaliar a solução algébrica para comparar com a solução numérica

y_analitica <- function(x) {
  x^2 + 1 / x^2
}

y_analitica_vals <- y_analitica(xseq)

ggplot() +
  geom_line(aes(x = xseq, y = out[, "y"]), color = "blue", size = 1.5) +
  geom_line(aes(x = xseq, y = y_analitica_vals), color = "red",
   linetype = "dashed", size = 1.5) +
  labs(x = "X", y = "Y") +
  theme_minimal() +
  theme(text = element_text(size = 30)) +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "none")

#' note que as linhas se sobrepõem perfeitamente

#%% 2. Exemplo 2 - Dinâmica de reservatórios ------

#' Assumindo que o reservatório se comporta como um tanque e que a vazão de
#' saida é proporcional ao volume armazenado, a dinâmica do volume,
#' i.e. Q = kV, o balanço hidrico deste reservatório é dado por:
#'
#' dV/dt = I - kV, onde
#' V = volume armazenado (m³)
#' I = inflow (m³/dia)
#' k = coeficiente de vazão (1/dia)
#'
#' Suponha que:
#' Volume inicial: V(0) = 1000 hm³
#' Volume máximo: Vmax = 5000 hm³
#' Inflow: I = 10 hm³/dia
#' Coeficiente de vazão: k = 0.05 1/dia
#'
#' Vamos simular a dinâmica do reservatório ao longo de 365 dias.

# 1) Definindo a equação como uma função
ode_fun_reservatorio <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dV <- I - k * V
    list(c(dV))
  })
}

# 2) Definindo o estado inicial
state_reservatorio <- c(V = 1000)  # volume inicial em hm³

# 3) Definindo os parâmetros
parameters_reservatorio <- c(I = 10, k = 0.008)
    # inflow em hm³/dia e coeficiente de vazão em 1/dia

# 4) Definindo o tempo de simulação
times_reservatorio <- seq(0, 365, by = 1) # tempo em dias

# 5) Rodando a simulação com a função ode()
out_reservatorio <- ode(y = state_reservatorio, times = times_reservatorio,
  func = ode_fun_reservatorio, parms = parameters_reservatorio) |>
  as.data.frame() |>
  mutate(Q = parameters_reservatorio[2] * V) # Calculando a vazão de saída

head(out_reservatorio)

#%% 2.1 Plotando resultados ------
ggplot(as.data.frame(out_reservatorio), aes(x = time, y = V)) +
  geom_line(color = "blue", size = 1.5) +
  labs(x = "Tempo (dias)", y = "Volume Armazenado (hm³)") +
  scale_x_continuous(breaks = seq(0, 365, by = 60)) +
  scale_y_continuous(breaks = seq(0, 5000, by = 1000), limits = c(0, 5000)) +
  theme_minimal() +
  theme(text = element_text(size = 30))


#%% 3. Atividade ------

#' Um lago sofre enriquecimento por nutrientes (eutrofização).
#' Observa-se que a biomassa de macrófitas N(t) (em g/m²) cresce de
#' acordo com o modelo logístico:
#'
#' dN/dt = rN(1 - N/K), com:
#' taxa intrínseca de crescimento: r = 0.5 mês⁻¹,
#' capacidade de suporte do lago: K = 1000 g/m²,
#' biomassa inicial: N(0) = 50 g/m².
#'
#' Tarefas:
#' 1) Determinar N(t) analiticamente.
#' 2) Calcular a biomassa após 6 meses.
#' 3) Plotar a curva de crescimento no R.
#' 4) Discutir o que acontece se K aumentar (mais nutrientes).

#%% 4. Atividade

#' Um solo possui as seguintes características:
#' Condutividade saturada Ks = 0.01 m/h
#' Potencial de sucção na frente de umidade ψf = 0.1
#' Umidade volumétrica saturada θs = 0.45
#' Umidade volumétrica inicial θi = 0.20
#' A infiltração acumulada F(t) [m] no solo é descrita pelo
#' modelo de Green-Ampt 1D:
#' dF/dt = Ks(1 + ψf(θs - θi)/F), com F(0) = 0.
#' 
#' Tarefas:
#' 1) Resolva numericamente a equação e gere um gráfico da
#'    infiltração para o intevalo [0,10] horas.
#' 2) Estime graficamente a taxa de infiltração instantânea
#'    f(t) = dF/dt a partir do resultado obtido.
#' 3) Discuta qualitativamente como a taxa de infiltração varia com
#'  o tempo e com diferentes tipos de solo (mais arenoso ou mais argiloso).


