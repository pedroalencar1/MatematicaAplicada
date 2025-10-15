"
Programa de Pós-Graduação em Engenharia Agricola (PPGEA)
Disciplina: Matemática Aplicada à Engenharia Agrícola
Aula 11 - Séries Temporais Hidrológicas

Autor: Dr. Pedro Alencar
19.11.2025
"

#%% 0. Carregando pacotes necessários ------
library(dplyr) # Para manipulação de dados
library(tidyr) # Para manipulação de dados
library(lubridate) # Para manipulação de datas

library(ggplot2) # Para visualização dos resultados

#%% 0.1 instalando o pacote rdwd (se necessário) ------
#' este pacote permite baixar dados hidrológicos do serviço meteorológico
#' alemão (DWD)

# Verifica se o pacote está instalado, se não, instala
if (!require(rdwd, quietly = TRUE)) {
    install.packages("rdwd")
    library(rdwd)
} else {
    library(rdwd) # Carrega o pacote RDWD
}

#' NOTA:
#' Nesta aula utilizaremos os dados de chuva e temperatura da estação de
#' Potsdam, na Alemanha. A estação de Potsdam é uma estação meteorológica
#' de referência na Alemanha, conhecida por sua longa série histórica de
#' dados climáticos, que remonta ao século XIX. Localizada na cidade de
#' Potsdam, próxima a Berlim, esta estação é operada pelo Serviço Meteorológico
#' Alemão (DWD).

# %% 1. Baixando os dados de chuva da estação de Potsdam ------

# selecione dos dados
link <- selectDWD(
    "Potsdam",
    res = "daily",
    var = "kl",
    per = "historical",
    current = TRUE
)

# baixe os dados (read=FALSE apenas baixa o arquivo, sem ler)
file <- dataDWD(link, read = FALSE)

# leia os dados
clim <- readDWD(file, varnames = TRUE) # can happen directly in dataDWD

# selecione dados desejados
data <- clim %>%
    select(MESS_DATUM, RSK.Niederschlagshoehe, TMK.Lufttemperatur) %>%
    rename(
        date = MESS_DATUM,
        prec = RSK.Niederschlagshoehe,
        temp = TMK.Lufttemperatur
    ) |>
    dplyr::mutate(date = as.Date(as.character(date), format = "%Y-%m-%d"))

# %% 2. Visualizando os dados ------

ggplot(data, aes(x = date)) +
    geom_line(aes(y = prec), color = "blue", size = 1) +
    labs(x = "Data", y = "Chuva (mm)") +
    theme_minimal() +
    theme(text = element_text(size = 30))

ggplot(data, aes(x = date)) +
    geom_line(aes(y = temp), color = "red", size = 1) +
    labs(x = "Data", y = "Chuva (mm)") +
    theme_minimal() +
    theme(text = element_text(size = 30))

# %% 3. Exercício ------
#' Avaliar os componentes da série temporal de chuva (tendência, sazonalidade,
#' ciclo e resíduo/ruído)
decompose(ts(data$prec, frequency = 365.25), type = "additive") |> plot()

decompose(ts(data$prec, frequency = 365.25), type = "multiplicative") |> plot()

#%% 3.1 - A série é muito longa. Vamos explorar os últimos 5 anos ------
data_10 <- data |>
    filter(date >= as.Date("2015-01-01") & date <= as.Date("2024-12-31"))

decompose(ts(data_10$prec, frequency = 365.25), type = "additive") |> plot()
decompose(ts(data_10$prec, frequency = 365.25), type = "multiplicative") |>
    plot()

#' A função decompose() utiliza uma janela móvel para estimar a tendência,
#' correspondente a Trend(t) = MA[MA(X(t-f/2):X(t+f/2))], where f is the
#' frequency. Seasonality effect is also computed with basis of the frequency f.
#' E.g. With frequency = 365.25:
#'  - Day 1 (Jan 1): Averages all Jan 1st values across years
#'  - Day 182 (July 1): Averages all July 1st values across years
#'  - Day 365 (Dec 31): Averages all Dec 31st values across years
#' Result: A 365-day pattern showing typical daily precipitation throughout
#' the year
#' Seasonal effect is computed as:
#' 1. Detrend(t) = X(t) - Trend(t) or X(t) / Trend(t)
#' 2. Seasonality(t) = Average of Detrend(t) for each day of the year (season)
#' 3. Adjust Seasonality(t) to have mean zero (additive) or one (multiplicative)
#' The random effect (noise) is computed as:
#' Residual(t) = X(t) - Trend(t) - Season(t) or X(t) / (Trend(t) * Season(t))

#%% 3.2 - Decomposição STL ------
# STL (Seasonal and Trend decomposition using Loess)
# STL é uma técnica mais avançada que permite decompor séries temporais
# em componentes de tendência, sazonalidade e resíduo usando suavização local
# (loess). STL é mais flexível que a decomposição clássica e pode lidar
# melhor com dados irregulares e mudanças na sazonalidade ao longo do tempo.
stl(ts(data_10$prec, frequency = 365.25), s.window = "periodic") |> plot()

#' no STL, a sazonalidade é estimada usando suavização local (loess)
#' com uma janela que pode ser ajustada. O parâmetro s.window controla a
#' suavização da sazonalidade. Com s.window = "periodic", a sazonalidade é
#' tratada como periódica, o que é adequado para dados com padrões sazonais
#' consistentes ao longo do tempo. STL também permite ajustar a tendência
#' usando suavização local, o que pode capturar mudanças mais sutis na tendência
#' ao longo do tempo. O componente de resíduo é o que resta após remover a
#' tendência e a sazonalidade dos dados originais.
#' As barras no gráfico STL representam a variabilidade ou incerteza
#' associada a cada componente (tendência, sazonalidade e resíduo). Elas
#' indicam a amplitude das flutuações em torno da linha central de cada
#' componente. Barras maiores indicam maior variabilidade, enquanto barras
#' menores indicam menor variabilidade. Essas barras ajudam a visualizar a
#' incerteza associada a cada componente.

# %% 3.3 - Diferentes frequências sazonais ------
# Frequência mensal
data_monthly <- data_10 |>
    mutate(year = lubridate::year(date), month = lubridate::month(date)) |>
    group_by(year, month) |>
    summarise(prec_monthly = sum(prec, na.rm = TRUE)) |>
    ungroup() |>
    mutate(date = as.Date(paste(year, month, "15", sep = "-"))) # mid-month

ts_monthly <- ts(data_monthly$prec_monthly, frequency = 12)
decompose(ts_monthly, type = "additive") |> plot()

# Frequência semanal
data_weekly <- data_10 |>
    mutate(year = lubridate::year(date), week = lubridate::week(date)) |>
    group_by(year, week) |>
    summarise(prec_weekly = sum(prec, na.rm = TRUE)) |>
    ungroup() |>
    mutate(date = as.Date(paste(year, week, "1", sep = "-"))) # first day of week

ts_weekly <- ts(data_weekly$prec_weekly, frequency = 52)
decompose(ts_weekly, type = "additive") |> plot()

# Frequencia sazonal - estações do ano
# No hemisfério norte:
# Primavera: 21 de março a 20 de junho
# Verão: 21 de junho a 20 de setembro
# Outono: 21 de setembro a 20 de dezembro
# Inverno: 21 de dezembro a 20 de março

data_seasonal <- data_10 |>
    mutate(
        year = lubridate::year(date),
        month = lubridate::month(date),
        day = lubridate::day(date),
        season = case_when(
            (month == 3 & day >= 21) |
                (month %in% c(4, 5)) |
                (month == 6 & day <= 20) ~
                "Spring",
            (month == 6 & day >= 21) |
                (month %in% c(7, 8)) |
                (month == 9 & day <= 20) ~
                "Summer",
            (month == 9 & day >= 21) |
                (month %in% c(10, 11)) |
                (month == 12 & day <= 20) ~
                "Autumn",
            TRUE ~ "Winter"
        )
    ) |>
    group_by(year, season) |>
    summarise(prec_seasonal = sum(prec, na.rm = TRUE)) |>
    ungroup() |>
    mutate(
        season_num = case_when(
            season == "Spring" ~ 1,
            season == "Summer" ~ 2,
            season == "Autumn" ~ 3,
            season == "Winter" ~ 4
        ),
        date = as.Date(paste(year, season_num * 3 - 1, "15", sep = "-")) # mid-season
    )

ts_seasonal <- ts(data_seasonal$prec_seasonal, frequency = 4)
decompose(ts_seasonal, type = "additive") |> plot()


# %% 4. Estatisticas básicas

#%% 4.1  Média e desvio padrão móvel ------

# A média móvel é uma técnica simples para suavizar séries temporais
# e identificar tendências subjacentes. Ela calcula a média dos valores
# em uma janela deslizante ao longo do tempo. O desvio padrão móvel mede
# a variabilidade dos valores dentro dessa janela. Ambos são úteis para
# analisar séries temporais, destacando tendências e flutuações.

data_10 %>%
    arrange(date) %>%
    mutate(
        moving_avg = zoo::rollmean(prec, k = 30, fill = NA, align = "right"),
        moving_sd = zoo::rollapply(
            prec,
            width = 30,
            FUN = sd,
            fill = NA,
            align = "right"
        )
    ) |>
    ggplot(aes(x = date)) +
    geom_line(aes(y = prec), color = "lightblue", size = 1) +
    geom_line(aes(y = moving_avg), color = "blue", size = 1) +
    geom_ribbon(
        aes(ymin = moving_avg - moving_sd, ymax = moving_avg + moving_sd),
        fill = "blue",
        alpha = 0.2
    ) +
    labs(x = "Data", y = "Chuva (mm)") +
    theme_minimal() +
    theme(text = element_text(size = 30))

# %% 4.2 Autocorrelação ------

# A autocorrelação mede a correlação entre uma série temporal e uma
# versão defasada dela mesma. Ela ajuda a identificar padrões repetitivos
# e a dependência temporal nos dados.

ts(data_monthly$prec_monthly, frequency = 12) |>
    stats::acf(lag.max = 48, main = "Autocorrelação") # ACF

# A autocorrelação parcial (PACF) mede a correlação entre uma série temporal
# e uma versão defasada dela mesma, removendo os efeitos das defasagens
# intermediárias. Ela ajuda a identificar a ordem de um modelo AR
# (AutoRegressivo) apropriado para a série temporal.
ts(data_monthly$prec_monthly, frequency = 12) |>
    stats::acf(lag.max = 48, main = "Autocorrelação", type = "partial") # PACF - parcial

#%% 6. Modelos estocásticos ------

# %% 6.1 white noise ------
# White noise é uma série temporal composta por valores aleatórios
# independentes e identicamente distribuídos, com média zero e variância
# constante. É usado como um modelo básico para ruído em séries temporais.
set.seed(123)
white_noise <- rnorm(1000, mean = 0, sd = 1) # 100 valores aleatórios
ts_white_noise <- ts(white_noise)
plot(ts_white_noise, main = "White Noise", ylab = "Value", xlab = "Time")

acf(ts_white_noise, lag.max = 30, main = "ACF of White Noise")
acf(ts_white_noise, lag.max = 30, main = "ACF of White Noise", type = "partial")

# %% 6.2 Random Walk ------
# Random walk é uma série temporal onde cada valor é a soma do valor
# anterior e um termo de ruído aleatório. É usado para modelar processos
# que evoluem de forma imprevisível ao longo do tempo.
set.seed(123)
random_steps <- rnorm(1000, mean = 0, sd = 1)
random_walk <- cumsum(random_steps) # soma cumulativa
ts_random_walk <- ts(random_walk)
plot(ts_random_walk, main = "Random Walk", ylab = "Value", xlab = "Time")

acf(ts_random_walk, lag.max = 30, main = "ACF of Random Walk")
acf(ts_random_walk, lag.max = 30, main = "ACF of Random Walk", type = "partial")

# %% 6.3 Moving Average (AM) ------

# Moving Average (MA) é um modelo onde o valor atual da série temporal
# é uma média ponderada dos erros passados. É usado para modelar séries
# temporais com dependência de curto prazo.

set.seed(123)
ma_order <- 2 # ordem do modelo MA
ma_coeffs <- c(0.5, 0.3) # coeficientes
white_noise_ma <- rnorm(1000, mean = 0, sd = 1)
# Use stats::filter() para convolução, não dplyr::filter()
ma_series <- stats::filter(
    white_noise_ma,
    ma_coeffs,
    method = "convolution",
    sides = 1
) |>
    na.omit() # remove NA values created by convolution

ts_ma <- ts(ma_series)
plot(ts_ma, main = "Moving Average (MA)", ylab = "Value", xlab = "Time")

acf(ts_ma, lag.max = 30, main = "ACF of MA")
acf(ts_ma, lag.max = 30, main = "PACF of MA", type = "partial")

# %% 6.4 Autoregressive (AR) ------
# Autoregressive (AR) é um modelo onde o valor atual da série temporal
# é uma combinação linear dos valores passados. É usado para modelar séries
# temporais com dependência de longo prazo.

set.seed(123)
ar_coeffs <- c(0.7, -0.3) # coeficientes
white_noise_ar <- rnorm(1000, mean = 0, sd = 1)
ar_series <- numeric(length(white_noise_ar))
for (t in (length(ar_coeffs) + 1):length(white_noise_ar)) {
    ar_series[t] <- sum(
        ar_coeffs *
            rev(ar_series[(t - length(ar_coeffs)):(t - 1)])
    ) +
        white_noise_ar[t]
}
ts_ar <- ts(ar_series)
plot(ts_ar, main = "Autoregressive (AR)", ylab = "Value", xlab = "Time")

acf(ts_ar, lag.max = 30, main = "ACF of AR")
acf(ts_ar, lag.max = 30, main = "PACF of AR", type = "partial")

# %% 6.5 ARMA (Autoregressive Moving Average) ------
# ARMA combina os modelos AR e MA, onde o valor atual da série temporal
# depende tanto dos valores passados quanto dos erros passados. É usado
# para modelar séries temporais com dependência de curto e longo prazo.

set.seed(123)
ar_coeffs_arma <- c(0.5, -0.3) # coeficientes AR
ma_coeffs_arma <- c(0.4, -0.2) # coeficientes MA
white_noise_arma <- rnorm(1000, mean = 0, sd = 1)
arma_series <- numeric(length(white_noise_arma))
for (t in (max(length(ar_coeffs_arma), length(ma_coeffs_arma)) + 1):length(
    white_noise_arma
)) {
    arma_series[t] <- sum(
        ar_coeffs_arma *
            rev(arma_series[(t - length(ar_coeffs_arma)):(t - 1)])
    ) +
        sum(
            ma_coeffs_arma *
                rev(white_noise_arma[(t - length(ma_coeffs_arma)):(t - 1)])
        ) +
        white_noise_arma[t]
}
ts_arma <- ts(arma_series)
plot(ts_arma, main = "ARMA", ylab = "Value", xlab = "Time")

acf(ts_arma, lag.max = 30, main = "ACF of ARMA")
acf(ts_arma, lag.max = 30, main = "PACF of ARMA", type = "partial")

# %% 6.6 ARIMA (Autoregressive Integrated Moving Average) ------
# ARIMA é uma extensão do modelo ARMA que inclui uma etapa de diferenciação
# para tornar a série temporal estacionária. É usado para modelar séries
# temporais não estacionárias com dependência de curto e longo prazo.

set.seed(123)
ar_coeffs_arima <- c(0.5) # coeficientes AR
ma_coeffs_arima <- c(0.4) # coeficientes MA
white_noise_arima <- rnorm(1000, mean = 0, sd = 1)
arima_series <- numeric(length(white_noise_arima))
# Diferenciação de ordem 1
diff_series <- c(0, diff(white_noise_arima))
for (t in (max(length(ar_coeffs_arima), length(ma_coeffs_arima)) + 1):length(
    diff_series
)) {
    arima_series[t] <- sum(
        ar_coeffs_arima *
            rev(arima_series[(t - length(ar_coeffs_arima)):(t - 1)])
    ) +
        sum(
            ma_coeffs_arima *
                rev(diff_series[(t - length(ma_coeffs_arima)):(t - 1)])
        ) +
        diff_series[t]
}
ts_arima <- ts(arima_series)
plot(ts_arima, main = "ARIMA", ylab = "Value", xlab = "Time")

acf(ts_arima, lag.max = 30, main = "ACF of ARIMA")
acf(ts_arima, lag.max = 30, main = "PACF of ARIMA", type = "partial")

# %% 6.6A - Utilizando a função arima
