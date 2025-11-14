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

# %% 0.1 instalando o pacote rdwd (se necessário) ------
#' este pacote permite baixar dados hidrológicos do serviço meteorológico
#' alemão (DWD)

# Verifica se options(repos = c(CRAN = "https://cran.rstudio.com/"))

# options(repos = c(CRAN = "https://cran.rstudio.com/"))
# install.packages(c("rdwd", "rkt"))
library(rdwd) # Carrega o pacote RDWD
library(rkt)
library(forecast)


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

#%% 5. Modelos estocásticos ------

# %% 5.1 white noise ------
# White noise é uma série temporal composta por valores aleatórios
# independentes e identicamente distribuídos, com média zero e variância
# constante. É usado como um modelo básico para ruído em séries temporais.
set.seed(123)
white_noise <- rnorm(1000, mean = 0, sd = 1) # 100 valores aleatórios
ts_white_noise <- ts(white_noise)
plot(ts_white_noise, main = "White Noise", ylab = "Value", xlab = "Time")

acf(ts_white_noise, lag.max = 30, main = "ACF of White Noise")
acf(ts_white_noise, lag.max = 30, main = "ACF of White Noise", type = "partial")

# %% 5.2 Random Walk ------
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

# %% 5.3 Moving Average (AM) ------

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

# %% 5.4 Autoregressive (AR) ------
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

# %% 5.5 ARMA (Autoregressive Moving Average) ------
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

# %% 5.6 ARIMA (Autoregressive Integrated Moving Average) ------
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

# %% 5.7. Modelagem com ARIMA ------

# Frequência mensal
data_monthly <- data |>
    mutate(year = lubridate::year(date), month = lubridate::month(date)) |>
    group_by(year, month) |>
    summarise(temp = mean(temp, na.rm = TRUE)) |>
    ungroup() |>
    mutate(date = as.Date(paste(year, month, "15", sep = "-"))) # mid-month

ts_monthly <- ts(data_monthly$temp, frequency = 12)

# decompose a série temporal
decompose(ts_monthly, type = "additive") |> plot()

# ajusta o modelo ARIMA automaticamente

# para testes iniciais:
arima_model <- auto.arima(ts_monthly)

# Pode-se adicionar parametros adicionais para melhorar o ajuste:
# arima_model <- auto.arima(ts_monthly,
#     lambda = "auto", # Box-Cox transformation
#     seasonal = TRUE, # Seasonal component
#     seasonal.test = "seas", # Test for seasonal differencing
#     stepwise = FALSE, # Use full search
#     approximation = FALSE, # No approximation
#     parallel = TRUE, num.cores = 4 # Parallel processing - accelerates model
# )

summary(arima_model)
# diagnostico do modelo
checkresiduals(arima_model)
# residuais parecem ser ruído branco (white noise)?
# Se sim, o modelo é adequado.
# repare que os residuos apresentao distribuição normal (histograma) e média 0
# também não apresentam autocorrelação (ACF).
ts_residuals <- ts(residuals(arima_model))
acf(ts_residuals, lag.max = 50, main = "ACF of ARIMA")
# pequena correlação em lags multiplos de 12 (sazonalidade anual)

# teste de estacionariedade dos resíduos - Augmented Dickey-Fuller Test
tseries::adf.test(residuals(arima_model))

# parametros do modelo
arimaorder(arima_model)
# (p, d, q)(P, D, Q)[m]

# Estes parametros podem ser usados para ajustar manualmente o modelo ARIMA:
arima_model_manual <- arima(
    ts_monthly,
    order = c(1, 0, 0), # (p, d, q)
    seasonal = list(order = c(1, 1, 0), period = 12) # (P, D, Q)[m]
)

# Previsões
forecast_values <- forecast(arima_model, h = 12) # Forecast next 12 months

# Plot customizado mostrando apenas os últimos 5 anos + previsão
plot(
    forecast_values,
    xlim = c(time(ts_monthly)[1518], max(time(forecast_values$mean))),
    main = "Previsão de Temperatura - Últimos 5 anos + 12 meses",
    xlab = "Tempo",
    ylab = "Temperatura (°C)"
)


# Exercicio:
#' Ajuste o modelo arima para o periodo de 1893 a 2000 e faça previsões
#' para os anos seguintes. Compare as previsões com os dados observados
#' de 2001 a 2024.

# %% 6. Análise de Tendência ------
# Mann-Kendall test for trend

#temp média anual e prec acumulada anual
data_year <- data |>
    mutate(
        year = lubridate::year(date),
    ) |>
    group_by(year) |>
    summarise(
        prec = sum(prec, na.rm = TRUE),
        temp = mean(temp, na.rm = TRUE)
    ) |>
    ungroup()


# %% 6.1 Mann-Kendall test for precipitation
mk_prec <- rkt(date = data_year$year, y = data_year$prec)
print(mk_prec)

slope_prec <- mk_prec$B
# %% plotando a tendência de precipitação anual ------
ggplot(data_year, aes(x = year, y = prec)) +
    geom_point() +
    geom_abline(
        intercept = mean(data_year$prec[1:6]) - slope_prec * data_year$year[1],
        slope = slope_prec,
        color = "red",
        size = 1
    ) +
    labs(
        title = "Annual Precipitation with Trend Line",
        x = "Year",
        y = "Annual Precipitation (mm)"
    ) +
    theme_minimal() +
    theme(text = element_text(size = 20))

# comparando com modlo linear simples
lm_prec <- lm(prec ~ year, data = data_year)
slope_lm <- lm_prec$coefficients['year']

print(paste("Mann-Kendall slope:", slope_prec, "Linear model slope:", slope_lm))
print(paste(
    "Difference (%):",
    round((slope_prec - slope_lm) / slope_prec * 100, 3)
))

#%% 6.2 Mann-Kendall test for temperature
mk_temp <- rkt(date = data_year$year, y = data_year$temp)
print(mk_temp)

slope_temp <- mk_temp$B

# %% plotando a tendência de precipitação anual ------
ggplot(data_year, aes(x = year, y = temp)) +
    geom_point() +
    geom_abline(
        intercept = mean(data_year$temp[1:6]) - slope_temp * data_year$year[1],
        slope = slope_temp,
        color = "red",
        size = 1
    ) +
    labs(
        title = "Annual Temperature with Trend Line",
        x = "Year",
        y = "Annual Mean Temp (°C)"
    ) +
    theme_minimal() +
    theme(text = element_text(size = 20))

# comparando com modlo linear simples
lm_temp <- lm(temp ~ year, data = data_year)
slope_lm <- lm_temp$coefficients['year']

print(paste("Mann-Kendall slope:", slope_temp, "Linear model slope:", slope_lm))
print(paste(
    "Difference (%):",
    round((slope_temp - slope_lm) / slope_temp * 100, 3)
))

# %% 6.3 seasonal Mann-Kendall test ------
# seasonal Mann-Kendall test for monthly data

data_month <- data |>
    mutate(
        year = lubridate::year(date),
        month = lubridate::month(date)
    ) |>
    group_by(month, year) |>
    summarise(
        prec = sum(prec, na.rm = TRUE),
        temp = mean(temp, na.rm = TRUE)
    ) |>
    ungroup() |>
    data.frame() |>
    mutate(year = year + (month - 1) / 12) # fractional year for monthly data

mk_temp_seasonal <- rkt(
    date = data_month$year,
    y = data_month$temp,
    block = data_month$month,
    correct = TRUE
)

# %% visualizando os resultados
print(mk_temp_seasonal)

slope_season <- mk_temp_seasonal$B

ggplot(data_year, aes(x = year, y = temp)) +
    geom_point() +
    geom_abline(
        intercept = mean(data_year$temp[1:6]) -
            slope_season * data_year$year[1],
        slope = slope_season,
        color = "red",
        size = 1
    ) +
    labs(
        title = "Annual Precipitation with Trend Line",
        x = "Year",
        y = "Annual Precipitation (mm)"
    ) +
    theme_minimal() +
    theme(text = element_text(size = 20))

#' o teste de Mann-Kendall sazonal leva em consideração a
#' variação sazonal nos dados, o que pode fornecer uma análise
#' mais precisa da tendência subjacente. Ao comparar os resultados
#' do teste de Mann-Kendall sazonal com o teste padrão, é possível
#' observar diferenças na magnitude e significância da tendência,
#' destacando a importância de considerar a sazonalidade ao analisar
#' séries temporais climáticas.
#'
#' EXERCÍCIO: Aplique o teste de Mann-Kendall sazonal para os dados
#' de precipitação e compare os resultados com o teste padrão.
#'
#' NOTA: Existe ainda a variante do teste de Mann-Kendall regional, em que
#' vários locais são considerados simultaneamente para avaliar tendências
#' regionais. Esta variante é particularmente útil em estudos climáticos
#' que envolvem múltiplas estações meteorológicas em uma região geográfica.

# %% 7. Bonus - Análise de Breakpoints ------

library(strucchange)

# get data and timeseries
temp_year <- data |>
    filter(date >= as.Date("1893-01-01") & date <= as.Date("2024-12-31")) |>
    mutate(
        year = lubridate::year(date),
    ) |>
    group_by(year) |>
    summarise(temp = mean(temp, na.rm = TRUE)) |>
    ungroup()

ts_temp <- ts(temp_year$temp, start = c(1893))
plot(ts_temp)


# h = mínimo de observações entre breakpoints
# breaks = número máximo de breakpoints a serem testados
bp <- breakpoints(ts_temp ~ 1, h = 5, breaks = 5)
summary(bp)
plot(bp)

# interpretando o plot acima:
# A recomendação estatistica é escolher o numero de breakpoints
# que minimiza o BIC (Bayesian Information Criterion). Contudo, é comum
# recomendar-se a escolha do numero de breakpoints em que BIC e RSS cruzem-se.
# Pelo gráfico, o critério 1 sugere 1 breakpoint, enquanto o critério 2 sugere
# 2 breakpoints.

# %% Caso com 1 breakpoint
# cria um fator para os segmentos
fac_1 <- breakfactor(bp, breaks = 2, label = "seg")

# ajusta o modelo linear com os segmentos
fm_1 <- lm(ts_temp ~ fac_1)

# intervalos de confiança
ci_temp_1 <- confint(bp, breaks = 2, het.err = FALSE, level = 0.95)

# plot
plot(ts_temp)
lines(ci_temp_1)

# Caso com 2 breakpoints
fac_2 <- breakfactor(bp, breaks = 3, label = "seg")
fm_2 <- lm(ts_temp ~ fac_2)
ci_temp_2 <- confint(bp, breaks = 3, het.err = FALSE, level = 0.95)

plot(ts_temp)
lines(ci_temp_2)

# %% testeando a validade dos modelosn
# 1. Autocorrelation

acf(fm_1$residuals, type = "correlation") # No autocorrelation observed
acf(fm_2$residuals, type = "correlation") # No autocorrelation observed

# 2. Stationarity - Augmented Dickey-Fuller Test
library(tseries)
adf_test_1 <- adf.test(fm_1$residuals, alternative = "stationary")
print(adf_test_1) # p-value < 0.05 indicates stationarity

adf_test_2 <- adf.test(fm_2$residuals, alternative = "stationary")
print(adf_test_2) # p-value < 0.05 indicates stationarity
# note, the adf test indicates that the resivuals are stationary, not that the # series itself is stationary!

adf.test(ts_temp, alternative = "stationary")
# p-value > 0.05 indicates non-stationarity of the original series

# EXERCISE: Apply breakpoint analysis to the precipitation data, min and max
# temperatures and compare the results.
