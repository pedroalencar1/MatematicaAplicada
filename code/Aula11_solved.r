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
