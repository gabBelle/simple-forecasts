#' @title Calculate real values exchange rate
#' @name cambio_real
#'
#' @description A função calcula o câmbio ajustado pela inflação das moedas computadas em sua taxa, câmbio real.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série de câmbio limpa e organizada;
#' @param df_ipca_dom Dataframe contendo a série de inflação da moeda doméstica;
#' @param df_ipc_int Dataframe contendo a série de inflação da moeda estrangeira;
#' @param mes_base Ano e mês base para o câmbio real.
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' {date}: Data da observação:
#' {vl}: valor da observação;
#'
#' @return Retorna um dataframe contendo o câmbio real.
#'
#' @examples
#' cambio_real(real_usd, #BRL/USD
#'             ipca_br,
#'             ipc_us,
#'             '2022-01-01'
#'             ))
#'
#' @export

cambio_real <- function(df, df_ipc_dom, df_ipc_int, mes_base) {

  mes_base = base::as.Date(mes_base)

  real <- df %>%
    dplyr::rename(cambio = vl) %>%
    dplyr::mutate(var_cambio = cambio/dplyr::lag(cambio)) %>%
    dplyr::select(-forecast) %>%
    dplyr::full_join(df_ipc_dom %>%
                       dplyr::mutate(vl = vl/dplyr::lag(vl)) %>%
                       dplyr::rename(ipc_dom = vl) %>%
                       dplyr::select(-forecast)) %>%
    dplyr::full_join(df_ipc_int %>%
                       dplyr::mutate(vl = vl/dplyr::lag(vl)) %>%
                       dplyr::rename(ipc_int = vl) %>%
                       dplyr::select(-forecast)) %>%
    stats::na.omit()

  cambio_base <- real[real$date == mes_base,]$cambio

  forecast_dt <- base::min(min(filter(df, forecast)$date),
                           base::min(filter(df_ipc_dom, forecast)$date),
                           base::min(filter(df_ipc_int, forecast)$date))
  result <- real %>%
    dplyr::mutate(acum_cambio = base::cumprod(var_cambio),
           acum_ipc_dom = base::cumprod(ipc_dom),
           acum_ipc_int = base::cumprod(ipc_int)) %>%
    dplyr::mutate(cambio_real = dplyr::case_when(
      date == mes_base ~ cambio_base,
      date > mes_base ~ cambio_base * acum_cambio * (acum_ipc_int/acum_ipc_dom),
      date < mes_base ~ cambio_base / (acum_cambio * (acum_ipc_int/acum_ipc_dom)))) %>%
    dplyr::select(date, cambio_real) %>%
    dplyr::rename(vl = cambio_real) %>%
    dplyr::mutate(forecast = date >= forecast_dt)

  return(result)
}

