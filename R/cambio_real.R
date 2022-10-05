#' @title Calculate yoy from target value
#' @name calc_yoy
#'
#' @description A função realiza a conversão de um target em valores, mesma unidade de medida que o dataframe de interesse,
#' para valores em Year over Year (YoY).
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada;
#' @param target_aop Vetor de valores indicando a projeção desejada para média de período;
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#'
#' O @param target_aop indica o valor para média de período desejado, idealmete advindo de uma projeção anual.
#' Por exemplo, a projeção anual do LatamFocus aponta média de 15% para 2022 e 12% para 2023. Pode-se preencher:
#' target_aop = c(0.15,0.12)
#'
#' @return Retorna um vetor de valores de mesmo cumprimento de @param target_aop,
#' porém com os valores representando o YoY para ser aplicado nos meses do período.
#'
#' @examples
#' \dontrun{
#' calc_yoy <- function(df,
#'                      target_aop = c(11.5,10.5,10.1,9.8,9.5)) {
#' }
#'
#' @export

cambio_real <- function(df, df_ipc_dom, df_ipc_int, mes_base) {

  mes_base = as.Date(mes_base)

  real <- df %>%
    rename(cambio = vl) %>%
    mutate(var_cambio = cambio/lag(cambio)) %>%
    select(-forecast) %>%
    full_join(df_ipc_dom %>%
                mutate(vl = vl/lag(vl)) %>%
                rename(ipc_dom = vl) %>%
                select(-forecast)) %>%
    full_join(df_ipc_int %>%
                mutate(vl = vl/lag(vl)) %>%
                rename(ipc_int = vl) %>%
                select(-forecast)) %>%
    na.omit()

  cambio_base <- real[real$date == mes_base,]$cambio *
    (real[real$date == mes_base, ]$ipc_int/real[real$date == mes_base,]$ipc_dom)

  forecast_dt <- min(min(filter(df, forecast)$date),
                     min(filter(df_ipc_dom, forecast)$date),
                     min(filter(df_ipc_int, forecast)$date))
  result <- real %>%
    mutate(acum_cambio = cumprod(var_cambio),
           acum_ipc_dom = cumprod(ipc_dom),
           acum_ipc_int = cumprod(ipc_int)) %>%
    mutate(cambio_real = case_when(
      date == mes_base ~ cambio_base,
      date > mes_base ~ cambio_base * acum_cambio * (acum_ipc_int/acum_ipc_dom),
      date < mes_base ~ cambio_base / (acum_cambio * (acum_ipc_int/acum_ipc_dom)))) %>%
    select(date, cambio_real) %>%
    rename(vl = cambio_real) %>%
    mutate(forecast = date >= forecast_dt)

  return(result)
}

