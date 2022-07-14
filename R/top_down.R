#' @title Average of period forecasting
#' @name aop_forecast
#'
#' @description A função projeta uma série a partir de um projeção anual contendo média
#' para o período.
#' O método é manter o mesmo YoY que a projeção anual, mas mantendo a sazonalidade.
#' O YoY médio é aplicado a cada um dos meses a ser projetado, resultando em um valor médio igual ao alvo.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada;
#' @param end_forecast Data (YYYY-MM-DD) contendo o último mês a ser projetado;
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
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método,
#' e com a coluna forecast indicando quais observações são projeção.
#'
#' @examples
#' \dontrun{
#' target_aop <- function(df,
#'                        end_forecast = '2026-12-01',
#'                        target_aop = c(11.5,10.5,10.1,9.8,9.5)) {
#' }
#'
#' @export

top_down <- function(target_agg, ...) {
  df_down <- list(...) %>%
    purrr::reduce(left_join, by = 'date')

  periodicity <- get_periodicity(target_agg)

  df_yoy <- target_agg %>%
    rename(target_vl = vl) %>%
    mutate(yoy = target_vl / lag(target_vl, periodicity$p_nmonths) - 1) %>%
    filter(date >= min(df_down$date)) %>%
    left_join(df_down, by = 'date')

  df_yoy['forecast'] <- ifelse(
    is.na(rowSums(df_yoy[5:ncol(df_yoy)])), T, F)

  df_forecast <- df_yoy

  for (dt in filter(df_forecast, forecast)$date) {
    forecast_vl <- df_forecast %>%
      filter(date <= dt) %>%
      mutate(across(c(5:ncol(df_forecast)), ~ifelse(is.na(.x),
                                                    lag(.x, periodicity$p_nmonths)*(1+yoy),
                                                    .x))) %>%
      filter(date == dt)

   df_forecast[df_forecast['date'] == dt,] <- forecast_vl
  }

  df_forecast <- df_forecast %>%
    select(-yoy) %>%
    relocate(forecast, .after = 'date')

  return(df_forecast)
}

