#' @title Forecasting top down
#' @name sf_topdown
#'
#' @description Realiza a projeção de N séries utilizando uma projeção já feita em outra série.
#' Utiliza o YoY da série que já possui projeção (top) para projetar as demais séries (down).
#' Por manter o mesmo YoY em cada mês para as séries (down), sempre haverá compatibilidade com a série (top).
#' Exp: ao projetar aberturas da PMC/PMS/PIM/PIB, não importa como as aberturas são compostas para chegar no indicador geral,
#' sempre estará compatibilizado.
#'
#' @author Gabriel Bellé
#'
#' @param target_agg Dataframe contendo a série (top) já com projeção;
#' @param ... Dataframe, N dfs que serão projetados utilizando a projeção contida em target_agg
#'
#' @details
#' O @param target_agg deve conter as seguintes colunas:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#' \code{forecast}: bool indicando se o valor é projeção.
#'
#' Os dfs passados em @param ... devem conter as colunas:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação e;
#'
#' deve possuir mesma periodicidade que o @param target_agg.
#'
#' @return Retorna um dataframe com colunas de \code{date}, \code{forecast} e uma
#' coluna de valor para cada série que foi projetada, na mesma ordem em que foi inputada na função.
#'
#' @examples
#' \dontrun{
#' sf_topdown(target_agg = df_pmc_geral,
#'            pmc_construcao, pmc_eletrodomestico, pmc_vestuario) {
#' }
#'
#' @export

sf_topdown <- function(target_agg, ...) {
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
