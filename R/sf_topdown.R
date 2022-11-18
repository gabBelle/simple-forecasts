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
#' {date}: Data da observação:
#' {vl}: valor da observação;
#' {forecast}: bool indicando se o valor é projeção.
#'
#' Os dfs passados em @param ... devem conter as colunas:
#' {date}: Data da observação:
#' {vl}: valor da observação e;
#'
#' deve possuir mesma periodicidade que o @param target_agg.
#'
#' @return Retorna um dataframe com colunas de \code{date}, \code{forecast} e uma
#' coluna de valor para cada série que foi projetada, na mesma ordem em que foi inputada na função.
#'
#' @examples
#' sf_topdown(target_agg = df_pmc_geral,
#'            pmc_construcao, pmc_eletrodomestico, pmc_vestuario)
#'
#' @export

sf_topdown <- function(target_agg, ...) {
  df_down <- list(...) %>%
    purrr::reduce(left_join, by = 'date')

  periodicity <- get_periodicity(target_agg)

  df_yoy <- target_agg %>%
    dplyr::rename(target_vl = vl) %>%
    dplyr::mutate(yoy = target_vl / dplyr::lag(target_vl, periodicity$p_nmonths) - 1) %>%
    dplyr::filter(date >= base::min(df_down$date)) %>%
    dplyr::left_join(df_down, by = 'date')

  df_yoy['forecast'] <- base::ifelse(
    base::is.na(base::rowSums(df_yoy[5:ncol(df_yoy)])), T, F)

  df_forecast <- df_yoy

  for (dt in dplyr::filter(df_forecast, forecast)$date) {
    forecast_vl <- df_forecast %>%
      dplyr::filter(date <= dt) %>%
      dplyr::mutate(across(c(5:ncol(df_forecast)), ~base::ifelse(base::is.na(.x),
                                                                 dplyr::lag(.x, periodicity$p_nmonths)*(1+yoy),
                                                                 .x))) %>%
      dplyr::filter(date == dt)

   df_forecast[df_forecast['date'] == dt,] <- forecast_vl
  }

  df_forecast <- df_forecast %>%
    dplyr::select(-yoy) %>%
    dplyr::relocate(forecast, .after = 'date')

  return(df_forecast)
}

