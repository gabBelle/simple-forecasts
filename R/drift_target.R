#' @title Drift forecast with target values
#' @name drift_target
#'
#' @description Incorpora tendência na projeção de uma série.
#' Para calcular a tendência, utiliza a diferença entre o último dado realizado e o valor alvo para o ano.
#'
#' Atenção! O comportamento dessa função funciona como esperado apenas quando a projeção de entrada foi
#' originada de um método naive (onde a projeção é apenas a repetição do último valor).
#'
#' @author Gabriel Bellé
#'
#' @param df_forecast Dataframe contendo a série a ser projetada;
#' @param target_value Vetor de valores indicando a projeção desejada para final de período;
#' @param trend_type Opcional, linear ou exponential. Utilizado apenas quando target_value é chamado. Padrão é exponential.
#'
#' @details
#' O @param df_forecast de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#' \code{forecast}: bool indicando se a observação é uma projeção.
#'
#' O @param target_value indica o valor para o final de período desejado, idealmete advindo de uma projeção anual.
#' Por exemplo, a projeção anual do LatamFocus aponta 150 para 2023 e 200 para 2024, pode-se preencher:
#' end_projection = '2025-12-01' e target_value = c(150, 200)
#'
#' Isto fará com que a tendência linear seja tal qual respeite os valores de entrada.
#'
#' @param type_drif o valor no parâmetro irá modificar a fórmula empregada para cálculo do drift quando utilizado os valores
#' alvo em target_value. Caso linear, a tendência será linear, caso exponencial, a tendência será exponencial.
#'
#' @return O retorno é um df, com as colunas de date, indo até o fim da projeção,
#' e uma coluna 'drift', indicando o valor da tendência para aquele período.
#'
#' @examples
#' \dontrun{
#' drift_target(df_forecast = cleaned_df,
#'              target_value = c(200, 230, 150, 210, 100))
#' }
#'
#' @export

drift_target <- function(df_forecast,
                         target_value,
                         trend_type = 'exponential') {

  periodicity <- get_periodicity(filter(df_forecast,
                                        !forecast)
                                 )

  df_forecast <- df_forecast %>%
    dplyr::mutate(
      year = format(date, '%Y') %>%
        as.numeric(),
      month = format(date, '%m') %>%
        as.numeric()
    )

  target_value_adj <- check_vector_len(df_forecast = df_forecast,
                                       vector_to_check = target_value)

  last_row_hist <- df_forecast %>%
    dplyr::filter(!forecast) %>%
    tail(1)

  last_vl_hist <- last_row_hist$vl

  last_hist_date <- last_row_hist$date

  first_date_target <- df_forecast %>%
    dplyr::filter(forecast) %>%
    dplyr::filter(year == min(year),
           month == max(month)) %>%
    purrr::pluck('date')

  dist_hist_first_target <- length(
    seq(last_hist_date,
        first_date_target,
        by = periodicity$p_name)
  ) - 1

  depara_year_target <- df_forecast %>%
    dplyr::filter(forecast) %>%
    dplyr::group_by(year) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vl = target_value_adj)

  if(trend_type == 'linear') {
    drift_forecast <- depara_year_target %>%
      dplyr::mutate(
        drift = (vl - lag(vl,1)) / periodicity$p_nmonths,
        drift = ifelse(is.na(drift),
                       (vl - last_hist_vl) / dist_hist_first_target,
                       drift)
      ) %>%
      dplyr::select(c(forecast, year, drift)) %>%
      dplyr::right_join(df_forecast) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(drift = ifelse(forecast, drift, 0),
             drift = cumsum(drift))

    type_drift = 'add'
  } else if(trend_type == 'exponential') {
    drift_forecast <- depara_year_target %>%
      dplyr::mutate(
        drift = (vl - lag(vl))/lag(vl),
        drift = drift + 1,
        drift = drift ^ (1/periodicity$p_nmonths),
        first_year = (vl - last_hist_vl) / last_hist_vl,
        first_year = first_year + 1,
        first_year = first_year ^ (1/dist_hist_first_target),
        drift = ifelse(is.na(drift),
                       first_year,
                       drift)
      ) %>%
      dplyr::select(c(forecast, year, drift)) %>%
      dplyr::right_join(df_forecast) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(drift = ifelse(forecast, drift, 1),
             drift = cumprod(drift))

    type_drift = 'mult'
  }

  drift_forecast <- drift_forecast %>%
    dplyr::select(c(date, vl, drift, forecast))

  return(list(
    df = drift_forecast,
    type_drift = type_drift
  ))
}
