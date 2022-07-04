#' @title Drift forecast with target values
#' @name drift_target
#'
#' @description Realiza a projeção de uma série usando tendência naive,
#' calculada com base no valor final desejado para o fim de período do ano.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série a ser projetada;
#' @param end_projection Data indicando fim da projeção;
#' @param target_value Vetor de valores indicando a projeção desejada para final de período
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' O @param target_value indica o valor para o final de período desejado, idealmete advindo de uma projeção anual.
#' Por exemplo, a projeção anual do LatamFocus aponta 150 para 2023 e 200 para 2024, pode-se preencher:
#' end_projection = '2025-12-01' e target_value = c(150, 200)
#'
#' Isto fará com que a tendência linear seja tal qual respeite os valores de entrada.
#'
#' @return O retorno é um df, com as colunas de date, indo até o fim da projeção,
#' e uma coluna 'drift_forecast', indicando o valor da tendência para aquele período.
#'
#' @examples
#' \dontrun{
#' drift_target(df = cleaned_df,
#'            end_projection = '2025-12-01',
#'            target_value = c(200, 230))
#' }
#'
#' @export

drift_target <- function(df,
                         end_projection,
                         target_value) {

  periodicity <- get_periodicity(df)

  df_forecast <- expand_series(df, end_projection = end_projection)

  df_forecast <- df_forecast %>%
    dplyr::mutate(year = format(date, '%Y'))

  n_years <- df_forecast %>%
    dplyr::filter(is.na(vl)) %>%
    purrr::pluck('year') %>%
    dplyr::n_distinct()

  if(length(target_value) > n_years) {
    warning('Foram fornecidos mais valores em target_value que o número de anos da projeção!')

    target_value <- target_value[1:n_years]

  } else if (length(target_value) < n_years) {
    last_target <- target_value[-1]
    target_value <- target_value[1:n_years]
    target_value <- ifelse(is.na(target_value), last_target, target_value)
  }

  #Ultimo valor do dado realizado
  last_vl_hist <- df %>%
    dplyr::filter(date == max(date)) %>%
    purrr::pluck('vl')

  #Data que que houve o último dado realizado
  last_date_hist <- df %>%
    dplyr::filter(date == max(date)) %>%
    purrr::pluck('date')

  #Último mês/ano do primeiro ano de projeção
  last_date_first_year_forecast <- df_forecast %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(forecast) %>%
    dplyr::filter(year == min(year)) %>%
    dplyr::filter(date == max(date)) %>%
    purrr::pluck('date')

  #Quantidade de meses a serem projetados
  n_months <- length(
    seq(last_date_hist,
        last_date_first_year_forecast,
        by = periodicity$p_name)
  )

  #Calcula o valor mensal a ser adicionado como tendência para cada ano,
  #Dado os valores em target_value
  depara_year_target <- df_forecast %>%
    dplyr::filter(forecast) %>%
    dplyr::group_by(year) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vl = target_value,
                  drift_forecast = (vl - lag(vl, 1)) / periodicity$p_nmonths,
                  drift_forecast = ifelse(is.na(drift_forecast),
                                          (vl - last_vl_hist) / (n_months - 1),
                                          drift_forecast)
    ) %>%
    dplyr::select(year, drift_forecast)

  #Preenche o df final com a tendência correta para cada ano de projeção
  df_forecast <- df_forecast %>%
    dplyr::left_join(depara_year_target) %>%
    dplyr::mutate(drift_forecast = ifelse(is.na(vl), drift_forecast, 0),
                  drift_forecast = cumsum(drift_forecast),
                  vl = ifelse(is.na(vl), last_vl_hist, vl),
                  vl = vl + drift_forecast) %>%
    dplyr::select(c(date, drift_forecast))

  return(df_forecast)
}
