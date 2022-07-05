#' @title Drift forecast with target values
#' @name drift_target
#'
#' @description Realiza a projeção de uma tendência naive, a qual pode ser acoplada em uma projeção.
#' O drift_target irá utilizar um vetor pré-definido com o valor para final de período desejado e calculará
#'
#' Ex: drift naive, repete o último valor e soma ou multiplica o valor do drift_forecast; ou,
#' seasonal naive with drift, repete o último valor do mesmo mês e soma ou multiplica o valor do drift_forecast.
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
#'              end_projection = '2025-12-01',
#'              target_value = c(200, 230, 150, 210, 100))
#' }
#'
#' @export

drift_target <- function(df,
                         end_projection,
                         target_value) {

  periodicity <- get_periodicity(df)

  df_forecast <- expand_series(df, end_projection = end_projection)  %>%
    dplyr::mutate(year = format(date, '%Y'))

  target_value_adj <- check_vector_len(df_forecast = df_forecast,
                                   vector_to_check = target_value)

  #Dado realizado do último mês do ano disponível
  #Se dado realizado acaba em junho/20, em série mensal,
  #retornar valor de dez/19, por exemplo
  vl_from_last_month <- df %>%
    dplyr::mutate(
      year = format(date, '%Y'),
      month = format(date, '%m')) %>%
    dplyr::filter(month == max(month)) %>%
    dplyr::filter(year == max(year)) %>%
    purrr::pluck('vl')

  #Calcula o valor mensal a ser adicionado como tendência para cada ano,
  #Dado os valores em target_value
  depara_year_target <- df_forecast %>%
    dplyr::filter(forecast) %>%
    dplyr::group_by(year) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vl = target_value_adj,
                  drift_forecast = (vl - lag(vl, 1)) / periodicity$p_nmonths,
                  drift_forecast = ifelse(is.na(drift_forecast),
                                          (vl - vl_from_last_month) / periodicity$p_nmonths,
                                          drift_forecast)
    ) %>%
    dplyr::select(year, drift_forecast)

  #Preenche o df final com a tendência correta para cada ano de projeção
  df_forecast <- df_forecast %>%
    dplyr::left_join(depara_year_target) %>%
    dplyr::mutate(drift_forecast = ifelse(is.na(vl), drift_forecast, 0),
                  drift_forecast = cumsum(drift_forecast)) %>%
    dplyr::select(c(date, drift_forecast))

  return(df_forecast)
}
