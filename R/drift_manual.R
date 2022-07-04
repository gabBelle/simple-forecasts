#' @title Drift forecast with pre-defined MoM variation.
#' @name drift_manual
#'
#' @description Realiza a projeção de uma tendência naive, a qual pode ser acoplada em uma projeção.
#' O drift_manual irá utilizar um vetor pré-definido para calcular a tendência futura.
#'
#' Ex: drift naive, repete o último valor e soma ou multiplica o valor do drift_forecast; ou,
#' seasonal naive with drift, repete o último valor do mesmo mês e soma ou multiplica o valor do drift_forecast.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série a ser projetada;
#' @param end_projection Data indicando fim da projeção;
#' @param manual_drift Opcional, vetor de valores numéricos indicando drift em %;
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' O @param manual_drift deve conter um vetor numérico onde cada constante
#' indica a variação mensal desejada na projeção para cada ano. Caso o tamanho seja menor que
#' a quantidade de anos a ser projetada, repetirá-se o último valor colocado.
#' Ex: manual_drift = c(0.1, 0.1, 0.15, 0.2).
#' Assim, será utilizado 0.1% nos dois primeiros anos, 0.15% no terceiro, 0.2% no quarto e nos anos seguintes,
#' até o fim do horizonte da projeção.
#'
#' Atenção: este parâmetro faz com que a tendência seja exponencial. O aumento em t+1 é uma % do valor em t + valor em t.
#'
#' @return O retorno é um df, com as colunas de date, indo até o fim da projeção,
#' e uma coluna 'drift_forecast', indicando o valor da tendência para aquele período.
#'
#' @examples
#' \dontrun{
#' drift_manual(df = cleaned_df,
#'              end_projection = '2025-12-01',
#'              manual_drift = c(0.1, 0.15))
#' }
#'
#' @export

drift_manual <- function(df,
                         end_projection,
                         manual_drift,
                         n_months = NULL) {

  df_forecast <- expand_series(df, end_projection = end_projection) %>%
    dplyr::mutate(year = format(date, '%Y'))

  manual_drift_adj <- check_vector_len(df_forecast = df_forecast,
                                       vector_to_check = manual_drift)

  depara_year_manual <- df_forecast %>%
    dplyr::filter(forecast) %>%
    dplyr::group_by(year) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(drift_forecast = manual_drift_adj)  %>%
    dplyr::select(year, drift_forecast)

  df_forecast <- df_forecast %>%
    dplyr::left_join(depara_year_manual) %>%
    dplyr::mutate(
      drift_forecast = drift_forecast / 100 + 1,
      drift_forecast = ifelse(forecast, drift_forecast, 1),
      drift_forecast = cumprod(drift_forecast)) %>%
    dplyr::select(c(date, drift_forecast))

  return(df_forecast)

}
