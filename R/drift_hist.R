#' @title Drift forecast with historical tendency.
#' @name drift_hist
#'
#' @description Realiza a projeção de uma série usando tendência naive,
#' calculada com base no histórico da série fornecida.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série a ser projetada;
#' @param end_projection Data indicando fim da projeção;
#' @param nmean Constante numérica;
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' O @param nmean indica quantos anos do histórico serão utilizados para calcular a tendência linear.
#' Se nenhum valor fornecido, utilizará o histórico completo.
#'
#' @return O retorno é um df, com as colunas de date, indo até o fim da projeção,
#' e uma coluna 'drift_forecast', indicando o valor da tendência para aquele período.
#'
#' @examples
#' \dontrun{
#' drift_hist(df = cleaned_df,
#'            end_projection = '2025-12-01',
#'            nmeans = 5)
#' }
#'
#' @export

drift_hist <- function(df,
                  end_projection,
                  nmeans) {

  df_forecast <- expand_series(df, end_projection = end_projection)

  if (is.null(nmeans)) {
    df_drift_default <- df
  } else {
    df_drift_default <- df %>%
      dplyr::filter(date >= max(date)- years(nmeans))
  }

  start_vl <- df_drift_default %>%
    dplyr::filter(date == min(date)) %>%
    purrr::pluck('vl')

  end_vl <- df_drift_default %>%
    dplyr::filter(date == max(date)) %>%
    purrr::pluck('vl')

  len_dt <- df_drift_default %>%
    base::nrow()

  drift_vl <- (end_vl - start_vl) / (len_dt - 1)

  df_forecast <- df_forecast %>%
    mutate(drift_forecast = ifelse(is.na(vl), drift_vl, 0),
           drift_forecast = cumsum(drift_forecast)) %>%
    select(c(date, drift_forecast))

  return(df_forecast)
}
