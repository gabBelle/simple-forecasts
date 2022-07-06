#' @title Drift forecast with historical tendency.
#' @name drift_hist
#'
#' @description Incorpora tendência na projeção de uma série.
#' Para calcular a tendência, utiliza a tendência linear presente no histórico da série.
#'
#' @author Gabriel Bellé
#'
#' @param df_forecast Dataframe contendo a série a ser projetada;
#' @param nmean Restrição de anos para uso do histórico da série.
#'
#' @details
#' O @param df_forecast de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#' \code{forecast}: bool indicando se a observação é uma projeção.
#'
#' O @param nmean indica quantos anos do histórico serão utilizados para calcular a tendência linear.
#' Se nenhum valor fornecido, utilizará o histórico completo.
#'
#' @return O retorno é um df, com as colunas de date, indo até o fim da projeção,
#' e uma coluna 'drift', indicando o valor da tendência para aquele período.
#'
#' @examples
#' \dontrun{
#' drift_hist(df_forecast = cleaned_df,
#'            nmeans = 5)
#' }
#'
#' @export

drift_hist <- function(df_forecast,
                       nmeans = 1) {

  df_hist <- df_forecast %>%
    dplyr::filter(!forecast)

  if (is.null(nmeans)) {
    date_filt = df_hist %>%
      dplyr::mutate(
        year = format(date, '%Y') %>%
          as.numeric(),
        month = format(date, '%m') %>%
          as.numeric()
      )

    month_last_date <- nmeans %>%
      dplyr::filter(date == max(date)) %>%
      purrr::pluck('month')

    date_filt <- date_filt %>%
      dplyr::filter(year == min(year)) %>%
      dplyr::filter(month == month_last_date) %>%
      purrr::pluck('date')

    df_drift_default <- df_hist %>%
      dplyr::filter(date >= date_filt)

  } else {
    df_drift_default <- df_hist %>%
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
    dplyr::mutate(drift = ifelse(forecast, drift_vl, 0),
           drift = cumsum(drift)) %>%
    dplyr::select(c(date, vl, drift, forecast))

  return(df_forecast)
}
