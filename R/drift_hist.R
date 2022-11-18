#' @title Drift forecast with historical tendency.
#' @name drift_hist
#'
#' @description Incorpora tendência na projeção de uma série.
#' Para calcular a tendência, utiliza a tendência linear presente no histórico da série.
#'
#' @author Gabriel Bellé
#'
#' @param df_forecast Dataframe contendo a série a ser projetada;
#' @param nyears Restrição de anos para uso do histórico da série.
#'
#' @details
#' O @param df_forecast de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#' \code{forecast}: bool indicando se a observação é uma projeção.
#'
#' O @param nyears indica quantos anos do histórico serão utilizados para calcular a tendência linear.
#' Se nenhum valor fornecido, utilizará o histórico completo.
#'
#' @return O retorno é um df, contendo as colunas de:
#' \code{date}; \code{vl}; \code{drift} e \code{forecast}.
#' As colunas são iguais ao input, com exceção da coluna drift, que possui o valor a ser adicionado na série.
#'
#' @examples
#' \dontrun{
#' drift_hist(df_forecast = cleaned_df,
#'            nyears = 5)
#' }
#'
#' @export

drift_hist <- function(df_forecast,
                       nyears = NULL) {

  if(!all(c('date', 'forecast', 'vl') %in% base::colnames(df_forecast))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }

  df_hist <- df_forecast %>%
    dplyr::filter(!forecast)

  if (base::is.null(nyears)) {
    date_filt = df_hist %>%
      dplyr::mutate(
        year = base::format(date, '%Y') %>%
          base::as.numeric(),
        month = base::format(date, '%m') %>%
          base::as.numeric()
      )

    month_last_date <- date_filt %>%
      dplyr::filter(date == base::max(date)) %>%
      purrr::pluck('month',1)

    date_filt <- date_filt %>%
      dplyr::filter(month == month_last_date) %>%
      dplyr::filter(year == base::min(year)) %>%
      purrr::pluck('date',1)

    df_drift_default <- df_hist %>%
      dplyr::filter(date >= date_filt)

  } else {
    df_drift_default <- df_hist %>%
      dplyr::filter(date >= base::max(date) - lubridate::years(nyears))
  }

  start_vl <- df_drift_default %>%
    dplyr::filter(date == base::min(date)) %>%
    purrr::pluck('vl',1)

  end_vl <- df_drift_default %>%
    dplyr::filter(date == base::max(date)) %>%
    purrr::pluck('vl',1)

  len_dt <- df_drift_default %>%
    base::nrow()

  drift_vl <- (end_vl - start_vl) / (len_dt - 1)

  df_forecast <- df_forecast %>%
    dplyr::mutate(drift = base::ifelse(forecast, drift_vl, 0),
           drift = base::cumsum(drift)) %>%
    dplyr::select(c(date, vl, drift, forecast))

  return(df_forecast)
}
