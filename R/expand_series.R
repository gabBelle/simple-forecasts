#' @title Expand date span in a time series
#' @name expand_series
#'
#' @description Aumenta o vetor de date de um dataframe, inputando NAs na coluna de observação,
#' para ser preenchida por uma função de forecast.
#'
#' @param df DataFrame contendo uma linha com data e observação da série;
#' @param end_forecast Date informando quando a projeção encerra.
#'
#' @author Gabriel Bellé
#'
#' @details O input deve ser um df contendo pelo as colunas:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' @return Retorna o mesmo df de input, mas com o período de datas aumentado de acordo com
#' o fim da projeção fornecida.
#'
#' @examples
#' \dontrun{
#' expand_series(df = df_series,
#'               end_forecast = '2026-12-01')
#' }
#'
#' @export

expand_series <- function(df, end_forecast) {

  if(!all(c('date', 'vl') %in% base::colnames(df))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }

  periodicity <- get_periodicity(df)

  df <- df %>%
    dplyr::mutate(forecast = F) %>%
    dplyr::bind_rows(
      dplyr::tibble(date = base::seq(base::max(df$date) %m+% months(periodicity$p_ngap), #Lubridate
                                     base::as.Date(end_forecast),
                                     by = periodicity$p_name))) %>%
    dplyr::mutate(forecast = ifelse(base::is.na(forecast), T, forecast))

  return(df)
}
