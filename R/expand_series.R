#' @title Expand date span in a time series
#' @name expand_series
#'
#' @description Aumenta o vetor de datas de um dataframe, inputando NAs na coluna de observação,
#' para ser preenchida na função de forecast.
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

  if(!all(c('date', 'vl') %in% colnames(df))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }
  
  periodicity <- get_periodicity(df)

  df <- df %>%
    mutate(forecast = F) %>%
    bind_rows(
      tibble(date = seq(max(df$date) %m+% months(periodicity$p_ngap), #Lubridate
                        as.Date(end_forecast),
                        by = periodicity$p_name)
             )
    ) %>%
    mutate(forecast = ifelse(is.na(forecast), T, forecast))

  return(df)
}
