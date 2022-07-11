#' @title X-13 ARIMA-SEATS
#' @name get_x13
#'
#' @description Calcula o dessaz de uma série com as configurações automáticas
#' do X13.
#'
#' @param df série para dessazonalização 
#'
#' @author Gabriel Bellé
#'
#' @details O input deve ser um df contendo pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' @return O retorno é um df contendo os valores da série dessazonalizada e a data.
#'
#' @examples
#' \dontrun{
#' get_x13(df = cleaned_df)
#' }
#'
#' @export

get_x13 <- function(df) {

  if(!all(c('date', 'vl') %in% colnames(df))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }
  
  periodicity <- get_periodicity(df)

  mo <- as.numeric(format(df$date[1], '%m'))
  yr <- as.numeric(format(df$date[1], '%Y'))

  df_x13 <- stats::ts(df[['vl']], start = c(yr, mo),
                       freq = periodicity$p_nmonths) %>%
    seasonal::seas() %>%
    seasonal::final() %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(df$date) %>%
    dplyr::rename(x13 = 1,
                  date = 2) %>% 
    dplyr::relocate(date, .after = NULL)
  return(df_x13)
}
