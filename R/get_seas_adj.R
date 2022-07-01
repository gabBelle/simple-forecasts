#' @title Seasonality adjust
#' @name get_seas_adj
#'
#' @description Calcula o dessaz de uma série com as configurações automáticas do STL/X13 e computa média.
#'
#' @author Gabriel Bellé
#'
#' @details O input deve ser um df contendo pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' @return O retorno é um df contendo os valores da série dessazonalidada e a data.
#'
#' @examples
#' \dontrun{
#' get_seas_adj(df = cleaned_df)
#' }
#'
#' @export

get_seas_adj <- function(df) {

  periodicity <- get_periodicity(df)

  df_stl <- df %>%
    mutate(date = tsibble::yearmonth(date)) %>%
    tsibble::as_tsibble(index = date) %>%
    fabletools::model(feasts::STL(vl ~ season(window = 'periodic',
                                              period = periodicity$p_nmonths),
              robust = T)) %>%
    fabletools::components() %>%
    tibble::as_tibble() %>%
    dplyr::rename(stl = season_adjust) %>%
    dplyr::select(date, stl) %>%
    dplyr::mutate(date = as.Date(date))

  # mo <- as.numeric(format(pib$date[1], '%m'))
  # yr <- as.numeric(format(pib$date[1], '%Y'))
  # pib_x13 <- stats::ts(pib[[2]], start = c(yr, mo),
  #                      freq = periodicity$p_nmonths) %>%
  #   seas() %>%
  #   final() %>%
  #   as_tibble() %>%
  #   bind_cols(pib$date) %>%
  #   rename(x13 = 1,
  #          date = 2)
  #
  # test <- pib_dessaz %>%
  #   rename(dessaz_fs = vl) %>%
  #   left_join(pib) %>%
  #   left_join(pib_stl %>% select(date, stl)) %>%
  #   left_join(pib_x13) %>%
  #   pivot_longer(-date)

  return(df_stl)
}
