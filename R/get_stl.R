#' @title Seasonal Decomposition of Time Series by Loess (STL)
#' @name get_stl
#'
#' @description Calcula o dessaz de uma série com as configurações automáticas
#' do STL.
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
#' get_stl(df = cleaned_df)
#' }
#'
#' @export

get_stl <- function(df) {

  periodicity <- get_periodicity(df)

  df_stl <- df %>%
    dplyr::mutate(date = tsibble::yearmonth(date)) %>%
    tsibble::as_tsibble(index = date) %>%
    fabletools::model(feasts::STL(vl ~ season(window = 'periodic',
                                              period = periodicity$p_nmonths),
              robust = T)) %>%
    fabletools::components() %>%
    tibble::as_tibble() %>%
    dplyr::rename(stl = season_adjust) %>%
    dplyr::select(date, stl) %>%
    dplyr::mutate(date = as.Date(date))

  return(df_stl)
}