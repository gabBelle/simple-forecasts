#' @title Seasonal Decomposition of Time Series by Loess (STL)
#' @name get_stl
#'
#' @description Calcula o dessaz de uma série com as configurações automáticas
#' do STL.
#'
#' @param df representa a série para dessazonalização
#'
#' @author Gabriel Bellé
#'
#' @details O input deve ser um df contendo pelo as colunas de:
#' {date}: Data da observação:
#' {vl}: valor da observação.
#'
#' @return O retorno é um df contendo os valores da série dessazonalidada e a data.
#'
#' @examples
#' get_stl(df = cleaned_df)
#'
#' @export

get_stl <- function(df) {

  if(!all(c('date', 'vl') %in% base::colnames(df))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }

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
