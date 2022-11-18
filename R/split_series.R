#' @title Split loaded series from FS
#' @name split_series
#'
#' @description Seleciona apenas uma série do retorno da fct load_clean_series.
#'
#' @param df Dataframe contendo a base de dados a ser quebrada;
#' @param sid_vl Chr contendo o SeriesID da série a ser retornada;
#' @param estimate bool indicando se deve retornar dado realizado ou projetado. Se NULL retorna ambas.
#'
#' @author Gabriel Bellé
#'
#' @details
#' Seleciona apenas um SeriesID (sid) do Dataframe de retorno da função load_clean_series.
#'
#' @return O retorno é um df contendo as colunas de date, vl, forecast.
#'
#' @examples
#' split_fs_series(df = cleaned_df,
#'                 sid_vl = 'BREMP0018000OOML'),
#'                 estimate = T)
#'
#' @export

split_series <- function(df, sid_vl, estimate = NULL) {

  if(!all(c('sid', 'date', 'forecast', 'vl') %in% base::colnames(df))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }

  df <- df %>%
    dplyr::filter(sid == sid_vl)

  if(base::nrow(df) == 0) {
    stop('O parâmetro sid fornecido não existe no df de input!')
  }

  if(base::is.null(estimate)) {
    df <- df %>%
      dplyr::select(date, vl, forecast)
  } else if (estimate) {
    df <- df %>%
      dplyr::filter(forecast) %>%
      dplyr::select(date, vl, forecast)
  } else {
    df <- df %>%
      dplyr::filter(!forecast) %>%
      dplyr::select(date, vl, forecast)
  }

  return(df)
}
