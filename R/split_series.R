#' @title Split loaded series from FS
#' @name split_fs_series
#'
#' @description Retorna apenas uma série de um df contendo séries empilhadas.
#' Pode ser útil como função auxiliar.
#'
#' @param df Dataframe contendo a base de dados a ser quebrada;
#' @param sid Chr contendo o nome da série a ser retornada;
#' @param type Chr especificando tipo de retorno (realizado, projetado, ambos)
#' @author Gabriel Bellé
#'
#' @details O input @param df deve ser um DataFrame contendo pelo menos as colunas de:
#' \code{sid}: SériesID da série;
#' \code{date}: Data da observação:
#' \code{forecast}: TRUE quando a observação for uma projeção;
#' \code{vl}: valor da observação.
#'
#' @return O retorno é um df contendo os valores da série e a data.
#'
#' @examples
#' \dontrun{
#' split_fs_series(df = cleaned_df,
#'                 sid = 'BREMP0018000OOML'),
#'                 type = 'ambos')
#' }
#'
#' @export

split_series <- function(df, sid, type = 'ambos') {

  if(!all(c('sid', 'date', 'forecast', 'vl') %in% colnames(df))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }

  df <- df %>%
    dplyr::filter(sid == sid)

  if(nrow(df) == 0) {
    stop('O parâmetro sid fornecido não existe no df de input!')
  }

  if(type == 'realizado') {
    df <- df %>%
      dplyr::filter(!forecast) %>%
      dplyr::select(date, vl)
  } else if (type  == 'projetado') {
    df <- df %>%
      dplyr::filter(forecast) %>%
      dplyr::select(date, vl)
  } else if (type == 'ambos') {
    df <- df %>%
      dplyr::select(date, vl, forecast)
  }else{
    stop("ERRO: Type selecionado incompatível!")
  }

  return(df)
}
