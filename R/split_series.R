#' @title Split loaded series from FS
#' @name split_fs_series
#'
#' @description Retorna apenas uma série de um df contendo séries empilhadas.
#' Pode ser útil como função auxiliar.
#'
#' @param name_sid Chr contendo o nome da série a ser retornada;
#' @param type Chr dizendo se deve retornar apenas a série realizada, projetada ou ambos.
#'
#' @author Gabriel Bellé
#'
#' @details O input deve ser um df contendo pelo menos as colunas de:
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
#'                 name_sid = 'BREMP0018000OOML'),
#'                 type = 'realizado')
#' }
#'
#' @export

split_series <- function(df, name_sid, type) {

  if(!all(c('sid', 'date', 'forecast', 'vl') %in% colnames(df))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }

  df <- df %>%
    dplyr::filter(sid == name_sid)

  if(nrow(df) == 0) {
    stop('O parâmetro name_sid fornecido não existe no df de input!')
  }

  if(type == 'realizado') {
    df <- df %>%
      dplyr::filter(!forecast)
  } else if (type  == 'projetado') {
    df <- df %>%
      dplyr::filter(!forecast)
  }

  df <- df %>%
    dplyr::select(date, vl)

  return(df)
}
