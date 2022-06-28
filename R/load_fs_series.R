#' @title Load Feature Store (FS) series
#' @name load_fs_series
#'
#' @description Carrega séries da FS
#'
#' @param sids Vetor do tipo \code{chr} contendo as séries a serem retornadas da FS.
#'
#' @author Gabriel Bellé
#'
#' @details ---
#'
#' @return O retorno é um df contendo uma coluna de datas \code{date} seguido por
#' outras colunas com o nome de cada SID (seriesID).
#'
#' @examples
#' \dontrun{
#' load_fs_series(sids = c('BREMP0018000OOML', 'BREMP0018000SOML'))
#' }
#'
#' @export

load_fs_series <- function(sids, auth_path) {

  query_data <- data.frame(sid = sids, force = T)

  series_fs <- series.4macro::get_multi_series(query_data,
                                               filepath = auth_path,
                                               lang = "pt-br")

  return(series_fs)
}
