#' @title Wrapper das funções load_fs_series e clean_fs_series
#' @name load_clean_series
#'
#' @description Carrega e limpas as séries da FS
#'
#' @param sids Vetor do tipo \code{chr} contendo as séries a serem retornadas da FS;
#' @param auth_path Chr contendo o caminho para o arquivo de autenticação do series.4macro
#'
#' @author Gabriel Bellé
#'
#' @details Wrapper das funções para baixar as séries da FS \code{load_fs_series} e
#' da função de limpeza das séries da FS \code{clean_fs_series}.
#'
#' @return O retorno é um df contendo as colunas:
#' \code{sid}: SériesID da série;
#' \code{date}: Data da observação:
#' \code{forecast}: TRUE quando a observação for uma projeção;
#' \code{vl}: valor da observação.
#'
#' @examples
#' \dontrun{
#' load_clean_series(sids = c('BREMP0018000OOML', 'BREMP0018000SOML'),
#'                   auth_path = 'User/auth.ini')
#' }
#'
#' @export

load_clean_series <- function(sids, auth_path) {

  loaded_series <- load_fs_series(sids, auth_path)
  cleaned_series <- clean_fs_series(loaded_series)

  return(cleaned_series)
}
