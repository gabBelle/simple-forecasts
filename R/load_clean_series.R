#' @title Load and Clean Feature Store (FS) series
#' @name load_clean_series
#'
#' @description Carrega e limpas as séries da FS
#'
#' @param sids Vetor do tipo \code{chr} contendo as séries a serem retornadas da FS;
#' @param auth_path Chr contendo o caminho para o arquivo de autenticação do series.4macro
#'
#' @author Gabriel Bellé
#'
#' @details Baixa as séries da FS utilizados o pacote series.4macro e limpa elas
#' para que fiquem no formato de \code{tibble}, empiladas e indicando onde é a projeção.
#'
#' @return O retorno é um df contendo as colunas:
#' \code{sid}: SeriesID da série;
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

load_clean_series <- function(sids, auth_path, estimate = NULL) {

  if(base::is.null(estimate)) {
    query_data <- base::data.frame(sid = sids, force = T)
  } else {
    query_data <- base::data.frame(sid = sids, force = T, estimate = estimate)
  }

  series_fs <- series.4macro::get_multi_series(query_data,
                                               filepath = auth_path,
                                               lang = "pt-br")

  cleaned_series <- series_fs %>%
    tibble::as_tibble() %>%
    dplyr::select(c(sid, contents)) %>%
    tidyr::unnest(contents) %>%
    dplyr::mutate(
      dt = dt %>%
        base::as.Date(),
      vl = base::as.numeric(vl),
      sid = base::as.character(sid),
      forecast = base::ifelse(lbl == '', F, T)) %>%
    dplyr::arrange(sid, dt) %>%
    dplyr::rename(date = dt) %>%
    dplyr::select(-lbl) %>%
    dplyr::relocate(forecast, .before = 'vl')

  return(cleaned_series)
}
