#' @title Load and Clean Feature Store (FS) series
#' @name load_clean_series
#'
#' @description Carrega e limpas as séries da FS.
#'
#' @param sid_vl Vetor chr com um ou mais SeriesID;
#' @param auth_path Chr com caminho para o arquivo de autenticação do series.4macro;
#' @param estimate bool indicando se deve baixar dado realizado ou projetado. Se NULL baixa ambas.
#'
#' @author Gabriel Bellé
#'
#' @details Baixa as séries da FS utilizando o pacote series.4macro e limpa elas
#' para que fiquem no formato de {tibble}, empiladas e indicando qual valor é projeção.
#'
#' @return O retorno é um df contendo as colunas: sid, date, forecast e vl.
#'
#' @examples
#' load_clean_series(sid_vl = c('BREMP0018000OOML', 'BREMP0018000SOML'),
#'                   auth_path = 'User/auth.ini'
#'                   estimate = TRUE)
#'
#' @export

load_clean_series <- function(sid_vl, auth_path, estimate = NULL) {

  if(base::is.null(estimate)) {
    query_data <- base::data.frame(sid = sid_vl, force = T)
  } else {
    query_data <- base::data.frame(sid = sid_vl, force = T, estimate = estimate)
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
