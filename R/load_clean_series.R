#' @title Load and Clean Feature Store (FS) series
#' @name load_clean_series
#'
#' @description Carrega e limpas as séries da FS.
#'
#' @param sid_vl Vetor chr com um ou mais SeriesID;
#' @param estimate bool indicando se deve baixar dado realizado ou projetado. Se NULL baixa ambas.
#'
#' @author Gabriel Bellé
#'
#' @details Baixa as séries da FS utilizando o pacote fs-r-package e limpa elas
#' para que fiquem no formato de {tibble}, empiladas e indicando qual valor é projeção.
#' A autenticação deve ser feita no pacote fs-r-package.
#'
#' Se o parâmetro estimate for TRUE baixa apenas projeções, FALSE apenas dado realizado e NULL tudo disponível.
#'
#' @return O retorno é um df contendo as colunas: sid, date, forecast e vl.
#'
#' @examples
#' load_clean_series(sid_vl = c('BREMP0018000OOML', 'BREMP0018000SOML'),
#'                   estimate = TRUE)
#'
#' @export

load_clean_series <- function(sid_vl, estimate = NULL) {

  series_fs <- fs4i::get_multi_series(series_code = sid_vl, estimate = T)[[1]] %>%
    dplyr::rename(
      sid = serie,
      vl = value,
      forecast = estimated
    ) %>%
    relocate(sid, date, forecast, vl) %>%
    mutate(date = as.Date(str_sub(date, 1, 10)))

  if(estimate %>% is.null()) {
    cleaned_series <- series_fs
  } else if(estimate) {
    cleaned_series <- series_fs %>%
      filter(forecast)
  } else{
    cleaned_series <- series_fs %>%
      filter(!forecast)
  }

  return(cleaned_series)
}
