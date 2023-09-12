#' @title Load and Clean Feature Store (FS) series
#' @name load_clean_series
#'
#' @description Carrega e limpas as séries da FS.
#'
#' @param sid_vl Vetor chr com um ou mais SeriesID;
#' @param estimate bool indicando se deve baixar dado realizado ou projetado. Se NULL baixa ambas.
#' @param to_wider bool indicando se o retorno deve ser _wider_. Atenção: a coluna indicativa se é forecast será removida, sugere-se usar esse parametro como T concomitante ao estimate = F
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

load_clean_series <- function(sid_vl, estimate = NULL, to_wider = F) {

  series_fs <- fs4i::get_multi_series(series_code = sid_vl, estimate = T)[[1]] %>%
    dplyr::rename(
      sid = serie,
      vl = value,
      forecast = estimated
    ) %>%
    dplyr::relocate(sid, date, forecast, vl) %>%
    dplyr::mutate(date = base::as.Date(stringr::str_sub(date, 1, 10)),
                  vl = base::as.numeric(vl))

  if(estimate %>% is.null()) {
    cleaned_series <- series_fs
  } else if(estimate) {
    cleaned_series <- series_fs %>%
      dplyr::filter(forecast)
  } else{
    cleaned_series <- series_fs %>%
      dplyr::filter(!forecast)
  }

  if(to_wider) {
    series_fs <- series_fs %>%
      dplyr::select(-forecast) %>%
      tidyr::pivot_wider(names_from = sid, values_from = vl)
  }

  return(cleaned_series)
}
