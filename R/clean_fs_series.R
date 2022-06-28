#' @title Clean Feature Store (FS) series
#' @name clean_fs_series
#'
#' @description Limpa o arquivo retornado pela API da FS
#'
#' @param series Lista contendo as séries carregadas da FS pelo pacote series.4macro
#'
#' @author Gabriel Bellé
#'
#' @details ---
#'
#' @return O retorno é um df contendo as colunas:
#' \code{sid}: SériesID da série;
#' \code{date}: Data da observação:
#' \code{forecast}: TRUE quando a observação for uma projeção;
#' \code{vl}: valor da observação.
#'
#' @examples
#' \dontrun{
#' clean_fs_series(series = loaded_series)
#' }
#'
#' @export

clean_fs_series <- function(series) {

  cleaned <- series %>%
      tibble::as_tibble() %>%
      dplyr::select(c(sid, contents)) %>%
      tidyr::unnest(contents) %>%
      dplyr::mutate(
        dt = dt %>%
               as.Date(),
        vl = as.numeric(vl),
        forecast = ifelse(lbl == '', F, T)) %>%
      dplyr::arrange(sid, dt) %>%
      dplyr::rename(date = dt) %>%
      dplyr::select(-lbl) %>%
      dplyr::relocate(forecast, .before = 'vl')

  return(cleaned)
}

