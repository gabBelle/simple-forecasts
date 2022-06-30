#' @title Discover periodicity from df
#' @name get_periodicity
#'
#' @description Descobre a periodicidade de um dataframe e cria parametros adequados a ela.
#'
#' @param df Dataframe contendo a base de dados.
#' @author Gabriel Bellé
#'
#' @details O input deve ser um df contendo pelo menos as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' @return O retorno é uma lista contendo os parametros de:
#' \code{p_name}: nome da periodicidade;
#' \code{p_nmonths}: quantos meses há na periodicidade;
#' \code{p_ngap}: quantos meses pular até a próxima observação.
#'
#' @examples
#' \dontrun{
#' split_fs_series(df = cleaned_df,
#'                 name_sid = 'BREMP0018000OOML'),
#'                 type = 'realizado')
#' }
#'
#' @export

get_periodicity <- function(df) {

  periodicity = df %>%
    dplyr::group_by(year = format(date, '%Y')) %>%
    dplyr::summarise(count = dplyr::n_distinct(date)) %>%
    dplyr::ungroup()

  if (stats::median(periodicity$count) == 12) {
    p_name = 'month'
    p_nmonths = 12
    p_ngap = 1
  } else if (stats::median(periodicity$count) == 4) {
    p_name = 'quarter'
    p_nmonths = 4
    p_ngap = 3
  }

  output <- list(
    p_name = p_name,
    p_nmonths = p_nmonths,
    p_ngap = p_ngap
  )

  return(output)
}
