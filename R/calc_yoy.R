#' @title Calculate yoy from target value
#' @name calc_yoy
#'
#' @description A função realiza a conversão de um target em valores, mesma unidade de medida que o dataframe de interesse,
#' para valores em Year over Year (YoY).
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada;
#' @param target_aop Vetor de valores indicando a projeção desejada para média de período;
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#'
#' O @param target_aop indica o valor para média de período desejado, idealmete advindo de uma projeção anual.
#' Por exemplo, a projeção anual do LatamFocus aponta média de 15% para 2022 e 12% para 2023. Pode-se preencher:
#' target_aop = c(0.15,0.12)
#'
#' @return Retorna um vetor de valores de mesmo cumprimento de @param target_aop,
#' porém com os valores representando o YoY para ser aplicado nos meses do período.
#'
#' @examples
#' \dontrun{
#' calc_yoy <- function(df,
#'                      target_aop = c(11.5,10.5,10.1,9.8,9.5)) {
#' }
#'
#' @export

calc_yoy <- function(df, target_aop) {

  df <- df %>%
    filter(!forecast) %>%
    mutate(year = format(date, '%Y') %>% as.numeric(),
           month = format(date, '%m') %>% as.numeric())

  last_finished_year <- df %>%
    filter(month == max(month)) %>%
    filter(year == max(year)) %>%
    pluck('year')

  mean_last_year <- df %>%
    filter(year == last_finished_year) %>%
    group_by(year) %>%
    summarise(mean = mean(vl)) %>%
    pluck('mean')

  target_yoy <- c(mean_last_year, target_aop)
  target_yoy <- target_yoy/lag(target_yoy,1) - 1
  target_yoy <- target_yoy[2:length(target_yoy)]

  return(target_yoy)
}
