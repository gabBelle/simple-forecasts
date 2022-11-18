#' @title Calculate yoy from target value
#' @name calc_yoy
#'
#' @description Converte um vetor de valores, que estão na mesma unidade de medida da séries, e representam a média de um período futuro
#' para a série, em um vetor contendo variações YoY entre os anos.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada;
#' @param target_aop Vetor de valores indicando a projeção desejada para média de período;
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' {date}: Data da observação:
#' {vl}: valor da observação;
#'
#' O @param target_aop indica o valor para média de período desejado, idealmete advindo de uma projeção anual.
#' Por exemplo, a projeção anual do LatamFocus aponta média de 15% para 2022 e 12% para 2023. Pode-se preencher:
#' target_aop = c(0.15,0.12)
#'
#' @return Retorna um vetor de valores de mesmo cumprimento de @param target_aop,
#' porém com os valores representando o YoY para o ano.
#'
#' @examples
#' calc_yoy <- function(df,
#'                      target_aop = c(11.5,10.5,10.1,9.8,9.5))
#'
#' @export

calc_yoy <- function(df, target_aop) {

  df <- df %>%
    dplyr::filter(!forecast) %>%
    dplyr::mutate(year = base::format(date, '%Y') %>% base::as.numeric(),
                  month = base::format(date, '%m') %>% base::as.numeric())

  last_finished_year <- df %>%
    dplyr::filter(month == max(month)) %>%
    dplyr::filter(year == max(year)) %>%
    purrr::pluck('year')

  mean_last_year <- df %>%
    dplyr::filter(year == last_finished_year) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(mean = mean(vl)) %>%
    purrr::pluck('mean')

  target_yoy <- c(mean_last_year, target_aop)
  target_yoy <- target_yoy/dplyr::lag(target_yoy,1) - 1
  target_yoy <- target_yoy[2:base::length(target_yoy)]

  return(target_yoy)
}
