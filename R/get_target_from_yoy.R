#' @title Get target from YoY
#' @name get_target_from_yoy
#'
#' @description Calcula um valor alvo, na mesma unidade de medida da série de input,
#' que respeite o dado de YoY passado como argumento.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada;
#' @param yoy Vetor numérico contendo a variação YoY em % (1% = 1)
#' @param end_projection Data contendo o último mês a ser projetado.
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' {date}: Data da observação:
#' {vl}: valor da observação;
#'
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método naïve,
#' e com a coluna forecast indicando quais observações são projeção.
#'
#' @examples
#' get_target_from_yoy(df = df_cleaned,
#'                     yoy = (10,12,12,15)
#'                     end_projection = '2026-12-01')
#'
#' @export

get_target_from_yoy <- function(df,
                                yoy,
                                end_forecast) {

  last_vl <- df %>%
    dplyr::mutate(
      month = base::format(date, '%m') %>%
        base::as.numeric(),
      year = base::format(date, '%Y') %>%
        base::as.numeric()
    ) %>%
    dplyr::filter(month == base::max(month)) %>%
    dplyr::filter(year == base::max(year)) %>%
    purrr::pluck('vl')

  df_forecast <- sf_naive(df, end_forecast = end_forecast)

  yoy_adj <- check_vector_len(df_forecast, yoy)

  target <- c(last_vl, yoy_adj/100 + 1) %>%
    base::cumprod()

  target <- target[2:base::length(target)]

  return(target)
}
