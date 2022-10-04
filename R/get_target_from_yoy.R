#' @title Naive forecast
#' @name naive
#'
#' @description Aplica o método naive para realizar projeção, cada valor previsto é igual ao último observado.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada;
#' @param end_projection Data contendo o último mês a ser projetado.
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#'
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método naïve,
#' e com a coluna forecast indicando quais observações são projeção.
#'
#' @examples
#' \dontrun{
#' naive(df = df_cleaned,
#'       end_projection = '2026-12-01')
#' }
#'
#' @export

get_target_from_yoy <- function(df,
                                yoy,
                                end_forecast) {

  last_vl <- df %>%
    dplyr::mutate(
      month = format(date, '%m') %>%
        as.numeric(),
      year = format(date, '%Y') %>%
        as.numeric()
    ) %>%
    dplyr::filter(month == max(month)) %>%
    dplyr::filter(year == max(year)) %>%
    pluck('vl')

  df_forecast <- naive(df, end_forecast = end_forecast)

  yoy_adj <- check_vector_len(df_forecast, yoy)

  target <- c(last_vl, yoy_adj/100 + 1) %>%
    cumprod()

  target <- target[2:length(target)]

  return(target)
}
