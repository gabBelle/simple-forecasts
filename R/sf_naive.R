#' @title Naive forecast
#' @name sf_naive
#'
#' @description Aplica o método naive para realizar projeção. Cada valor previsto é igual ao último observado.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada;
#' @param end_forecast Chr data contendo o último mês a ser projetado.
#'
#' @details
#' O @param df de entrada deve conter, pelo menos, as colunas de:
#' {date}: Data da observação:
#' {vl}: valor da observação;
#'
#' @return Retorna o mesmo df de input, porém com a projeção naïve,
#' e com a coluna forecast indicando quais observações são projetadas.
#'
#' @examples
#' sf_naive(df = df_cleaned, end_forecast = '2026-12-01')
#'
#' @export

sf_naive <- function(df,
                     end_forecast) {

  df_forecast <- expand_series(df,
                               end_forecast) %>%
    tidyr::fill(vl, .direction = 'down')

  return(df_forecast)
}
