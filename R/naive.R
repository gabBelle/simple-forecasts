#' @title Naive forecast
#' @name naive
#'
#' @description Aplica o método naive para realizar projeção, cada valor previsto é igual ao último observado.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada;
#' @param end_forecast Data contendo o último mês a ser projetado.
#'
#' @details
#' O @param df de entrada deve conter pelo menos as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#'
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método naïve,
#' e com a coluna forecast indicando quais observações são projeção.
#'
#' @examples
#' \dontrun{
#' naive(df = df_cleaned,
#'       end_forecast = '2026-12-01')
#' }
#'
#' @export

naive <- function(df,
                  end_forecast) {

  df_forecast <- expand_series(df,
                               end_forecast) %>%
    tidyr::fill(vl, .direction = 'down')

  return(df_forecast)
}
