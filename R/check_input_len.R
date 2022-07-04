#' @title Check input vector length is equal or less than needed
#' @name check_vector_len
#'
#' @description Compara a quantidade de valores no vetor de input com o número necessário
#' para fazer projeção utilizando o método drift.
#'
#' @author Gabriel Bellé
#'
#' @param df_forecast Dataframe contendo a série com o período de projeção calculado;
#' @param vector_to_check Vetor contendo o alvo do drift ou os valores do drift manual.
#'
#' @details
#' O @param df_forecast de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#' \code{forecast}: bool indicando se o valor na data é projetado.
#'
#' @return O retorno é um vetor com o tamanho adequado em relação ao número de meses
#' da projeção e o número de vezes que cada valor será repetido.
#'
#' @examples
#' \dontrun{
#' check_vector_leng(df_forecast = df_forecast,
#'                   vector_to_check = target_value)
#' }
#'
#' @export

check_vector_len <- function(df_forecast,
                             vector_to_check) {

  n_years <- df_forecast %>%
    dplyr::filter(is.na(vl)) %>%
    purrr::pluck('year') %>%
    dplyr::n_distinct()

  if(length(vector_to_check) > n_years) {
    warning('Para estimar a tendência foi fornecido um vetor maior que o necessário! O excesso será truncado.')

    vector_to_check <- vector_to_check[1:n_years]

  } else if (length(vector_to_check) < n_years) {
    last_value <- vector_to_check[-1]
    vector_to_check <- vector_to_check[1:n_years]
    vector_to_check <- ifelse(is.na(vector_to_check), last_value, vector_to_check)
  }

  return(complete_vector)
}
