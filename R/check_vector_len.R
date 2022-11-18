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
#' {date}: Data da observação:
#' {vl}: valor da observação;
#' {forecast}: bool indicando se o valor na data é projetado.
#'
#' @return O retorno é um vetor com o tamanho adequado em relação ao número de meses
#' da projeção e o número de vezes que cada valor será repetido.
#'
#' @examples
#' check_vector_len(df_forecast = df_forecast,
#'                   vector_to_check = target_value)
#'
#' @export

check_vector_len <- function(df_forecast,
                             vector_to_check) {

  if(!all(c('date', 'forecast', 'vl') %in% base::colnames(df_forecast))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }

  #Qtd de anos para serem projetados (completos e incompletos)
  n_years <- df_forecast %>%
    dplyr::filter(forecast) %>%
    dplyr::mutate(year = base::format(date, '%Y')) %>%
    purrr::pluck('year') %>%
    dplyr::n_distinct()

  if(n_years < base::length(vector_to_check)) {
    warning('Para estimar a tendencia foi fornecido um vetor maior que o necessario! O excesso sera truncado.')

    vector_to_check <- vector_to_check[1:n_years]

  } else if (n_years > base::length(vector_to_check)) {

    last_value <- utils::tail(vector_to_check, 1)
    vector_to_check <- vector_to_check[1:n_years]
    vector_to_check <- base::ifelse(base::is.na(vector_to_check), last_value, vector_to_check)
  } else {
    vector_to_check <- vector_to_check
  }

  return(vector_to_check)
}
