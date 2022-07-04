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
#' @param n_months Opcional, indica por quantos meses cada variação no vetor manual_drift será utilizado. Para série trimestral, quantos trimestres.
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

  #Qtd de anos para serem projetados (completos e incompletos)
  n_years <- df_forecast %>%
    dplyr::filter(forecast) %>%
    mutate(year = format(date, '%Y')) %>%
    purrr::pluck('year') %>%
    dplyr::n_distinct()

  if(n_years > length(vector_to_check)) {
    warning('Para estimar a tendencia foi fornecido um vetor maior que o necessario! O excesso sera truncado.')

    vector_to_check <- vector_to_check[1:n_years]

  } else if (n_years < length(vector_to_check)) {

    last_value <- vector_to_check[-1]
    vector_to_check <- vector_to_check[1:n_years]
    vector_to_check <- ifelse(is.na(vector_to_check), last_value, vector_to_check)

  } else {
    vector_to_check <- vector_to_check
  }

  return(vector_to_check)
}
