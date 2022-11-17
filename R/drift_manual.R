#' @title Drift forecast with pre-defined MoM variation.
#' @name drift_manual
#'
#' @description Incorpora tendência na projeção de uma série.
#' Para calcular a tendência, utiliza os valores fornecidos, que devem ser % de MoM a ser aplicado na projeção.
#'
#' @author Gabriel Bellé
#'
#' @param df_forecast Dataframe contendo a série projetada, a ser adicionada tendência;
#' @param manual_drift Vetor de valores numéricos indicando drift em %;
#'
#' @details
#' O @param df_forecast de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#' \code{forecast}: bool indicando se a observação é uma projeção.
#'
#' O @param manual_drift deve conter um vetor numérico onde cada constante
#' indica a variação mensal desejada na projeção para cada ano. Caso o tamanho seja menor que
#' a quantidade de anos a ser projetada, repetirá-se o último valor colocado.
#' Ex: manual_drift = c(0.1, 0.1, 0.15, 0.2).
#' Assim, será utilizado 0.1% nos dois primeiros anos, 0.15% no terceiro, 0.2% no quarto e nos anos seguintes,
#' até o fim do horizonte da projeção.
#'
#' Atenção: este parâmetro faz com que a tendência seja exponencial. O aumento em t+1 é uma % do valor em t + valor em t.
#'
#' @return O retorno é um df, contendo as colunas de:
#' \code{date}; \code{vl}; \code{drift} e \code{forecast}.
#' As colunas são iguais ao input, com exceção da coluna drift, que possui o valor a ser adicionado na série.
#'
#' @examples
#' \dontrun{
#' drift_manual(df_forecast = cleaned_df,
#'              manual_drift = c(0.1, 0.15))
#' }
#'
#' @export

drift_manual <- function(df_forecast,
                         manual_drift) {

  if(!all(c('date', 'forecast', 'vl') %in% colnames(df_forecast))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }

  df_forecast <- df_forecast %>%
    dplyr::mutate(year = format(date, '%Y'))

  manual_drift_adj <- check_vector_len(df_forecast = df_forecast,
                                       vector_to_check = manual_drift)

  depara_year_manual <- df_forecast %>%
    dplyr::filter(forecast) %>%
    dplyr::group_by(year) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(drift = manual_drift_adj)  %>%
    dplyr::select(year, drift)

  df_forecast <- df_forecast %>%
    dplyr::left_join(depara_year_manual) %>%
    dplyr::mutate(
      drift = drift / 100 + 1,
      drift = ifelse(forecast, drift, 1),
      drift = cumprod(drift)) %>%
    dplyr::select(c(date, vl, drift, forecast))

  return(df_forecast)

}
