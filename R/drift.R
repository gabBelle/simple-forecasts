#' @title Drift forecast
#' @name drift
#'
#' @description Incorpora tendência na projeção de uma série.
#'
#' Métodos recomendados de projeção utilizados no input são:
#' naive e seasonal naive
#'
#' @author Gabriel Bellé
#'
#' @param df_forecast Dataframe contendo a série a ser projetada;
#' @param nmean Opcional, constante numérica;
#' @param manual_drift Opcional, vetor de valores numéricos indicando drift em %;
#' @param target_value Opcional, vetor de valores indicando a projeção desejada para final de período.
#' @param type_drift Opcional, linear ou exponencial. Utilizado apenas quando target_value é chamado.
#'
#' @details
#' O @param df_forecast de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#' \code{forecast}: bool indicando se a observação é uma projeção.
#'
#' @param nmean indica quantos anos do histórico serão utilizados para calcular a tendência linear.
#' Se nenhum valor fornecido, utilizará o histórico completo.
#'
#' @param manual_drift deve conter um vetor numérico onde cada constante
#' indica a variação mensal desejada na projeção para cada ano. Caso o tamanho seja menor que
#' a quantidade de anos a ser projetada, repetirá-se o último valor colocado.
#' Ex: manual_drift = c(0.1, 0.1, 0.15, 0.2).
#' Assim, será utilizado 0.1% nos dois primeiros anos, 0.15% no terceiro, 0.2% no quarto e nos anos seguintes,
#' até o fim do horizonte da projeção.
#'
#' Atenção: este parâmetro faz com que a tendência seja exponencial. O aumento em t+1 é uma % do valor em t + valor em t.
#'
#' @param target_value indica o valor para o final de período desejado, idealmete advindo de uma projeção anual.
#' Por exemplo, a projeção anual do LatamFocus aponta 150 para 2023 e 200 para 2024, pode-se preencher:
#' target_value = c(150, 200)
#'
#' Isto fará com que a tendência seja tal qual respeite os valores objetivo.
#'
#' @param type_drif o valor no parâmetro irá modificar a fórmula empregada para cálculo do drift quando utilizado os valores
#' alvo em target_value. Caso linear, a tendência será linear, caso exponencial, a tendência será exponencial.
#'
#' @return O retorno é o mesmo Dataframe de entrada. No entando, a coluna vl representa os valores adicionados de tendência.
#' A coluna vl_old é incluída e contém a projeção anterior à modificação.
#'
#' @examples
#' \dontrun{
#' drift(df = cleaned_df,
#'       nmean = 5)
#'
#' drift(df = cleaned_df,
#'       manual_drift = c(0.1, 0.15))
#'
#' drift(df = cleaned_df,
#'       target_value = c(200, 230))
#' }
#'
#' @export

drift <- function(df_forecast,
                  nmeans = 5,
                  manual_drift = NULL,
                  target_value = NULL) {

  df_forecast <- df_forecast %>%
    dplyr::mutate(date = as.Date(date))

  #Checa se apenas 1 dos parâmetros opcionais está selecionado
  #Caso todos estejam como NULL o padrão é usar o histórico para calcular
  #A tendência
  if(all(sapply(list(nmeans, manual_drift, target_value), is.null)) |
     all(sapply(list(manual_drift, target_value), is.null)) |
     all(sapply(list(nmeans, target_value), is.null)) |
     all(sapply(list(nmeans, manual_drift), is.null))) {
    stop("Somente um dos três parâmetros opcionais pode ser escolhido!")
  }

  if(!is.null(manual_drift)) {

    drift_manual_out <- drift_manual(df_forecast = df_forecast,
                                     manual_drift = manual_drift)

    df_drift = drift_manual_out
    type_drift = 'mult'

  } else if(!is.null(target_value)) {

    drift_target_out <- drift_target(df_forecast = df_forecast,
                                     target_value = target_value)

    df_drift = drift_target_out$df
    type_drift = drift_target_out$type_drift

  } else {

    drift_hist_out <- drift_hist(df_forecast = df_forecast,
                                 nmeans = nmeans)

    df_drift = drift_hist_out
    type_drift = 'add'
  }

  df_out <- df_drift %>%
    dplyr::rename(vl_old = vl)

  if(type_drift == 'add') {
    df_out <- df_out %>%
      dplyr::mutate(vl = vl_old + drift)
  } else if(type_drif == 'mult') {
    df_out <- df_out %>%
      dplyr::mutate(vl = vl_old * drift)
  }

  return(df_out)
}
