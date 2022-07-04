#' @title Drift forecast
#' @name drift
#'
#' @description Realiza a projeção de uma tendência naive, a qual pode ser acoplada em uma projeção.
#'
#' Ex: drift naive, repete o último valor e soma ou multiplica o valor do drift_forecast; ou,
#' seasonal naive with drift, repete o último valor do mesmo mês e soma ou multiplica o valor do drift_forecast.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série a ser projetada;
#' @param end_projection Data indicando fim da projeção;
#' @param nmean Opcional, constante numérica;
#' @param manual_drift Opcional, vetor de valores numéricos indicando drift em %;
#' @param target_value Opcional, vetor de valores indicando a projeção desejada para final de período.
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
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
#' end_projection = '2025-12-01' e target_value = c(150, 200)
#'
#' Isto fará com que a tendência linear seja tal qual respeite os valores de entrada.
#'
#' @return O retorno é uma lista contendo os objetos:
#' df: O mesmo df de entrada, com o adicional da do período da projeção com
#' os valores NA e uma coluna 'drift_forecast', indicando o valor da tendência para aquele período.
#' type: character indicando como a tendência deve ser adicionada à projeção: add para soma e mult para multiplicação.
#'
#'
#' @examples
#' \dontrun{
#' drift(df = cleaned_df,
#'       end_projection = '2025-12-01',
#'       nmean = 5)
#'
#' drift(df = cleaned_df,
#'       end_projection = '2025-12-01',
#'       manual_drift = c(0.1, 0.15))
#'
#' drift(df = cleaned_df,
#'       end_projection = '2025-12-01',
#'       target_value = c(200, 230))
#' }
#'
#' @export

drift <- function(df,
                  end_projection,
                  nmeans = 5,
                  manual_drift = NULL,
                  target_value = NULL) {

  df <- df %>%
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

    drift_manual_out <- drift_manual(df = df,
                                     end_projection = end_projection,
                                     manual_drift = manual_drift)

    df_drift = drift_manual_out$df
    type_drift = 'mult'

  } else if(!is.null(target_value)) {

    drift_target_out <- drift_target(df = df,
                                     end_projection = end_projection,
                                     target_value = target_value)

    df_drift = drift_target_out$df
    type_drift = 'add'

  } else {

    drift_hist_out <- drift_hist(df = df,
                                 end_projection = end_projection,
                                 nmeans = nmeans)

    df_drift = drift_hist_out$df
    type_drift = 'add'

  }

  return(list(
    df = df_drift,
    type = type_drift
  ))
}
