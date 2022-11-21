#' @title Drift forecast
#' @name sf_drift
#'
#' @description Incorpora tendência na projeção de uma série.
#' Ou seja, dada uma série já com projeção, adiciona uma tendência.
#'
#' Recomenda-se utilizar a função sf_naive ou sf_snaive para previamente projetar o input.
#'
#' @author Gabriel Bellé
#'
#' @param df_forecast Dataframe contendo a série já projetada, a ser adicionada tendência;
#' @param nyears Opcional, constante numérica. X anos do histórico.
#' @param manual_drift Opcional, vetor de valores numéricos indicando drift em \% MoM;
#' @param target_value Opcional, vetor de valores indicando a projeção desejada para final de período.
#' @param trend_type Opcional, linear ou exponencial. Utilizado apenas quando target_value é chamado.
#'
#' @details
#' Deve-se preencher apenas uma opção entre nyear, manual_drift e target_value
#' As restantes devem ser mantidas como NULL.
#'
#' nyears: indica quantos anos do histórico serão utilizados para calcular a tendência linear/exponencial.
#' Se nenhum valor fornecido, utilizará o histórico completo.
#'
#' manual_drift: deve conter um vetor numérico onde cada constante
#' indica a variação mensal desejada na projeção para cada ano. Caso o tamanho seja menor que
#' a quantidade de anos a ser projetada, repetirá-se o último valor colocado.
#'
#' Ex: manual_drift = c(0.1, 0.1, 0.15, 0.2).
#' Assim, será utilizado 0.1\% nos dois primeiros anos, 0.15% no terceiro, 0.2\% no quarto e nos anos seguintes,
#' até o fim do horizonte da projeção.
#'
#' Atenção: este parâmetro faz com que a tendência seja exponencial. O aumento em t+1 é uma \% do valor em t + valor em t.
#'
#' target_value: indica o valor para o final de período desejado, idealmete advindo de uma projeção anual.
#'
#' Por exemplo, a projeção anual do LatamFocus aponta 150 para 2023 e 200 para 2024, pode-se preencher:
#' target_value = c(150, 200)
#'
#' Isto fará com que a tendência seja tal qual respeite os valores objetivo.
#' Atenção: o alvo será atingido apenas se a projeção de entrada vier da sf_naive.
#'
#' trend_type: o valor no parâmetro irá modificar a fórmula empregada para cálculo do drift quando utilizado os valores
#' alvo em target_value. Aceita os valores (linear, exponencial).
#'
#' O retorno é um Dataframe. A coluna 'vl' representa os valores adicionados de tendência.
#' A coluna 'vl_old' é incluída e contém a projeção anterior à modificação.
#'
#' @examples
#' sf_drift(df_forecast = cleaned_df,
#'          nyears = 5)
#'
#' sf_drift(df_forecast = cleaned_df,
#'          manual_drift = c(0.1, 0.15))
#'
#' sf_drift(df_forecast = cleaned_df,
#'          target_value = c(200, 230),
#'          trend_type = 'linear')
#' @export

sf_drift <- function(df_forecast,
                     nyears = NULL,
                     manual_drift = NULL,
                     target_value = NULL,
                     trend_type = 'linear') {

  if(!all(c('date', 'forecast', 'vl') %in% base::colnames(df_forecast))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }

  df_forecast <- df_forecast %>%
    dplyr::mutate(date = base::as.Date(date))

  if(base::any(!base::is.null(nyears), !base::is.null(manual_drift), !base::is.null(target_value))) {
    a = 0
    if(!base::is.null(nyears)) {
      a = a + 1
    }
    if(!base::is.null(manual_drift)){
      a = a + 1
    }
    if(!base::is.null(target_value)){
      a = a + 1
    }
    if(a > 1) {
      stop("Escolha apenas 1 método de drift!")
    }
  }

  if(!base::is.null(manual_drift)) {

    drift_manual_out <- drift_manual(df_forecast = df_forecast,
                                     manual_drift = manual_drift)

    df_drift = drift_manual_out
    type_drift = 'mult'

  } else if(!base::is.null(target_value)) {

    drift_target_out <- drift_target(df_forecast = df_forecast,
                                     target_value = target_value,
                                     trend_type = trend_type)

    df_drift = drift_target_out$df
    type_drift = drift_target_out$type_drift

  } else {

    drift_hist_out <- drift_hist(df_forecast = df_forecast,
                                 nyears = nyears)

    df_drift = drift_hist_out
    type_drift = 'add'
  }

  df_out <- df_drift %>%
    dplyr::rename(vl_old = vl)

  if(type_drift == 'add') {
    df_out <- df_out %>%
      dplyr::mutate(vl = vl_old + drift)
  } else if(type_drift == 'mult') {
    df_out <- df_out %>%
      dplyr::mutate(vl = vl_old * drift)
  }

  df_out <- df_out %>%
    dplyr::select(c(date, forecast, vl))

  return(df_out)
}
