#' @title Montly Forecast
#' @name get_montly_forecast
#'
#' @description A partir de uma projeção para final de período anual, a função
#' irá estimar os valores mensais desda última observação realizada até o valor alvo,
#' mantendo a sazonalidade presente no histórico da série.
#'
#' É um wrapper nas funções: naive, seas_adj, seas_ratio, drift_target.
#'
#' Também realiza projeções de séries trimestrais.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada;
#' @param end_projection Data (YYYY-MM-DD) contendo o último mês a ser projetado;
#' @param target_value Vetor de valores indicando a projeção desejada para final de período;
#' @param trend_type Opcional, linear ou exponential. Utilizado apenas quando target_value é chamado. Padrão é exponential.
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#'
#' O @param target_value indica o valor para o final de período desejado, idealmete advindo de uma projeção anual.
#' Por exemplo, a projeção anual do LatamFocus aponta 150 para 2023 e 200 para 2024, pode-se preencher:
#' end_projection = '2025-12-01' e target_value = c(150, 200)
#'
#' Isto fará com que a tendência linear seja tal qual respeite os valores de entrada.
#'
#' @param type_drif o valor no parâmetro irá modificar a fórmula empregada para cálculo do drift quando utilizado os valores
#' alvo em target_value. Caso linear, a tendência será linear, caso exponencial, a tendência será exponencial.
#'
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método naïve,
#' e com a coluna forecast indicando quais observações são projeção.
#'
#' @examples
#' \dontrun{
#' get_montly_forecast <- function(df,
#'                                 end_projection = '2026-12-01',
#'                                 nmeans = 5,
#'                                 target_value = c(180, 190, 195, 200),
#'                                 trend_type = 'exponential') {
#' }
#'
#' @export

get_montly_forecast <- function(df,
                                end_projection,
                                nmeans = NULL,
                                target_value,
                                trend_type = 'exponential') {

  #Realiza o dessaz
  df_seas_adj <- get_seas_adj(df, type = 'STL')

  #Pega o fator sazonal histórico
  df_hist_seas_ratio <- seas_ratio(df_original = df,
                                   df_dessaz = df_seas_adj,
                                   nmeans = nmeans)

  last_month_ratio_mean <- df_hist_seas_ratio %>%
    filter(month == max(month)) %>%
    pluck('ratio_mean')

  #Transforma o target_value em valore dessaz usando o fator sazonal historico
  target_vl_dessaz = target_value / last_month_ratio_mean

  #Faz projeção naive do dessaz
  df_naive <- naive(df = df_seas_adj,
                    end_projection = end_projection)

  #Adiciona a tendência na projeção naive da série dessaz
  df_drift <- drift(df_forecast = df_naive,
                    target_value = target_vl_dessaz,
                    trend_type = trend_type)

  #Converte a projeção com tendência dessaz para a série original
  #Mantendo a sazonalidade de acordo com o fator dessaz histórico
  df_seas_ratio <- seas_ratio(df_original = df,
                              df_dessaz = df_drift,
                              nmeans = nmeans)

  return(df_forecast)
}
