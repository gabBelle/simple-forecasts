#' @title Forecasting with target
#' @name sf_target
#'
#' @description Projeta uma série mensal ou trimestral, tendo como alvo um valor final, inserido manualmente.
#' Mantém a sazonalidade histórica para os meses projetados.
#' A função também lida com uma série que termina em qualquer mês do ano (Exp: último realizado 04/22, será completado os meses restantes até dezembro)
#'
#' É um wrapper das funções: naive, seas_adj, seas_ratio, drift_target.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada;
#' @param end_forecast Data (YYYY-MM-DD) contendo o último mês a ser projetado;
#' @param nyears Opcional, número de anos que será utilizado do histórico para as computações. Se não preenchido, usa o histórico inteiro.
#' @param target_value Vetor de valores indicando a projeção desejada para final de período;
#' @param trend_type Tipo de tendência (linear ou exponential). Padrão é linear.
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
#' @param trend_type o valor no parâmetro irá modificar a fórmula empregada para cálculo do drift quando utilizado os valores
#' alvo em target_value. Aceita os valores (linear, exponencial).
#'
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método naïve,
#' e com a coluna forecast indicando quais observações são projeção.
#'
#' @examples
#' \dontrun{
#' sf_target(df,
#'           end_forecast = '2026-12-01',
#'           nyears = 5,
#'           target_value = c(180, 190, 195, 200),
#'           trend_type = 'linear') {
#' }
#'
#' @export

sf_target <- function(df,
                      end_forecast,
                      nyears = NULL,
                      target_value,
                      trend_type = 'linear') {

  #Realiza o dessaz
  df_seas_adj <- get_seas_adj(df, type = 'STL')

  #Pega o fator sazonal histórico
  df_hist_seas_ratio <- seas_ratio(df_original = df,
                                   df_dessaz = df_seas_adj,
                                   nyears = nyears)

  last_month_ratio_mean <- df_hist_seas_ratio %>%
    filter(month == max(month)) %>%
    pluck('ratio_mean')

  #Transforma o target_value em valore dessaz usando o fator sazonal historico
  target_vl_dessaz = target_value / last_month_ratio_mean

  #Faz projeção naive do dessaz
  df_naive <- naive(df = df_seas_adj,
                    end_forecast = end_forecast)

  #Adiciona a tendência na projeção naive da série dessaz
  df_drift <- drift(df_forecast = df_naive,
                    target_value = target_vl_dessaz,
                    trend_type = trend_type)

  #Converte a projeção com tendência dessaz para a série original
  #Mantendo a sazonalidade de acordo com o fator dessaz histórico
  df_seas_ratio <- seas_ratio(df_original = df,
                              df_dessaz = df_drift,
                              nyears = nyears)
  return(df_seas_ratio)

  }

