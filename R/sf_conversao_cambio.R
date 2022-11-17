#' @title Modify country in exchange rate
#' @name sf_conversao_cambio
#'
#' @description Realiza a projeção de um par de câmbio AB, se disponível a projeção dos pares AC e CB.
#' (Ex.: par a ser projetado: ARS/EUR, há projeção de ARS/USD e USD/EUR)
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada a ser projetada, contendo apenas dados realizados;
#' @param df_target Dataframe contendo a taxa de câmbio apenas com projeção;
#' @param df_ratio Dataframe contendo apenas a projeção do par incomum em @param df e @param df_target
#'
#' @details
#' Todos parâmetros de entrada devem conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#'
#' Caso algum parâmetro tenha tanto dado realizado quanto projetado, deve ser acompanhado de uma coluna
#' \code{forecast}: bool contendo se o valor é projetado
#'
#' @return Retorna um Dataframe contendo a projeção da série @param df, no mesmo horizente que @param df_target .
#'
#' @examples
#' \dontrun{
#' sf_conversaro_cambio(df = ars_eur, df_target = ars_usd, df_ratio = usd_eur) {
#' }
#'
#' @export

sf_conversao_cambio <- function(df, df_target, df_ratio) {

  if('forecast' %in% colnames(df)) {
    df <- df %>%
      dplyr::filter(!forecast)
  } if('forecast' %in% colnames(df_target)) {
    df_target <- df_target %>%
      dplyr::filter(forecast)
  } if('forecast' %in% colnames(df_ratio) {
    df_ratio <- df_ratio %>%
      dplyr::filter(forecast)
  }

  df_to_forecast <- df_target %>%
    dplyr::rename(paridade_projetada = vl) %>%
    dplyr::left_join(rename(df_ratio,
                            paridade_alvo = vl),
                     by = 'date') %>%
    dplyr::mutate(vl = paridade_projetada * paridade_alvo) %>%
    dplyr::select(date, vl) %>%
    dplyr::mutate(forecast = T)

  df_out <- df %>%
    dplyr::mutate(forecast = F) %>%
    dplyr::bind_rows(df_to_forecast)

  return(df_out)
}

