#' @title Modify country in exchange rate
#' @name sf_conversao_cambio
#'
#' @description Realiza a projeção de um par de câmbio AB, utilizando a projeção dos pares AC e CB.
#' (Ex.: par a ser projetado: ARS/EUR, há projeção de ARS/USD e USD/EUR)
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada a ser projetada, contendo apenas dados realizados;
#' @param df_target Dataframe contendo a taxa de câmbio apenas com projeção;
#' @param df_ratio Dataframe contendo apenas a projeção do par incomum em df e df_target
#'
#' @return Retorna um Dataframe contendo a projeção da série df, no mesmo horizente que df_target
#'
#' @examples
#' sf_conversaro_cambio(df = ars_eur, df_target = ars_usd, df_ratio = usd_eur)
#'
#' @export

sf_conversao_cambio <- function(df, df_target, df_ratio) {

  if('forecast' %in% base::colnames(df)) {
    df <- df %>%
      dplyr::filter(!forecast)
  }
  if('forecast' %in% base::colnames(df_target)) {
    df_target <- df_target %>%
      dplyr::filter(forecast)
  }
  if('forecast' %in% colnames(df_ratio)) {
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

