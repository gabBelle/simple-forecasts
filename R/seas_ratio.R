#' @title Seasonality ratio
#' @name seas_ratio
#'
#' @description Realiza a projeção de uma série original usando a projeção de uma série dessaz
#' fazendo a compatibilização pela razão histórica entre a série original e a dessaz.
#'
#' @author Gabriel Bellé
#'
#' @param df_original Dataframe de entrada com a série original;
#' @param df_dessaz Dataframe de entrada com a série dessaz e a projeção já realizada até horizonte desejado;
#' @param nmeans Opcional, número de anos que será utilizado do histórico para as computações.
#'
#' @details O input deve ser um df contendo pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' @return O retorno é um df contendo os valores da série projetada e a data.
#'
#' @examples
#' \dontrun{
#' seas_ratio(df_original = df_cleaned,
#'            df_dessaz = df_forecast_dessaz,
#'            nmeans = 5)
#' }
#'
#' @export

seas_ratio <- function(df_original, df_dessaz, nmeans = NULL) {

  if(max(df_dessaz$date) <= max(df_original$date)) {
    stop('ERRO: o df dessaz deve ter projeção em relação ao original!')
  }

  df_ratio = df_original %>%
    dplyr::rename(original = vl) %>%
    dplyr::right_join(df_dessaz %>%
                 dplyr::rename(dessaz = vl)) %>%
    dplyr::mutate(ratio = original / dessaz)

  if(is.null(nmeans)) {
    hist_mean <- df_ratio
  } else{
    hist_mean <- df_ratio %>%
      dplyr::filter(date >= max(date) - years(nmeans))
  }

  hist_mean <- hist_mean %>%
    dplyr::group_by(month = format(date, '%B')) %>%
    dplyr::summarise(ratio_mean = mean(ratio, na.rm = T)) %>%
    dplyr::ungroup()

  df_forecast <- df_ratio %>%
    dplyr::mutate(month = format(date, '%B')) %>%
    dplyr::left_join(hist_mean) %>%
    dplyr::mutate(
      forecast = is.na(original),
      original = ifelse(is.na(original),
                        dessaz * ratio_mean,
                        original)
    ) %>%
    dplyr::rename(vl = original) %>%
    dplyr::select(date, vl, forecast)

  return(df_forecast)
}
