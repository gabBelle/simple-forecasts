#' @title Seasonality ratio
#' @name seas_ratio
#'
#' @description Realiza a projeção de uma série original usando a projeção de uma série dessaz
#' fazendo a compatibilização pela razão histórica entre a série original e a dessaz.
#'
#' Caso seja fornecido uma série dessaz sem projeção, o retorno passa a ser apenas o fator sazonal histórico médio.
#'
#' @author Gabriel Bellé
#'
#' @param df_original Dataframe de entrada com a série original;
#' @param df_dessaz Dataframe de entrada com a série dessaz e a projeção já realizada até horizonte desejado;
#' @param nyears Opcional, número de anos que será utilizado do histórico para as computações.
#'
#' @return Pode ser o mesmo df_original com a projeção ou apenas o histórico média do fator sazonal.
#'
#' @examples
#' \dontrun{
#' seas_ratio(df_original = df_cleaned,
#'            df_dessaz = df_forecast_dessaz,
#'            nyears = 5)
#' }
#'
#' @export

seas_ratio <- function(df_original, df_dessaz, nyears = NULL) {

  if(!all(c('date', 'vl') %in% colnames(df_original))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }
  
  df_ratio = df_original %>%
    dplyr::rename(original = vl) %>%
    dplyr::right_join(df_dessaz %>%
                 dplyr::rename(dessaz = vl)) %>%
    dplyr::mutate(
      forecast = ifelse(is.na(original), T, F),
      ratio = original / dessaz)

  if (is.null(nyears)) {
    date_filt = df_ratio %>%
      dplyr::filter(!forecast) %>%
      dplyr::mutate(
        year = format(date, '%Y') %>%
          as.numeric(),
        month = format(date, '%m') %>%
          as.numeric()
      )

    month_last_date <- date_filt %>%
      dplyr::filter(date == max(date)) %>%
      purrr::pluck('month')

    date_filt <- date_filt %>%
      dplyr::filter(month == month_last_date) %>%
      dplyr::filter(year == min(year)) %>%
      purrr::pluck('date')

    hist_mean <- df_ratio %>%
      dplyr::filter(date >= date_filt)

  } else {
    hist_mean <- df_ratio %>%
      filter(!forecast) %>%
      dplyr::filter(date >= max(date)- years(nyears))
  }

  hist_mean <- hist_mean %>%
    dplyr::group_by(month = format(date, '%m'),
                    month = as.numeric(month)) %>%
    dplyr::summarise(ratio_mean = mean(ratio, na.rm = T)) %>%
    dplyr::ungroup()

  if(max(df_dessaz$date) <= max(df_original$date)) {
    warning('Como não há projeção no df_dessaz em relação ao df_original, o retorno é a média histórica do fator sazonal.')

    output <- hist_mean
  } else {
    output <- df_ratio %>%
      dplyr::mutate(month = format(date, '%m'),
                    month = as.numeric(month)) %>%
      dplyr::left_join(hist_mean) %>%
      dplyr::mutate(
        original = ifelse(forecast,
                          dessaz * ratio_mean,
                          original)
      ) %>%
      dplyr::rename(vl = original) %>%
      dplyr::select(date, vl, forecast)
  }

  return(output)
}
