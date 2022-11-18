#' @title Seasonal naïve method
#' @name sf_snaive
#'
#' @description Método Seasonal Naïve, no qual cada previsão é igual ao último valor observado da mesma estação
#' (por exemplo, o mesmo mês do ano anterior) ou a média dos meses de X anos.
#'
#' @param df DataFrame contendo a série limpa e organizada;
#' @param nyears Opcional, número de anos que será utilizado do histórico para as computações. Se não preenchido, usa o histórico inteiro.
#' @param end_forecast Chr representa a data final para a projeção;
#'
#' @author Luiz Paulo T.
#'
#' @details
#' O @param df de entrada deve conter, pelo menos, as colunas de:
#' {date}: Data da observação:
#' {vl}: valor da observação.
#'
#' @return Retorna o mesmo df de input, porém com a projeção Seasonal naïve.
#'
#' @examples
#' sf_snaive(df, nyears = 1, end_forecast = "2026-12-01")
#'
#'
#' @export

sf_snaive <- function(df, nyears = NULL, end_forecast){
  if(!all(c('date', 'vl') %in% base::colnames(df))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }

  df <- df %>%
    dplyr::mutate(
      year = base::format(date, '%Y') %>%
        base::as.numeric(),
      month = base::format(date, '%m') %>%
        base::as.numeric()
    )

  serie <- expand_series(df,
                         end_forecast) %>%
    dplyr::mutate(month = lubridate::month(date))

  if (is.null(nyears)) {
    month_last_date <- df %>%
      dplyr::filter(date == base::max(date)) %>%
      purrr::pluck('month')

    date_filt <- df %>%
      dplyr::filter(month == month_last_date) %>%
      dplyr::filter(year == base::min(year)) %>%
      purrr::pluck('date')

    df <- df %>%
      dplyr::filter(date >= date_filt)

  } else {
    df <- df %>%
      dplyr::filter(date >= base::max(date) - lubridate::years(nyears))
  }

  base_date <- df %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(vl_mean = mean(vl)) %>%
    dplyr::ungroup()

  output <- serie %>%
    dplyr::left_join(base_date) %>%
    dplyr::mutate(vl = ifelse(base::is.na(vl), vl_mean, vl)) %>%
    dplyr::select(date, vl) %>%
    dplyr::full_join(dplyr::select(serie, date, forecast), by = "date")

  return(base::data.frame(output))
}


