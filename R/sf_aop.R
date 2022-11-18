#' @title Average of period forecasting
#' @name sf_aop
#'
#' @description A projeção é realizada utilizando um input manual, que deve corresponder ao YoY médio para o ano.
#' A função ajusta o YoY para os meses restantes do ano, levando em conta os dados realizados, para que o YoY médio corresponda ao input.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada;
#' @param end_forecast Data (YYYY-MM-DD) contendo o último mês a ser projetado;
#' @param target_aop Vetor de valores indicando a projeção desejada para média de período;
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' {date}: Data da observação:
#' {vl}: valor da observação;
#'
#' O @param target_aop indica o valor para média de período desejado, idealmete advindo de uma projeção anual.
#' Por exemplo, a projeção anual do LatamFocus aponta média de 15% para 2022 e 12% para 2023. Pode-se preencher:
#' target_aop = c(15,12). Caso a quantidade de valores seja menor que o número de anos a ser projetado, repete-se o último valor.
#'
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método,
#' e com a coluna forecast indicando quais observações são projeção.
#'
#' @examples
#' sf_aop(df, end_forecast = '2026-12-01',
#'        target_aop = c(11.5,10.5,10.1,9.8,9.5)
#'        )
#'
#' @export

sf_aop <- function(df, end_forecast, target_aop, is_yoy = F) {

  periodicity <- get_periodicity(df)

  df_forecast <- df %>%
    expand_series(end_forecast) %>%
    dplyr::mutate(year = base::format(date, '%Y') %>% base::as.numeric(),
                  month = base::format(date, '%m') %>% base::as.numeric())

  target_aop <- check_vector_len(df_forecast, target_aop)
  if(is_yoy) {
    target_yoy = target_aop
  } else {
    target_yoy <- calc_yoy(df_forecast, target_aop)
  }

  months_by_year <- df_forecast %>%
    dplyr::filter(forecast) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(n_months = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(yoy = target_yoy)

  df_yoy <- df_forecast %>%
    dplyr::left_join(months_by_year %>% dplyr::select(year, yoy))

  if(base::length(base::unique(months_by_year$n_months)) > 1) {
    #Caso o end_forecast seja acabe antes do último mes do ano
    #Tbm apontará problema. Fix no futuro

    df_yoy_adj <- df_yoy %>%
      dplyr::mutate(yoy = dplyr::case_when(
        !forecast & !base::is.na(yoy) ~ vl/dplyr::lag(vl, periodicity$p_nmonths) - 1,
        T ~ yoy
      ))

    ##Ajusta o YoY do restante do mês para compatibilizar com a média desejada
    target_first_year <- months_by_year %>%
      dplyr::filter(year == base::min(year)) %>%
      purrr::pluck('yoy')

    sum_yoy_hist <- df_yoy_adj %>%
      dplyr::filter(!forecast) %>%
      dplyr::filter(year == base::max(year)) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(sum = base::sum(yoy)) %>%
      dplyr::ungroup() %>%
      purrr::pluck('sum')

    new_yoy = (periodicity$p_nmonths * target_first_year - sum_yoy_hist) /
      months_by_year[1,]$n_months

    df_yoy <- df_yoy_adj %>%
      dplyr::mutate(yoy = dplyr::case_when(
        forecast & year == months_by_year[1,]$year ~ new_yoy,
        T ~ yoy
      ))
  }

  df_out <- df_yoy %>%
    dplyr::mutate(yoy = base::ifelse(!forecast, NA, yoy))

  for (dt in filter(df_yoy, forecast)$date) {
    forecast_vl <- df_out %>%
      dplyr::filter(date <= dt) %>%
      dplyr::mutate(vl = ifelse(base::is.na(vl),
                         (1+yoy) * dplyr::lag(vl, periodicity$p_nmonths),
                         vl)) %>%
      dplyr::filter(date == dt) %>%
      purrr::pluck('vl')

    df_out <- df_out %>%
      dplyr::mutate(vl = base::ifelse(date == dt, forecast_vl, vl))

  }

  df_out <- df_out %>%
    dplyr::select(date, vl, forecast)

  return(df_out)

}
