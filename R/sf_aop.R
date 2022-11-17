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
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#'
#' O @param target_aop indica o valor para média de período desejado, idealmete advindo de uma projeção anual.
#' Por exemplo, a projeção anual do LatamFocus aponta média de 15% para 2022 e 12% para 2023. Pode-se preencher:
#' target_aop = c(0.15,0.12). Caso a quantidade de valores seja menor que o número de anos a ser projetado, repete-se o último valor.
#'
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método,
#' e com a coluna forecast indicando quais observações são projeção.
#'
#' @examples
#' \dontrun{
#' sf_aop(df, end_forecast = '2026-12-01',
#'        target_aop = c(11.5,10.5,10.1,9.8,9.5)
#'        )
#' }
#'
#' @export

sf_aop <- function(df, end_forecast, target_aop, is_yoy = F) {

  periodicity <- get_periodicity(df)

  df_forecast <- df %>%
    expand_series(end_forecast) %>%
    mutate(year = format(date, '%Y') %>% as.numeric(),
           month = format(date, '%m') %>% as.numeric())

  target_aop <- check_vector_len(df_forecast, target_aop)
  if(is_yoy) {
    target_yoy = target_aop
  } else {
    target_yoy <- calc_yoy(df_forecast, target_aop)
  }

  months_by_year <- df_forecast %>%
    filter(forecast) %>%
    group_by(year) %>%
    summarise(n_months = n()) %>%
    ungroup() %>%
    mutate(yoy = target_yoy)

  df_yoy <- df_forecast %>%
    left_join(months_by_year %>% select(year, yoy))

  if(length(unique(months_by_year$n_months)) > 1) {
    #Caso o end_forecast seja acabe antes do último mes do ano
    #Tbm apontará problema. Fix no futuro

    df_yoy_adj <- df_yoy %>%
      mutate(yoy = case_when(
        !forecast & !is.na(yoy) ~ vl/lag(vl, periodicity$p_nmonths) - 1,
        T ~ yoy
      ))

    ##Ajusta o YoY do restante do mês para compatibilizar com a média desejada
    target_first_year <- months_by_year %>%
      filter(year == min(year)) %>%
      pluck('yoy')

    sum_yoy_hist <- df_yoy_adj %>%
      filter(!forecast) %>%
      filter(year == max(year)) %>%
      group_by(year) %>%
      summarise(sum = sum(yoy)) %>%
      ungroup() %>%
      pluck('sum')

    new_yoy = (periodicity$p_nmonths * target_first_year - sum_yoy_hist) /
      months_by_year[1,]$n_months

    df_yoy <- df_yoy_adj %>%
      mutate(yoy = case_when(
        forecast & year == months_by_year[1,]$year ~ new_yoy,
        T ~ yoy
      ))
  }

  df_out <- df_yoy %>%
    mutate(yoy = ifelse(!forecast, NA, yoy))

  for (dt in filter(df_yoy, forecast)$date) {
    forecast_vl <- df_out %>%
      filter(date <= dt) %>%
      mutate(vl = ifelse(is.na(vl),
                         (1+yoy) * lag(vl, periodicity$p_nmonths),
                         vl)) %>%
      filter(date == dt) %>%
      pluck('vl')

    df_out <- df_out %>%
      mutate(vl = ifelse(date == dt, forecast_vl, vl))

  }

  df_out <- df_out %>%
    select(date, vl, forecast)

  return(df_out)

}
