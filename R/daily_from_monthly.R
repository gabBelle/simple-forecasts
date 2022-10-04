#' @title Calculate yoy from target value
#' @name calc_yoy
#'
#' @description A função realiza a conversão de um target em valores, mesma unidade de medida que o dataframe de interesse,
#' para valores em Year over Year (YoY).
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série limpa e organizada;
#' @param target_aop Vetor de valores indicando a projeção desejada para média de período;
#'
#' @details
#' O @param df de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#'
#' O @param target_aop indica o valor para média de período desejado, idealmete advindo de uma projeção anual.
#' Por exemplo, a projeção anual do LatamFocus aponta média de 15% para 2022 e 12% para 2023. Pode-se preencher:
#' target_aop = c(0.15,0.12)
#'
#' @return Retorna um vetor de valores de mesmo cumprimento de @param target_aop,
#' porém com os valores representando o YoY para ser aplicado nos meses do período.
#'
#' @examples
#' \dontrun{
#' calc_yoy <- function(df,
#'                      target_aop = c(11.5,10.5,10.1,9.8,9.5)) {
#' }
#'
#' @export

daily_from_monthly <- function(target, ...) {

  df_diario <- list(...) %>%
    purrr::reduce(left_join, by = 'date')

  #return(df_diario)}

  #df_diario = dolar_forecast
  #df_diario = dolar_forecast %>% filter(date >= '2022-01-01')

  cal_br <- bizdays::create.calendar("Brazil/ANBIMA",
                                     bizdays::holidays("Brazil/ANBIMA"),
                                     weekdays=c("saturday", "sunday"))

  df_target <- target %>%
    mutate(date = lead(date) - 1,
           date = ifelse(is.na(date), as.Date('2050-12-31'),date),
           date = as.Date(date, origin = '1970-01-01'),
           date = bizdays::adjust.previous(date,cal_br)) %>%
    filter(date>max(df_diario$date)) %>%
    rename(target_vl = vl) %>%
    select(-forecast)

  daily_dates <- bizdays::bizseq(max(df_diario$date)+1,
                                 max(df_target$date),
                                 cal_br)

  df_merged <- df_diario %>%
    mutate(forecast = F) %>%
    bind_rows(tibble(date = daily_dates,
                     forecast = T)) %>%
    left_join(df_target) %>%
    mutate(across(starts_with('vl'), ~ifelse(is.na(.x), target_vl, .x)),
           month = format(date, '%Y-%m'))


  df_drift <- df_merged %>%
    #Seleciona o ultimo dado realizado
    filter(!forecast) %>%
    filter(date == max(date)) %>%
    #Concatena com o período a ser projetado
    bind_rows(filter(df_merged, forecast)) %>%
    #Faz a contagem de dias por mês
    group_by(month, forecast) %>%
    mutate(n_days = n()) %>%
    #Mantem apenas o target e o último realizado
    #Junto com a contagem de dias
    filter(!is.na(vl.x)) %>%
    ungroup() %>%
    select(-c(target_vl)) %>%
    #Para cada X a ser projetado,
    #Calcula o drift (quanto tem q ser adicionado por dia para chegar até o prox valor)
    pivot_longer(-c(date, month, forecast, n_days)) %>%
    group_by(name) %>%
    mutate(drift = (value - lag(value, 1)) / n_days) %>%
    ungroup() %>%
    na.omit() %>%
    group_by(month) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    select(name,month, drift)

  df_forecast <- df_merged %>%
    #Pivota o dado realizado + NA no período a ser projetado
    select(-target_vl) %>%
    pivot_longer(-c(date, forecast, month)) %>%
    #Adiciona o drift
    left_join(select(df_drift,name, month, drift)) %>%
    group_by(name) %>%
    mutate(drift = ifelse(!forecast, 0, drift),
           drift = cumsum(drift),
           value = ifelse(forecast, NA, value)) %>%
    fill(value, .direction = 'down') %>%
    mutate(value = value + drift) %>%
    select(-c(month, drift)) %>%
    pivot_wider(names_from = name, values_from = value)

  return(df_forecast)
}

