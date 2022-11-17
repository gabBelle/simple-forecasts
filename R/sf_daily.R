#' @title Daily interpolation
#' @name sf_daily
#'
#' @description Realiza a projeção de uma série diária, com base em uma série mensal já com projeção.
#' O último dia da série diária será igual ao valor da série mensal fornecida.
#' A interpolação feita é a linear, baseada nas funções 'naive' e 'drift_target'.
#' A projeção da série diária considera apenas os dias úteis, seguindo calendário da Anbima.
#'
#' @author Gabriel Bellé
#'
#' @param target Dataframe contendo a série limpa e organizada, mensal e com projeção que será o alvo da projeção diária;
#' @param ... Dataframe, N dfs que serão projetados utilizando a projeção contida em target
#'
#' @details
#' O @param target de entrada deve conter pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação;
#'
#' @return Retorna um dataframe com colunas de \code{date}, \code{forecast} e uma
#' coluna de valor para cada série que foi projetada, na mesma ordem em que foi inputada na função.
#'
#' @examples
#' \dontrun{
#' sf_daily(target = cambio_real_mensal,
#'          cambio_diario_venda, cambio_diario_compra) {
#' }
#'
#' @export

sf_daily <- function(target, ...) {

  if('forecast' %in% colnames(target)) {
    target <- target %>%
      dplyr::select(-forecast)
  }

  df_diario <- list(...) %>%
    purrr::reduce(left_join, by = 'date')

  cal_br <- bizdays::create.calendar("Brazil/ANBIMA",
                                     bizdays::holidays("Brazil/ANBIMA"),
                                     weekdays=c("saturday", "sunday"))

  df_target <- target %>%
    mutate(date = lead(date) - 1,
           date = ifelse(is.na(date), as.Date('2050-12-31'),date),
           date = as.Date(date, origin = '1970-01-01'),
           date = bizdays::adjust.previous(date,cal_br)) %>%
    filter(date>max(df_diario$date)) %>%
    rename(target_vl = vl)

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

