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

  if('forecast' %in% base::colnames(target)) {
    target <- target %>%
      dplyr::select(-forecast)
  }

  df_diario <- list(...) %>%
    purrr::reduce(left_join, by = 'date')

  cal_br <- bizdays::create.calendar("Brazil/ANBIMA",
                                     bizdays::holidays("Brazil/ANBIMA"),
                                     weekdays=c("saturday", "sunday"))

  df_target <- target %>%
    dplyr::mutate(date = dplyr::lead(date) - 1,
           date = base::ifelse(base::is.na(date), base::as.Date('2050-12-31'), date),
           date = base::as.Date(date, origin = '1970-01-01'),
           date = bizdays::adjust.previous(date,cal_br)) %>%
    dplyr::filter(date>base::max(df_diario$date)) %>%
    dplyr::rename(target_vl = vl)

  daily_dates <- bizdays::bizseq(base::max(df_diario$date)+1,
                                 base::max(df_target$date),
                                 cal_br)

  df_merged <- df_diario %>%
    dplyr::mutate(forecast = F) %>%
    dplyr::bind_rows(tibble(date = daily_dates,
                     forecast = T)) %>%
    dplyr::left_join(df_target) %>%
    dplyr::mutate(dplyr::across(starts_with('vl'), ~base::ifelse(base::is.na(.x), target_vl, .x)),
           month = base::format(date, '%Y-%m'))


  df_drift <- df_merged %>%
    #Seleciona o ultimo dado realizado
    dplyr::filter(!forecast) %>%
    dplyr::filter(date == base::max(date)) %>%
    #Concatena com o período a ser projetado
    dplyr::bind_rows(dplyr::filter(df_merged, forecast)) %>%
    #Faz a contagem de dias por mês
    dplyr::group_by(month, forecast) %>%
    dplyr::mutate(n_days = n()) %>%
    #Mantem apenas o target e o último realizado
    #Junto com a contagem de dias
    dplyr::filter(!is.na(vl.x)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(target_vl)) %>%
    #Para cada X a ser projetado,
    #Calcula o drift (quanto tem q ser adicionado por dia para chegar até o prox valor)
    tidyr::pivot_longer(-c(date, month, forecast, n_days)) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(drift = (value - dplyr::lag(value, 1)) / n_days) %>%
    dplyr::ungroup() %>%
    stats::na.omit() %>%
    dplyr::group_by(month) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(name,month, drift)

  df_forecast <- df_merged %>%
    #Pivota o dado realizado + NA no período a ser projetado
    dplyr::select(-target_vl) %>%
    tidyr::pivot_longer(-c(date, forecast, month)) %>%
    #Adiciona o drift
    dplyr::left_join(select(df_drift,name, month, drift)) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(drift = base::ifelse(!forecast, 0, drift),
           drift = base::cumsum(drift),
           value = base::ifelse(forecast, NA, value)) %>%
    dplyr::fill(value, .direction = 'down') %>%
    dplyr::mutate(value = value + drift) %>%
    dplyr::select(-c(month, drift)) %>%
    tidyr::pivot_wider(names_from = name, values_from = value)

  return(df_forecast)
}

