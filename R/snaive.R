#' @title Seasonal naïve method 
#' @name snaive
#'
#' @description Aplicação do método Seasonal naïve, no qual cada previsão é igual ao último valor observado da mesma estação (por exemplo, o mesmo mês do ano anterior) ou a média dos anos escolhida; 
#'
#' @param df DataFrame contendo a série limpa e organizada;
#' @param nyears Numérico indicando o número de anos para aplicar a média;
#' @param end_forecast representa a data final para a projeção;
#' @param drift aplicação de drift. Default é FALSE 
#'
#'
#' @author Luiz Paulo T. 
#'
#' @details O input deve ser um data.frame organizado:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método Seasonal naïve
#'
#' @examples
#' \dontrun{
#' snaive(df, drift = FALSE, nyears = 1, end_forecast = "2026-12-01")
#'               
#' }
#'
#' @export 

snaive <- function(df, drift = FALSE, nyears = NULL, end_forecast){
  
    if(!all(c('date', 'vl') %in% colnames(df))) {
      stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }
  
    serie <- expand_series(df,
                           end_forecast) %>% 
             dplyr::mutate(month = lubridate::month(date))
      
    base_date <- df %>% 
                 dplyr::mutate(year = lubridate::year(date), 
                               month = lubridate::month(date), 
                               day = lubridate::day(date)) %>% 
                 dplyr::filter(date >= max(date)- years(nyears)) %>%  
                 dplyr::group_by(month) %>% 
                 dplyr::summarise(vl_mean = mean(vl)) %>% 
                 dplyr::ungroup()
    
    output <- serie %>% 
                 dplyr::left_join(base_date) %>% 
                 dplyr::mutate(vl = ifelse(is.na(vl), vl_mean, vl)) %>% 
                 dplyr::select(date, vl) %>% 
                 dplyr::full_join(select(serie, date, forecast), by = "date")
    
    return(data.frame(output))
} 


