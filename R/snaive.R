#' @title Seasonal naïve method
#' @name snaive
#'
#' @description Aplicação do método Seasonal naïve, no qual cada previsão é igual ao último valor observado da mesma estação (por exemplo, o mesmo mês do ano anterior)
#'
#' @param df DataFrame contendo a série limpa e organizada;
#' @param nmeans Numérico indicando o número de anos para aplicar a média.
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
#' snaive(df, drift = F, nmeans = 1)
#'               
#' }
#'
#' @export

snaive <- function(df, nmeans = numeric()){
  
    serie <- expand_series(df,
                           end_projection = '2026-12-01') %>% 
             dplyr::mutate(month = month(date))
      
    base_date <- df %>% 
                 dplyr::mutate(year = lubridate::year(date), 
                               month = lubridate::month(date), 
                               day = lubridate::day(date)) %>% 
                 dplyr::filter(date >= max(date)- years(nmeans)) %>%  
                 dplyr::group_by(month) %>% 
                 dplyr::summarise(vl_mean = mean(vl)) %>% 
                 dplyr::ungroup()
      
    output <- serie %>% 
                 dplyr::left_join(base_date) %>% 
                 dplyr::mutate(vl = ifelse(is.na(vl), vl_mean, vl))
  
    return(data.frame(output))
} 



