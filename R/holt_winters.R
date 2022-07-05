#' @title Holt-Winters 
#' @name holtWinter
#'
#' @description Aplicação do método Holt-Winters para projeção 
#'
#' @param df DataFrame contendo a série limpa e organizada;
#' @param type Tipo de Holt-Winters: mutiplicativo, aditivo, tendência e default;
#' @param end_projection representa a data final, último mês ou trimestre, da projeção.
#'
#' @author Luiz Paulo T. 
#'
#' @details As opções de type são multiplicative, additive, trend e NULL. O quais representam, respectivamente: sazonalidade mutiplicativa e aditiva, o modelo Holt-Winters com tendência e, por fim, NULL representa o default, isto é, a escolha automática via critério de informação. 
#' @details O input deve ser um data.frame limpo e organizado:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método Holt-Winters 
#'
#' @examples
#' \dontrun{
#' holtWinter(df, type = NULL, end_projection = "2026-12-01")
#'               
#' }
#'
#' @export 


holtWinter <- function(df, type = NULL, end_projection){

    serie <- expand_series(df,
                           end_projection) # Chamando função expand_series 

# Filtrangem entre sazonalidade aditiva-multiplicativa e tendência 
    
    if(is.null(type)){
    hw_model <- df %>%
                dplyr::mutate(date = tsibble::yearmonth(as.character(date))) %>%
                tsibble::as_tsibble(index = date) %>% 
                fabletools::model(fable::ETS(vl)) %>% 
                fabletools::report()

    hw_projection = hw_model %>% 
                    fabletools::forecast(h = (zoo::as.yearmon(max(serie$date))- zoo::as.yearmon(max(df$date)))*get_periodicity(df)$p_nmonths)
      
    } else if(type == "multiplicative"){
      
    hw_model <- df %>%
                dplyr::mutate(date = tsibble::yearmonth(as.character(date))) %>%
                tsibble::as_tsibble(index = date) %>% 
                fabletools::model(fable::ETS(vl ~ error("M") + trend("A") + season("M"))) %>% 
                fabletools::report()
      
    hw_projection = hw_model %>% 
                    fabletools::forecast(h = (zoo::as.yearmon(max(serie$date))- zoo::as.yearmon(max(df$date)))*get_periodicity(df)$p_nmonths)
      
    } else if(type == "additive"){
      
    hw_model <- df %>%
                dplyr::mutate(date = tsibble::yearmonth(as.character(date))) %>%
                tsibble::as_tsibble(index = date) %>% 
                fabletools::model(fable::ETS(vl ~ error("A") + trend("A") + season("A"))) %>% 
                fabletools::report()
      
    hw_projection = hw_model %>% 
                    fabletools::forecast(h = (zoo::as.yearmon(max(serie$date))- zoo::as.yearmon(max(df$date)))*get_periodicity(df)$p_nmonths)
  
      }else if(type == "trend"){
    
    hw_model <- df %>%
                dplyr::mutate(date = tsibble::yearmonth(as.character(date))) %>%
                tsibble::as_tsibble(index = date) %>% 
                fabletools::model(fable::ETS(vl ~ error("A") + trend("A") + season("N"))) %>% 
                fabletools::report()
      
    hw_projection = hw_model %>% 
                    fabletools::forecast(h = (zoo::as.yearmon(max(serie$date))- zoo::as.yearmon(max(df$date)))*get_periodicity(df)$p_nmonths)  
      
    }else{
      stop("ERRO: Type inexistente")
  }
      
    result_projection <- data.frame(hw_projection) %>% 
                         dplyr::select(date,.mean) %>% 
                         dplyr::rename(vl = .mean) %>% 
                         dplyr::bind_rows(select(df, date, vl)) %>% 
                         dplyr::arrange(-desc(date)) %>% 
                         dplyr::full_join(select(serie, date, forecast), by = "date")
    
    return(data.frame(result_projection))  
    
}

