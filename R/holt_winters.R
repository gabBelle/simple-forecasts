#' @title Holt-Winters 
#' @name holtWinter
#'
#' @description Aplicação do método Holt-Winters para projeção 
#'
#' @param df DataFrame contendo a série limpa e organizada;
#' @param type Tipo de decomposição da sazonalidade: mutiplicativa ou aditiva; 
#'
#' @author Luiz Paulo T. 
#'
#' @details O input deve ser um data.frame organizado:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método Holt-Winters 
#'
#' @examples
#' \dontrun{
#' test_mult = holtWinter(df, type = "multiplicative", end_projection = "2026-12-01")
#'               
#' }
#'
#' @export 

holtWinter <- function(df, type = as.character(), end_projection = as.Date()){

    serie <- expand_series(df,
                           end_projection) # Chamando função expand_series 

# Filtrangem entre sazonalidade aditiva-multiplicativa e tredência 
    
    if(type == "additive"){
       hw_model <- df %>%
                   dplyr::mutate(date = tsibble::yearmonth(as.character(date))) %>%
                   tsibble::as_tsibble(index = date) %>% 
                   fabletools::model(fable::ETS(vl ~ error("A") + trend("A") + season("A")))

    hw_projection = hw_model %>% 
                       fabletools::forecast(h = (zoo::as.yearmon(max(serie$date))- zoo::as.yearmon(max(df$date)))*12)
      
    } else if(type == "multiplicative"){
      
    hw_model <- df %>%
                dplyr::mutate(date = tsibble::yearmonth(as.character(date))) %>%
                tsibble::as_tsibble(index = date) %>% 
                fabletools::model(fable::ETS(vl ~ error("M") + trend("A") + season("M")))
      
    hw_projection = hw_model %>% 
                    fabletools::forecast(h = (zoo::as.yearmon(max(serie$date))- zoo::as.yearmon(max(df$date)))*12)
      
    } else if(type == "trend"){
      
    hw_model <- df %>%
                dplyr::mutate(date = tsibble::yearmonth(as.character(date))) %>%
                tsibble::as_tsibble(index = date) %>% 
                fabletools::model(fable::ETS(vl ~ error("A") + trend("A") + season("N")))
      
    hw_projection = hw_model %>% 
                    fabletools::forecast(h = (zoo::as.yearmon(max(serie$date))- zoo::as.yearmon(max(df$date)))*12)
    }else{
      stop("ERRO: Type inexistente")
  }
      
    result_projection <- data.frame(hw_projection) %>% 
                         dplyr::select(date,.mean) %>% 
                         dplyr::rename(vl = .mean) %>% 
                         dplyr::bind_rows(df) %>% 
                         dplyr::arrange(-desc(date))
    
    return(data.frame(result_projection))  
    
}

