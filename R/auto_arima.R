#' @title Auto.Arima 
#' @name arimaUnivariate
#'
#' @description Aplicação do método Auto.Arima para projeção  
#'
#' @param df DataFrame contendo a série limpa e organizada;
#'
#' @author Luiz Paulo T. 
#'
#' @details O input deve ser um data.frame organizado:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método Auto.Arima 
#'
#' @examples
#' \dontrun{
#' 
#' arimaUnivariate(df, end_projection = "2023-12-01")
#'               
#' }
#'
#' @export 

arimaUnivariate <- function(df, end_projection = as.Date()){

    serie <- expand_series(df,
                           end_projection) # Chamando função expand_series 
  
# Modelando auto.arima  - automático      
    
    auto_arima <- df %>%
                  dplyr::mutate(date = tsibble::yearmonth(as.character(date))) %>%
                  tsibble::as_tsibble(index = date) %>% 
                  fabletools::model(fable::ARIMA(vl)) %>% 
                  fabletools::report()
# Janela de projeção     
    arima_projection = auto_arima %>% 
                       fabletools::forecast(h = (zoo::as.yearmon(max(serie$date))- zoo::as.yearmon(max(df$date)))*12)

# Guardando as projeções 
                          
    projection_arima <- data.frame(arima_projection) %>% 
                        dplyr::select(date,.mean) %>% 
                        dplyr::rename(vl = .mean) %>% 
                        dplyr::bind_rows(df) %>% 
                        dplyr::arrange(-desc(date))
    
    return(projection_arima)
  
    }


