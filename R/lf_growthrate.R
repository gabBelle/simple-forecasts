#' @title lf_growthrate 
#' @name lf_growthrate
#'
#' @description Aplicação do método de Taxa de crescimento de projeção.  
#' Captura a taxa de crescimento da projeção Y e aplica na projeção de X.  
#'
#'
#' @param df_forecast DataFrame contendo a série X projetada; 
#' @param sidforecast Série Y importada via SID. Necessariamente precisa ter projeção; 
#' @param auth_path Chr contendo o caminho para o arquivo de autenticação do series.4macro.
#' 
#' @author Luiz Paulo T. 
#'
#' @details O input deve ser um data.frame limpo e organizado com a série X:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#' \code{forecast}: bolleano 
#' 
#' @return Retorna o mesmo df de input, porém com a projeção formalmente aplicada com a captura de taxa de crescimento:
#'
#' @examples
#' \dontrun{
#' 
#' lf_growthrate(df_forecast = df_forecast, 
#'               sidforecast = "BRPUB0001000ROML", 
#'               auth_path = auth_path)
#'               
#' }
#'
#' @export 

lf_growthrate <- function(df_forecast, sidforecast, auth_path){
  
    if(!all(c('date', 'forecast', 'vl') %in% colnames(df_forecast))) {
      stop("Há coluna com nome errado/faltante no df fornecido de input!")
    }
    
    if(!any(df_forecast$forecast)){
      stop("ERRO: Série original sem projeção")
    }
    
    if(str_sub(sidforecast, start = -2, end = -1) == "ML"){
      
      projection <- load_clean_series(sidforecast, auth_path) %>% 
                    dplyr::mutate(growth_rate = (vl - dplyr::lag(vl))/dplyr::lag(vl))
      
    } else if(str_sub(sidforecast, start = -2, end = -1) == "MM"){
      projection <- load_clean_series(sidforecast, auth_path) %>% 
                    dplyr::rename(growth_rate = vl)
      
    }else{
     stop("ERRO: Série selecionada incompatível com ML ou MM") 
    }
    
    if(!any(projection$forecast)){
      stop("ERRO: Série importada via SID sem projeção")
    }
    
    model_growthrate <- df_forecast %>% 
                        dplyr::rename(vl_original = vl, 
                                      forecast_original = forecast) %>% 
                        dplyr::right_join(projection, by = "date") %>% 
                        stats::na.omit() %>% 
                        dplyr::mutate(vl_forecast = ifelse(forecast_original & forecast, 
                                                           vl_original*(1+growth_rate), vl_original), 
                                      new_forecast = ifelse(forecast_original & forecast, TRUE, FALSE)) %>% 
                        dplyr::select(date, vl_forecast, new_forecast)
  
      
      return(data.frame(model_growthrate))                      
    

}


