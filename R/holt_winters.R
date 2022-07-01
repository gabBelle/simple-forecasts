#' @title Holt-Winters 
#' @name snaive
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
#' teste =holtWinter(df, type = "multiplicative", h = 12)
#'               
#' }
#'
#' @export 

holtWinter <- function(df, type = as.character(), h = numeric()){
  
# Passando para object ts 

    base_ts <- stats::ts(data = df[,"vl"], 
                         start = c(min(df$date) %>% format("%Y") %>% as.numeric(),
                              min(df$date) %>% format("%m") %>% as.numeric()), 
                         end = c(max(df$date) %>% format("%Y") %>% as.numeric(),
                               max(df$date) %>% format("%m") %>% as.numeric()),
                         frequency = get_periodicity(df)$p_nmonths) # A função get_periodicity 

# Filtrangem entre sazonalidade aditiva ou multiplicativa   
    
    if(type == "additive"){
       hw_hat = stats::HoltWinters(base_ts, seasonal = "add")
       hw_projection = forecast:::forecast.HoltWinters(hw_hat, h)
       
    }else if(type == "multiplicative"){
       hw_hat = stats::HoltWinters(base_ts, seasonal = "mult")
       hw_projection = forecast:::forecast.HoltWinters(hw_hat, h)
      
    }else{
    stop("Erro no parâmetro Seasonal")
  }
  
# Organizando o data.frame com a projeção 
    print(hw) # Especicação dos parâmetros alpha, beta e gama 
 
    df_hat = data.frame(hw[["fitted"]]) %>% 
                        dplyr::select(xhat) %>% 
                        dplyr::bind_rows(data.frame(xhat = hw_projection$model$fitted)) %>% 
                        dplyr::rename(projection = xhat.xhat) %>% 
                        dplyr::mutate(vl = ifelse(is.na(xhat), projection, xhat))%>% 
                        tibble::rownames_to_column(var = "date") %>% 
                        dplyr::select(date, vl)
                       
       
    return(df_hw)  
    
}



