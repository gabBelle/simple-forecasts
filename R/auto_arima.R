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
#' model_arima = arimaUnivariate(df, h = 24)
#'               
#' }
#'
#' @export 

arimaUnivariate <- function(df, h = numeric()){
  
    base_ts <- stats::ts(data = df[,"vl"], 
                         start = c(min(df$date) %>% format("%Y") %>% as.numeric(),
                                 min(df$date) %>% format("%m") %>% as.numeric()), 
                         end = c(max(df$date) %>% format("%Y") %>% as.numeric(),
                               max(df$date) %>% format("%m") %>% as.numeric()),
                         frequency = get_periodicity(df)$p_nmonths) # A função get_periodicity 

# Modelando auto.arima       
    
    model_arima = forecast::auto.arima(base_ts, 
                                       stepwise = T, 
                                       approximation = T)
    print(model_arima)# ckeck do model.arima e coefficients 
    projection_arima <- data.frame(forecast::forecast(model_arima, h))

    
      return(projection_arima)
}

