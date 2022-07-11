#' @title Lf Inflation 
#' @name lf_inflation
#'
#' @description Aplicação do método de Inflacionamento de projeção real.  
#' Pega um série Y real projetada e inflaciona a projeção. 
#'
#'
#' @param df_real DataFrame contendo a série real limpa, organizada e com projeção;
#' @param df_nominal DataFrame contendo a série nominal, limpa e organizada; 
#' @param sid Representa o índice de preço selecionado para o inflacionamento. O qual 
#' necessariamente precisa ter projeção pelo menos no mesmo período da série real; 
#' @param auth_path Chr contendo o caminho para o arquivo de autenticação do series;4macro
#' @param index ....
#' 
#' @author Luiz Paulo T. 
#'
#' @details O input deve ser um data.frame limpo e organizado com a série real:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' @return Retorna o mesmo df de input, porém com a projeção formalmente aplicada com de inflacionamento:
#'
#' @examples
#' \dontrun{
#' 
#' lf_inflation(df_real = df_real, 
#'              df_nominal = df_nominal, index = NULL,
#'              sid = "BRPRC0046000OOMM", auth_path = auth_path)
#'               
#' }
#'
#' @export 

lf_inflation <- function(df_real, df_nominal, sid, auth_path, index = NULL){
  
    if(!all(c('date', 'forecast', 'vl') %in% colnames(df_real))) {
      stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }
  
    if(str_sub(sid, start = 3, end = 5) != "PRC"){
      stop("ERRO: SID selecionado imcompatível com o grupo de índice de preço")
  }
    
   if(!any(df_real$forecast)){
     stop("ERRO: variável real sem projeção")
   }
  
   if(str_sub(sid, start = -2, end = -1) == "ML"){
   
    inflation <- load_clean_series(sid, auth_path)
    inflation <- inflation %>% 
                 dplyr::mutate(growth_rate = (vl - dplyr::lag(vl))/dplyr::lag(vl))
      
    }else if(str_sub(sid, start = -2, end = -1) == "MM"){
      
    inflation <- load_clean_series(sid, auth_path = auth_path)
      
    }else{
      stop("ERRO: ...")
  }
  
    model_lf <- df_real %>% 
                dplyr::rename(vl_real = vl) %>% 
                dplyr::left_join(inflation, by = "date") %>% 
                dplyr::mutate(
                              vl = ifelse(forecast.x, 
                              vl_real*(1+vl), vl_real)) #%>% 
              #dplyr::select()
  
    
    return(data.frame(model_lf))
  
}










