#' @title Lf Inflation 
#' @name lf_inflation
#'
#' @description Aplicação do método de Inflacionamento de projeção real 
#'
#' @param df DataFrame contendo a série real limpa e organizada;
#' @param sid Representa o índice de preço selecionado para o inflacionamento
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
#' lf_inflation <- function(df, sid)
#'               
#' }
#'
#' @export 

lf_inflation <- function(df, sid, auth_path){
  
  if(str_sub(sid, start = 3, end = 5)== 'PRC') && 
    if(str_sub(sid, start = -2, end = -1)== 'MM') {
 
    inflation <- load_clean_series(sid, auth_path) 
    }else{
      stop("ERRO: SID selecionado imcompatível com o grupo de índice de preço ou série não mensal")
  }
  # if(max(df$date) >= max(inflation$date)) {
    
    model_lf <- df %>% 
                dplyr::rename(vl_real = vl) %>% 
                dplyr::left_join(inflation, by = "date") %>% 
                dplyr::mutate(vl = ifelse(forecast.x, 
                              vl_real*(1+vl), vl_real))
    
    }else{ 
      stop("ERRO: o data.frame deve ter projeção em relação ao original!")
  }
  
}
  
  
  
  
