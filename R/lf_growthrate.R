

lf_growthrate <- function(sid.projection, df){
  
    projection <- load_clean_series(sid.projection, auth_path)
  
    rate_projection <- projection %>% 
                       dplyr::mutate(growth_rate = (vl - dplyr::lag(vl))/dplyr::lag(vl)) %>% 
                      DPL
  return(rate_projection)
  
}


