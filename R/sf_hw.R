#' @title Holt-Winters
#' @name sf_hw
#'
#' @description Aplicação do método Holt-Winters para projeção
#'
#' @param df DataFrame contendo a série limpa e organizada;
#' @param type Tipo de Holt-Winters: multiplicative, additive, trend e NULL (default);
#' @param end_forecast representa a data final, último mês ou trimestre, da projeção.
#'
#' @author Luiz Paulo T.
#'
#' @return Retorna o mesmo df de input, porém a projeção formalmente aplicada com o método Holt-Winters
#'
#' @examples
#' sf_hw(df, type = NULL, end_forecast = "2026-12-01")
#'
#' @export


sf_hw <- function(df, type = NULL, end_forecast){

    if(!all(c('date', 'vl') %in% base::colnames(df))) {
      stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }

    serie <- expand_series(df,
                           end_forecast) # Chamando função expand_series

    hw_base <- df %>%
               dplyr::mutate(date = tsibble::yearmonth(as.character(date))) %>%
               tsibble::as_tsibble(index = date)

# Filtrangem entre sazonalidade aditiva-multiplicativa e tendência

    if(base::is.null(type)){

    hw_model <- hw_base %>% fabletools::model(fable::ETS(vl))

    } else if(type == "multiplicative"){

    hw_model = hw_base %>% fabletools::model(fable::ETS(vl ~ error("M") + trend("A") + season("M")))

    } else if(type == "additive"){

    hw_model = hw_base %>% fabletools::model(fable::ETS(vl ~ error("A") + trend("A") + season("A")))

      }else if(type == "trend"){

    hw_model = hw_base %>% fabletools::model(fable::ETS(vl ~ error("A") + trend("A") + season("N")))

    }else{
      stop("ERRO: Type inexistente")
  }

    hw_projection = hw_model %>%
                    fabletools::forecast(h = (zoo::as.yearmon(max(serie$date))- zoo::as.yearmon(max(df$date)))*get_periodicity(df)$p_nmonths)


    result_projection <- base::data.frame(hw_projection) %>%
                         dplyr::select(date,.mean) %>%
                         dplyr::rename(vl = .mean) %>%
                         dplyr::bind_rows(select(df, date, vl)) %>%
                         dplyr::arrange(-desc(date)) %>%
                         dplyr::full_join(select(serie, date, forecast), by = "date")

    return(base::data.frame(result_projection))

}

