#' @title Auto.Arima Default
#' @name arimaUnivariate
#'
#' @description Aplicação do método Auto.Arima para projeção
#'
#' @param df DataFrame contendo a série limpa e organizada;
#' @param end_forecast representa a data final, último mês ou trimestre, da projeção;
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
#' arimaUnivariate(df, end_forecast = "2026-12-01")
#'
#' }
#'
#' @export

arimaUnivariate <- function(df, end_forecast){

    if(!all(c('date', 'vl') %in% colnames(df))) {
      stop("Há coluna com nome errado/faltante no df fornecido de input!")

  }

    serie <- expand_series(df,
                           end_forecast) # Chamando função expand_series

# Modelando auto.arima  - automático

    auto_arima <- df %>%
                  dplyr::mutate(date = tsibble::yearmonth(as.character(date))) %>%
                  tsibble::as_tsibble(index = date) %>%
                  fabletools::model(fable::ARIMA(vl))
# Janela de projeção
    arima_projection = auto_arima %>%
                       fabletools::forecast(h = (zoo::as.yearmon(max(serie$date))- zoo::as.yearmon(max(df$date)))*get_periodicity(df)$p_nmonths)

# Guardando as projeções

    projection_arima <- data.frame(arima_projection) %>%
                        dplyr::select(date,.mean) %>%
                        dplyr::rename(vl = .mean) %>%
                        dplyr::bind_rows(select(df, date, vl)) %>%
                        dplyr::arrange(-desc(date)) %>%
                        dplyr::full_join(select(serie, date, forecast), by = "date")

    return(projection_arima)


  }


