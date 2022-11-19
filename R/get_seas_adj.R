#' @title Seasonality adjust
#' @name get_seas_adj
#'
#' @description Calcula o dessaz de uma série com as configurações automáticas
#' do STL, do X13, ou da média entre eles.
#'
#' @param df Dataframe contendo a série a ser dessazonalizada;
#' @param type Chr (STL, X13, mean) tipo de dessaz a ser aplicado.
#'
#' @details
#' Chama as funções do pacote, get_stl e get_x13, onde de fato são feitas as estimações.
#' A função realiza dessaz tanto de série mensal quanto de série trimestral, sem necessidade de parâmetro extra.
#'
#' @author Gabriel Bellé
#'
#' @return O retorno é um df contendo os valores da série dessazonalidada e a data.
#'
#' @examples
#' get_seas_adj(df = cleaned_df, type = 'mean')
#'
#' @export

get_seas_adj <- function(df, type = 'mean') {

  if(!all(c('date', 'vl') %in% base::colnames(df))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
  }

  if('forecast' %in% base::colnames(df)) {
    col_forecast <- df %>%
      dplyr::select(c(date, forecast))
  }

  periodicity <- get_periodicity(df)

  if(type == 'STL') {
    df_dessaz = get_stl(df) %>%
      dplyr::rename(vl = 'stl')

  } else if (type == 'X13') {
    df_dessaz = get_x13(df) %>%
      dplyr::rename(vl = 'x13')

  } else {
    df_stl = get_stl(df)
    df_x13 = get_x13(df)

    df_dessaz <- df_stl %>%
      dplyr::left_join(df_x13) %>%
      stats::na.omit()

    if(type == 'mean') {
    df_dessaz <- df_dessaz %>%
      dplyr::mutate(vl = purrr::pmap(.l = Filter(is.numeric, .),
                                     .f = purrr::lift_vd(..f = mean)),
                    vl = base::as.numeric(vl)
                    ) %>%
      select(-c(x13, stl))

    } else {
      stop("ERRO: type selecionado incompatível")
    }
  }

  if('forecast' %in% base::colnames(df)) {
    df_dessaz <- df_dessaz %>%
      dplyr::left_join(col_forecast)
  }

  return(base::data.frame(df_dessaz))

}
