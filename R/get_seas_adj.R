#' @title Seasonality adjust
#' @name get_seas_adj
#'
#' @description Calcula o dessaz de uma série com as configurações automáticas
#' do STL ou do X13, opcional retornar a média dos dois.
#'
#' @param df Dataframe contendo a série a ser dessazonalizada;
#' @param type Chr especificando tipo de dessaz a ser aplicado (STL, X13, mean).
#'
#' @author Gabriel Bellé
#'
#' @details
#' O input @param df deve ser um DataFrame contendo pelo menos as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' @return O retorno é um df contendo os valores da série dessazonalidada e a data.
#'
#' @examples
#' \dontrun{
#' get_seas_adj(df = cleaned_df, type = 'median')
#' }
#'
#' @export

get_seas_adj <- function(df, type = 'mean') {

  if(!all(c('date', 'vl') %in% base::colnames(df))) {
    stop("Há coluna com nome errado/faltante no df fornecido de input!")
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
      dplyr::select(date, vl)

    } else {
      stop("ERRO: type selecionado incompatível")
    }
  }

  return(base::data.frame(df_dessaz))

}
