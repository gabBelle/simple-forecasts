#' @title Seasonality adjust
#' @name get_seas_adj
#'
#' @description Calcula o dessaz de uma série com as configurações automáticas
#' do STL ou do X13, opcional retornar a média dos dois.
#'
#' @param df Dataframe contendo a série a ser dessazonalizada;
#' @param type tipo de dessaz a ser aplicado.
#'
#' @author Gabriel Bellé
#'
#' @details O input deve ser um df contendo pelo as colunas de:
#' \code{date}: Data da observação:
#' \code{vl}: valor da observação.
#'
#' O type pode ser 'STL', 'X13' ou agregações como 'mean' ou 'median'.
#' O padrão é 'median'.
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
                    .f = purrr::lift_vd(..f = mean))) %>% 
      dplyr::select(date, vl)

    } else {
      stop("ERRO: type selecionado incompatível")
    }
  }

  return(data.frame(df_dessaz))
}
