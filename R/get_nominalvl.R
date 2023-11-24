#' @title Calculate real values
#' @name get_nominalvl
#'
#' @description A função calcula o a inflação de uma série real.
#'
#' @author Gabriel Bellé
#'
#' @param df Dataframe contendo a série nominal limpa e organizada;
#' @param df_cpi Dataframe contendo a série de inflação da moeda;
#' @param dt_base_in Ano e mês base de inicio para o indice de inflação.
#' @param dt_base_fim Ano e mês base de fim para o indice de inflação.
#'
#' @details
#' Todos dataframes devem começar pelo menos 1 mês antes da data inserida em mes_base.
#'
#' @return Retorna um dataframe contendo a série real.
#'
#' @examples
#' get_nominalvl(price_fuel, #Preço gasolina
#'               ipca_br,
#'               '2022-12-01'
#'            )
#'
#' @export

get_nominalvl <- function(df, df_cpi, dt_base_in, dt_base_fim = NULL) {
  dt_in = base::as.Date(dt_base_in)
  dt_fim = base::as.Date(dt_base_fim)

  if(length(dt_fim) == 0) {
    dt_fim = dt_in
  }

  cpi_base <- df_cpi %>%
    filter(date >= dt_in & date <= dt_fim) %>%
    summarise(vl = mean(vl, na.rm = T)) %>%
    ungroup()

  cpi_vl = cpi_base$vl

  if(is.nan(cpi_vl)) {
    return(warning('Data informada indisponível na série de inflação'))
  }

  nominal <- df %>%
    dplyr::rename(real = vl) %>%
    dplyr::left_join(df_cpi %>%
                       dplyr::rename(cpi = vl)) %>%
    dplyr::mutate(nominal = real * (cpi / cpi_vl))

  return(nominal)
}
