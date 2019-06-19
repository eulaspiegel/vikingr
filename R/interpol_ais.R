#' AIS interpolation
#'
#' Interpolates missing values in tibble created by *read_ais* and selects all unique mmsis.
#'
#' This function takes a tibble as input (create it with read_ais and add timestamps as first column)
#'     and outputs a list with two tibbles, one with unique info for every mmsi and one complete 
#'     tibble with interpolated data.
#' @section Warning:
#' Not tested for more than the datasets suggested or included in this package.
#' @importFrom tibble tibble
#' @importFrom dplyr group_by summarise_all funs n_distinct select arrange desc first left_join
#' @importFrom stats na.omit
#' @param x A tibble (*read_ais* output), with timestamp as first column.
#' @return List with two tibbles. 1) All unique mmsis 2) complete input tibble with interpolated data
#' @export

interpol_ais <- function (x) {
  interpol_ais_int(x)
}

mmsi = NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @noRd
interpol_ais_int <- function (x) {
  df_t <- x %>% 
    group_by(mmsi) %>% 
    summarise_all(.funs = funs(n_distinct(na.omit(.))))
  
  vari <- colnames(df_t)[colSums(df_t)<=nrow(df_t)]
  
  df2 <- x %>% 
    select(mmsi, timestamp, vari) %>% 
    group_by(mmsi) %>% 
    arrange(desc(timestamp)) %>% 
    summarise_all(funs(first(na.omit(.))))
  
  df3 <- x %>% 
    select(-c(vari)) %>% 
    left_join(df2)
  
  return(list(df2, df3))
}