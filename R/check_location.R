#' check_location
#' @description
#' Auxiliary function
#'
#' @param dados data set containing variable
#' @param codigos vector containing codes of states
#' @param code column from the data set code e.g. 'cod_regint' or 'cod_mun'
#' @param variables names of variables from data set
#'
#' @returns data set with results

check_location <- function(dados,codigos, code, variables){
  res <- dados |>
    dplyr::select(ano,uf, cod_regint, cod_mun, variables) |>
    dplyr::left_join(codigos) |>
    dplyr::mutate(initial_code = substr(x = !!code, start = 1,stop = 2),
                  resultado = ifelse(codigos_uf == initial_code, F, T) ) |>
    dplyr::select(-initial_code,-codigos_uf) |>
    tidyr::pivot_longer(cols = !c(ano,uf,cod_regint,cod_mun,resultado), names_to = "Regra", values_to = "valores")|>
    dplyr::group_by(ano, uf) |>
    dplyr::summarise(
      Total        = dplyr::n(),
      Validada     = sum(!resultado, na.rm = TRUE),
      Suspeita     = sum(resultado, na.rm = TRUE),
      `% Validada` = round((Validada / Total) * 100, 2),
      `% Suspeita` = round((Suspeita / (Total ) )* 100, 2)
    ) |>
    dplyr::ungroup()
  return(res)
}
