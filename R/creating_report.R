#' Creating_report
#' @description
#' This function return a report with all statistical analysis and critical rules
#'
#' @param path_data Path to data
#' @param mydir Output directory
#' @param knitmydir output knit
#' @return Report
#' @export
#' @importFrom utils choose.files head

creating_report <- function(mydir = getwd(), knitmydir = getwd()){
  path_list <- c(
    #"CT_ARTIGOS"  = system.file("rmd", "report_artigos.Rmd", package = "dataedittecnology"),
    #"CT_BOLSAPOS" = system.file("rmd", "report_bolsapos.Rmd", package = "dataedittecnology"),
    #"CT_BNDES"    = system.file("rmd", "report_bndes.Rmd", package = "dataedittecnology"),
    #"CT_DOUTORES" = system.file("rmd", "report_doutores.Rmd", package = "dataedittecnology"),
    "CT_"         = system.file("rmd", "report_ct.Rmd", package = "dataedittecnology")
  )

  path_data <- choose.files()
  path_data <- split(path_data,f = path_data)

  noun <- sub(".*\\\\", "", names(path_data))
  noun <- sub("\\.[^.]+$", "", noun)
  names(path_data) <- noun
  first_obs <- dplyr::pull(as.data.frame(head(readRDS(path_data[[1]]),1)[,"codigo_indicador"]))
  #prefix <- substr(first_obs,1,nchar(first_obs))
  prefix <- substr(first_obs,1,3)


  rmarkdown::render(input = path_list[prefix],
                    output_dir = mydir,
                    knit_root_dir = knitmydir,
                    params = list(data = path_data))
}
