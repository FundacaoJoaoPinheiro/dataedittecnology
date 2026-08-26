#' Creating_report
#' @description
#' This function return a report with all statistical analysis and critical rules
#'
#' @param path_data Path to data
#' @param mydir Output directory
#' @return Report
#' @export
#' @importFrom utils choose.files head

creating_report <- function(mydir = getwd(),
                            data_path = NULL,
                            fator_mediana = 0.20,
                            regras = NULL){
  if(is.null(data_path)){
    path_data <- choose.files()
  }else{
    path_data <- data_path
  }
  path_data <- split(path_data,f = path_data)
  noun <- sub(".*\\\\", "", names(path_data))
  noun <- sub("\\.[^.]+$", "", noun)
  names(path_data) <- noun

  path_data <- lapply(path_data,load_data)

  #caminho para o relatorio
  path_list <- system.file("rmd", "report_ct.Rmd", package = "dataedittecnology")

  rmarkdown::render(input = path_list,
                    output_dir = mydir,
                    knit_root_dir = mydir,
                    intermediates_dir = mydir,
                    params = list(data = path_data,
                                  fator = fator_mediana,
                                  distributional_check = regras))
}
