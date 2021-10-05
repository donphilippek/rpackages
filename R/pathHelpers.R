
#' Splits Path to vector
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
split_path <- function(x){ 
  if (dirname(x)==x) 
    x 
else 
  c(basename(x),split_path(dirname(x)))
}


#' removes path and extension
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
getFilenameWithoutExtension <- function(x){
  sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x))
}