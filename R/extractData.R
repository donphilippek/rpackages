#' extracts complete zip and returns a filtered list of the content
#'
#' @param zipFileName 
#' @param workingfolder 
#' @param NameFilter 
#'
#' @return
#' @export
#'
#' @examples
extractData = function(zipFileName,workingfolder,NameFilter=".*"){
  currentWorkingfolder = paste(workingfolder,getFilenameWithoutExtension(zipFileName),sep="/")
  dir.create(currentWorkingfolder,showWarnings=FALSE)
  resultDataFile = unzip(zipFileName,list=T) %>% dplyr::filter(grepl(NameFilter,Name)) %>% select(Name) %>% as_vector()
  unzip(zipFileName,files=resultDataFile,exdir=currentWorkingfolder)
  fileList = paste(currentWorkingfolder,resultDataFile,sep="/")
  return(fileList)
}