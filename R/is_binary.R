#' Binary check function
#'
#' This function checks if a file is binary
#' @param filepath A character; path to target file
#' @param max An integer; max number of characters in file to be checked (default 1000)
#'
#' @return A boolean; TRUE if file is binary and FALSE otherwise
#'
#' @examples is_binary()
is_binary <- function(filepath,max=1000){
  f=file(filepath,"rb",raw=TRUE)
  b=readBin(f,"int",max,size=1,signed=FALSE)
  close(f)
  return(max(b)>128)
}
