`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

drop_nulls <- function(x){
  x[!sapply(x, is.null)]
}

"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}

"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

# Credit: Dean Attali 
# source: https://github.com/daattali/advanced-shiny/tree/master/upload-file-names
fixUploadedFilesNames <- function(x) {
  if (is.null(x)) {
    return()
  }
  
  oldNames = x$datapath
  newNames = file.path(dirname(x$datapath),
                       x$name)
  file.rename(from = oldNames, to = newNames)
  x$datapath <- newNames
  x
}
