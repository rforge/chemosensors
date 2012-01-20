
#' @export
devClassHierarchy <- function(classes=c("Sensor"))
{ 
  cat("\n\n * Class Hierarchy\n\n")
  for(cl in classes) {
    cat("", cl, "\n")
    cat(" -", paste(subClasses(cl), collapse=", "), "\n")
  }
  cat("\n ***\n\n")    
}

#' @export
devClassSlots <- function(classes=c(subClasses("Sensor"), "Sensor"))
{
  cat("\n\n * Class Slots\n\n")
  for(cl in classes) {
    cat("", cl, "\n")
    cat(" -", paste(slotNames(new(cl)), collapse=", "), "\n")
  }
  cat("\n ***\n\n")    
}

#' @export 
devClassParam <- function(fun=c("defaultConcUnitsInt", "defaultConcUnits"))
{
  cat("\n\n * Package Parameters\n\n")
  for(f in fun) {
    cat("", f, ":",  do.call(f, list()),  "\n")
  }
  cat("\n ***\n\n")    
}

#' @export
devDefaults <- function(fun=c("defaultDataPackage"))
{
  cat("\n\n * Package Defaults\n\n")
  for(f in fun) {
    cat("", f, ":",  do.call(f, list()),  "\n")
  }
  cat("\n ***\n\n")    
}

#' @export
devReport <- function(fun=c("devClassHierarchy", "devClassSlots", "devClassParam", "devDefaults"))
{
  for(f in fun) {
    do.call(f, list())
  }
}

