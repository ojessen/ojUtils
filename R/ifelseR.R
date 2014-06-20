#' ifelse replacement
#'
#' This function is a port of ifelse from base. Usually the behaviour should be identical.
#'
#' @param test logical vector or an object that can be compressed to a logical vector
#' @param yes numerical, character, integer or logical vector for cases where test is TRUE.
#' @param no numerical, character, integer or logical vector for cases where test is FALSE.

#' @export
#' @examples
#' test = c(T,F,NA); yes = c(1,1,1); no = c(2,2,2)
#' all(ifelseC(test, yes, no) == ifelse(test, yes, no))

ifelseC = function(test, yes, no){
  require(Rcpp)
  if(typeof(test) == "list"){
    test = unlist(test)
  }
  if(typeof(test) != "logical"){
    test = try(as.logical(test))
    stopifnot(typeof(test)== "logical")
  }
  type_yes = typeof(yes)
  type_no = typeof(no)
  stopifnot(type_yes == type_no)
  if(type_yes == "double"){
    out = try(ifelseCNum(test, yes, no))
  } else if(type_yes == "character"){
    out = try(ifelseCChar(test, yes, no))
  } else if(type_yes == "integer"){
    out = try(ifelseCInt(test, yes, no))
  } else if(type_yes == "logical"){
    out = try(ifelseCLogic(test, yes, no))
  } else{
    stop(paste("type", type_yes, "not supported."))
  }
  out
}
