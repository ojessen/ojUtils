#' ifelse replacement
#'
#' This function is a port of ifelse from base. Usually the behaviour should be identical.
#'
#' @param test logical vector or an object that can be compressed to a logical vector
#' @param yes numerical, character, integer or logical vector for cases where test is TRUE.
#' @param no numerical, character, integer or logical vector for cases where test is FALSE.

#' @export
#' @useDynLib ojUtils
#' @details The parameters must have the same length, or, in the case of yes or no, length must be 1.
#' @examples
#' test = c(TRUE,FALSE,NA); yes = c(1,1,1); no = c(2,2,2)
#' all(ifelseC(test, yes, no) == ifelse(test, yes, no))

ifelseC = function(test, yes, no){
  requireNamespace("Rcpp")
  if(typeof(test) == "list"){
    test = unlist(test)
  }
  if(typeof(test) != "logical"){
    test = try(as.logical(test))
    stopifnot(typeof(test)== "logical")
  }
  
  l_test = length(test)
  l_yes = length(yes)
  l_no = length(no)
  l_max = max(l_test, l_yes, l_no)
  
  if(l_yes == 1){
    yes = rep(yes,l_max)
  }
  
  if(l_no == 1){
    no = rep(no, l_max)
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

#' combn replacement
#'
#' This function is an adaptation of combn from base. currently the options are limited, and implying a value of m = 2.
#'
#' @param x is an integer vector, representing a sequence starting at 1.

#' @export
#' @useDynLib ojUtils
#' @examples
#' x = 1:4
#' all(combn(x,2)==combn_C(x))
#' 
combn_C = function(x){
  requireNamespace("Rcpp")
  internal_combn_C(x)
}
