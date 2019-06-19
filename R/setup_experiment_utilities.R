check_constraint_elements = function(x, treatments){
  #' Check if element of constraint expression is a treatment name
  #'
  #' @param x A character element of an expression
  #' @param treatments A vector of treatment names
  #' @return logical indicating if the expression element is valid
  #' @examples
  #' check_constraint_elements("+", c("x1", "x2"))
  #' #> TRUE
  #' check_constraint_elements("x2", c("x1", "x2"))
  #' #> TRUE
  #' check_constraint_elements("ph", c("x1", "x2"))
  #' #> FALSE
  if (is.na(as.numeric(x))){
    return(x %in% treatments)
  } else {
    return(TRUE)
  }
}
