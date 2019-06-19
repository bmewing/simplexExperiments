init = function(k = NA_integer_) {
  #' Initialize the R6 Class
  #'
  #' @param k dimensionality of space being experimented
  #' @examples
  #' experiment$new()
  if (k < 0) stop("You must specify a whole number of dimensions")
  self$k = k
}
