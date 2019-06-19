init = function(k = NA_integer_) {
  #' Initialize the R6 Class
  #'
  #' @param k dimensionality of space being experimented
  #' @examples
  #' experiment$new()
  if (k < 0) stop("You must specify a whole number of dimensions")
  self$k = k
}


valid_experiment = function(){
  #' Check if experiment is valid
  #'
  #' @return invisible
  #' @details PRIVATE
  if (length(self$treatments) != self$k) stop(paste0("You do not have ", self$k, " treatments.")) #nolint
  if (length(self$responses) < 1) stop("You must include at least one response.") #nolint
  invisible()
}
