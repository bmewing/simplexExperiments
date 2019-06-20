init = function(k = NA_integer_) {
  #' Initialize the R6 Class
  #'
  #' @param k dimensionality of space being experimented
  #'
  #' @return invisible
  #' @examples
  #' experiment$new(k = 3)
  if (k < 0) stop("You must specify a whole number of dimensions")
  self$k = k
  invisible()
}


valid_experiment = function(){
  #' Check if experiment is valid
  #'
  #' @return invisible
  #' @details PRIVATE
  if (length(self$treatments) != self$k) stop(paste0("You have not provided ", self$k, " treatments.")) #nolint
  if (length(self$responses) < 1) stop("You must include at least one response.") #nolint
  invisible()
}


valid_simplex = function(simplex, names, boundaries, constraints){
  #' Check if simplex is valid
  #'
  #' @param simplex data.frame of simplex to be checked
  #' @param names vector of treatment names
  #' @param boundaries list of treatment boundaries
  #' @param constraints list of treatment constraints
  #'
  #' @return logical indicating if simplex is valid
  #' @details PRIVATE

  within_boundaries(simplex, names, boundaries) && satisfied_constraints(simplex, constraints)
}


`%between%` = function(x, r){
  #' Check if values are between two other values
  #'
  #' @param x the value(s) to be checked (left hand side)
  #' @param r range to check if is between
  #' @return logical indicating if x is between min/max of r
  #' @details
  #' This is an implementation specific to this package as it returns TRUE if r is null.
  #' Not exported as it's not intended for broader use
  if (is.null(r)) return(rep(TRUE, length(x)))
  x >= min(r) & x <= max(r)
}


within_boundaries = function(simplex, names, boundaries){
  #' Check if all simplex values are within boundaries
  #'
  #' @param simplex data.frame containing the simplex to be checked
  #' @param names vector of treatment names
  #' @param boundaries list of treatment boundaries
  #'
  #' @return logical indicating if all values are within boundaries
  all(purrr::map_lgl(seq_along(names), function(i){
    all(simplex[[names[[i]]]] %between% boundaries[[i]])
  }))
}


satisfied_constraints = function(simplex, constraints){
  #' Check if all experimental constraints are met by the simplex
  #'
  #' @param simplex data.frame containing the simplex to be checked
  #' @param constraints list of treatment constraints
  #'
  #' @return logical indicating if all constraints are met
  if (length(constraints) == 0) return(TRUE)
  purrr::map_lgl(constraints, function(expr) all(with(simplex, expr = eval(expr))))
}


append_responses = function(simplex, responses){
  #' Append experiment responses to simplex data frame
  #'
  #' @param simplex data.frame of simplex
  #' @param responses vector of response names
  #'
  #' @return data.frame of simplex with responses appended

  for (r in responses){
    simplex[[r]] = NA
  }
  return(simplex)
}
