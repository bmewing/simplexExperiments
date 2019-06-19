add_response = function(name = paste0("y", length(self$responses) + 1),
                        range = NULL, value = function(y, hist){100-y}, weight = 1){
  #' Add a response to the experiment
  #'
  #' @param name a unique length 1 character vector giving the name of a response
  #' @param range optional length 2 numeric vector giving boundaries on possible response levels
  #' @param value function to compute the value of a particular response point
  #' @param weight relative weighting factor used to prioritize some responses over others
  #' @return invisible
  #'
  #' @details
  #' The value function must accept two positional arguments. The first is for the 'current' response
  #' value while the second is for a vector of all input response values. These two inputs can then
  #' be used however you like to return a numeric value.
  if (!is.numeric(weight) || length(weight) != 1) stop("Treatment weight must be a single numeric vector")
  if (!is.character(name) || length(name) != 1) stop("Treatment name must be a single character vector")
  if ((!is.null(range) && length(range != 2)) ||
      (!is.null(range) && !is.numeric(range))) stop("Response ranges must be exactly two numeric values (or left NULL)")
  names = purrr::modify_depth(self$responses, 1, `[[`, "name")
  if (name %in% names) stop("A response with that name already exists")
  if (!is.function(value)) stop("You must provide a function to compute the value of a given response")
  l = length(self$responses) + 1
  self$responses[[l]] = list(name = name, range = range, value = value, weight = weight)
  invisible()
}


drop_response = function(name){
  #' Drop a response from the experiment
  #'
  #' @param name a unique length 1 character vector giving the name of the response to drop
  #' @return invisible
  names = purrr::modify_depth(self$responses, 1, `[[`, "name")
  if(!name %in% names) stop("Response does not exist.")
  self$responses[[which(names == name)]] = NULL
  invisible()
}


add_treatment = function(name = paste0("x", length(self$treatments) + 1), boundaries = NULL){
  #' Add a treatment to the experiment
  #'
  #' @param name a unique length 1 character vector giving the name of a treatment
  #' @param boundaries optional length 2 numeric vector giving boundaries on possible treatment levels
  #' @return invisible
  if (!is.character(name) || length(name) != 1) stop("Treatment name must be a single character vector")
  if ((!is.null(boundaries) && length(boundaries != 2)) ||
      (!is.null(boundaries) && !is.numeric(boundaries))) stop("Boundaries must be exactly two numeric values (or left NULL)")
  l = length(self$treatments) + 1
  if (l > self$k) stop("You have already added as many treatments as specified")
  names = purrr::modify_depth(self$treatments, 1, `[[`, "name")
  if (name %in% names) stop("A treatment with that name already exists")
  self$treatments[[l]] = list(name = name, boundaries = boundaries)
  invisible()
}


drop_treatment = function(name){
  #' Drop a treatment from the experiment
  #'
  #' @param name a unique length 1 character vector giving the name of the treatment to drop
  #' @return invisible
  names = purrr::modify_depth(self$treatments, 1, `[[`, "name")
  if(!name %in% names) stop("Treatment does not exist.")
  self$treatments[[which(names == name)]] = NULL
  invisible()
}


add_constraint = function(constraint){
  #' Add a constraint to the experimental factors
  #'
  #' @param constraint a comparitive formula referencing treatment names
  #' @return invisible
  #'
  #' @examples
  #' experiment$add_constraint(x1 + x2 <= 14)
  const = substitute(constraint)
  const_string = deparse(const)
  comparison_operators = c(" < ", " > ", " <= ", " >= ", " == ")
  comparison_matches = vapply(comparison_operators, grepl, FUN.VALUE = logical(1), x = const_string)
  if (sum(comparison_matches) != 1) stop("Constraint must include a comparison operator")
  const_string = grep("[a-zA-Z0-9]", strsplit(const_string, " ")[[1]], value = TRUE)
  names = purrr::modify_depth(self$treatments, 1, `[[`, "name")
  valid = vapply(names, check_constraint_elements, FUN.VALUE = logical(1), treatments = names)
  if (all(valid)){
    l = length(self$constraints) + 1
    self$constraints[[l]] = const
  } else {
    stop("Invalid constraint provided. Ensure you're passing in a valid expression")
  }
  invisible()
}


drop_constraint = function(id){
  #' Drop a constraint from the experiment
  #'
  #' @param id the id of the constraint to drop
  #' @return invisible
  if (id > length(self$constraints)) stop("Invalid constraint ID")
  self$constraints[[id]] = NULL
  invisible()
}


check_constraint_elements = function(x, treatments){
  if (is.na(as.numeric(x))){
    return(x %in% treatments)
  } else {
    return(TRUE)
  }
}
