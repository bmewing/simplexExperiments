add_response = function(name = paste0("y", length(self$responses) + 1),
                        range = NULL, value = function(y, hist){100 - y}, weight = 3){ #nolint
  #' Add a response to the experiment
  #'
  #' @param name a unique length 1 character vector giving the name of a response
  #' @param range optional length 2 numeric vector giving boundaries on possible response levels
  #' @param value function to compute the value of a particular response point,
  #' the algorithm will maximize the response of this function
  #' @param weight relative weighting factor used to prioritize some responses over others
  #' @return invisible
  #'
  #' @details
  #' The value function must accept two positional arguments. The first is for the 'current' response
  #' value while the second is for a vector of all input response values. These two inputs can then
  #' be used however you like to return a numeric value.
  if (!is.numeric(weight) || length(weight) != 1) stop("Treatment weight must be a single numeric vector")
  if (!is.character(name) || length(name) != 1) stop("Treatment name must be a single character vector")
  if ((!is.null(range) && length(range) != 2) || (!is.null(range) && !is.numeric(range))){ #nolint
    stop("Response ranges must be exactly two numeric values (or left NULL)")
  }
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
  if (!name %in% names) stop("Response does not exist.")
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
  if ( (!is.null(boundaries) && length(boundaries) != 2) ||
      (!is.null(boundaries) && !is.numeric(boundaries))) {
    stop("Boundaries must be exactly two numeric values (or left NULL)")
  }
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
  if (!name %in% names) stop("Treatment does not exist.")
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

  names = purrr::modify_depth(self$treatments, 1, `[[`, "name")
  tmp = gen_fake_treatments(names)
  tryCatch({
    with(tmp, expr = eval(const))
    l = length(self$constraints) + 1
    self$constraints[[l]] = const
  }, error = function(e){stop("Invalid constraint provided. Ensure you're passing in a valid expression")}) #nolint

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


generate_initial_simplex = function(method = "manual", data = NULL){
  #' Generate initial simplex
  #'
  #' @param method Single character value determining the method of initial simplex calculation
  #' @param data Data Frame with relevant information, see details
  #' @return invisible
  #'
  #' @details
  #' The parameter 'method' can take three values, "manual", "corner" or "tilted". Each coresponds to a different method
  #' for determining the initial simplex.
  #'
  #' *manual*
  #' When the initial simplex is manually determined, the parameter 'data' must contain a data frame with one column
  #' per treatment (sharing the same name as the treatment) and the k+1 initial vertices.
  #'
  #' *corner* and *tilted*
  #' These two methods for automatically generating a starting simplex require that the parameter 'data' contain a 3
  #' column data frame with 1 row per treatment. The first column contains treatment names, the second contains the
  #' starting coordinate for the treatment and the third contains the step size for the treatment.
  #'
  #' The initial simplex has as the first vertex the combination of all starting coordinates. Each sequential vertex is
  #' produced by adding some amount of the step size to one or more of the other treatments.

  private$valid() #nolint

  if (length(self$simplexes) != 0){ #nolint
    stop("This experiment already has simplexes, you cannot generate a new initial one.")
  }
  if (length(method) != 1) stop("You can only specify one method for initial generation")
  if (is.null(data)) stop("You must provide information through the 'data' parameter to generate the initial simplex")

  names = purrr::modify_depth(self$treatments, 1, `[[`, "name") #nolint

  if (method == "manual"){
    if (!all(names %in% names(data))) stop("Not all treatments are included in the initial simplex")
    if (nrow(data) != self$k + 1) stop(paste0("You must provide ", self$k + 1, " initial vertices")) #nolint
    data = data[names]
    if (any(is.na(data)) || any(is.null(data))) stop("Vertices cannot contain missing coordinates.")

    # TODO: check cohypoplanarity (page 164)
  } else if (method %in% c("corner", "tilted")){
    # TODO: collect step size and starting coordinate for each factor (page 169)
  }
  invisible()
}
