add_response = function(name = paste0("y", length(self$responses) + 1),
                        range = NULL, value = NULL, weight = 3){ #nolint
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
  #' be used however you like to return a numeric value. e.g. `function(y, hist){100 - y}`
  if (!is.numeric(weight) || length(weight) != 1) stop("Treatment weight must be a single numeric vector")
  if (!is.character(name) || length(name) != 1) stop("Treatment name must be a single character vector")
  if ((!is.null(range) && length(range) != 2) || (!is.null(range) && !is.numeric(range))){ #nolint
    stop("Response ranges must be exactly two numeric values (or left NULL)")
  }
  if (name %in% names(self$responses)) stop("A response with that name already exists")
  if (!is.function(value)) stop("You must provide a function to compute the value of a given response")
  self$responses[[name]] = list(range = range, value = value, weight = weight)
  invisible()
}


drop_response = function(name){
  #' Drop a response from the experiment
  #'
  #' @param name a unique length 1 character vector giving the name of the response to drop
  #' @return invisible
  if (length(name) != 1) stop("Only one response can be dropped at a time")
  if (!name %in% names(self$responses)) stop("Response does not exist.")
  self$responses[[name]] = NULL
  invisible()
}


add_treatment = function(name = paste0("x", length(self$treatments) + 1), boundaries = NULL){
  #' Add a treatment to the experiment
  #'
  #' @param name a unique length 1 character vector giving the name of a treatment
  #' @param boundaries optional length 2 numeric vector giving boundaries on possible treatment levels
  #' @return invisible
  l = length(self$treatments) + 1
  if (l > self$k) stop("You have already added as many treatments as specified")

  if (!is.character(name) || length(name) != 1) stop("Treatment name must be a single character vector")
  if ( (!is.null(boundaries) && length(boundaries) != 2) ||
      (!is.null(boundaries) && !is.numeric(boundaries))) {
    stop("Boundaries must be exactly two numeric values (or left NULL)")
  }

  names = names(self$treatments)
  if (name %in% names) stop("A treatment with that name already exists")
  self$treatments[[name]] = list(boundaries = boundaries)
  invisible()
}


drop_treatment = function(name){
  #' Drop a treatment from the experiment
  #'
  #' @param name a unique length 1 character vector giving the name of the treatment to drop
  #' @return invisible
  if (length(name) != 1) stop("Only one treatment can be dropped at a time")
  if (!name %in% names(self$treatments)) stop("Treatment does not exist.")
  self$treatments[[name]] = NULL
  invisible()
}


add_constraint = function(constraint){
  #' Add a constraint to the experimental factors
  #'
  #' @param constraint a comparitive formula referencing treatment names
  #' @return invisible
  #'
  #' @details
  #' Call is made using expressions, e.g. `experiment$add_constraint(x1 + x2 <= 14)`
  const = substitute(constraint)
  if (class(const) != "call") stop("Constraints must be passed as expressions.")
  const_string = deparse(const)
  comparison_operators = c(" < ", " > ", " <= ", " >= ", " == ")
  comparison_matches = vapply(comparison_operators, grepl, FUN.VALUE = logical(1), x = const_string)
  if (sum(comparison_matches) != 1) stop("Constraint must include a comparison operator")

  tmp = gen_fake_treatments(names(self$treatments))
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

  private$valid_experiment() #nolint

  simplex_to_write = NULL

  if (length(self$simplexes) != 0){ #nolint
    stop("This experiment already has simplexes, you cannot generate a new initial one.")
  }
  if (length(method) != 1) stop("You can only specify one method for initial generation")
  if (is.null(data)) stop("You must provide information through the 'data' parameter to generate the initial simplex")

  treatments = names(self$treatments)
  responses = names(self$responses)
  k = self$k
  boundaries = lapply(self$treatments, function(x){x[["boundaries"]]}) #nolint

  if (method == "manual"){
    if (!all(treatments %in% names(data))) stop("Not all treatments are included in the initial simplex")
    if (nrow(data) != k + 1) stop(paste0("You must provide ", k + 1, " initial vertices")) #nolint
    data = data[unlist(treatments)]
    if (any(is.na(data)) || any(is.null(data))) stop("Vertices cannot contain missing coordinates.")
    if (determine_cohypoplanarity(data)){
      stop("Provided simplex is cohypoplanar (e.g. it is degenerate in at least one dimension")
    }
    if (private$valid_simplex(data, treatments, boundaries, self$constraints)){
      simplex_to_write = data
    } else {
      stop("Provided simplex is not within boundaries or does not satisfy constraints")
    }
  } else if (method %in% c("corner", "tilted")){
    if (nrow(data) != k) stop("You must provide one row per treatment to generate a corner or tilted design.")
    if (ncol(data) != 3) stop("You must provide three columns, treatment names, starting values and step sizes.")

    if (any(is.na(as.character(data[[1]])))) stop("The first column provided must contain treatment names.")
    if (!all(treatments %in% data[[1]])) stop("Not all treatments are represented in the data provided.")
    if (!is.numeric(data[[2]])) stop("The second column provided must contain starting values.")
    if (!is.numeric(data[[3]])) stop("The third column provided must contain step sizes.")
    names(data) = c("tmt", "start", "step")
    data[["p"]] = if (method == "tilted") data[["step"]] * (sqrt(k + 1) + k - 1) / (k * sqrt(2)) else data[["step"]]
    data[["q"]] = if (method == "tilted") data[["step"]] * (sqrt(k + 1) - 1) / (k * sqrt(2)) else 0

    initial = as.data.frame(matrix(rep(data[["start"]], self$k + 1), nrow = self$k + 1, byrow = TRUE))
    names(initial) = data[["tmt"]]

    if (!private$valid_simplex(initial, treatments, boundaries, self$constraints)){
      stop("Starting values provided do not satisfy boundaries or constraints.")
    }

    for (i in 1:nrow(data)){
      initial[i, i] = initial[i, i] + data[["p"]][i]
      initial[i, -i] = initial[i, -i] + data[["q"]][-i]
    }

    if (private$valid_simplex(initial, treatments, boundaries, self$constraints)){
      simplex_to_write = initial
    } else {
      stop("Generated simplex is not within boundaries or does not satisfy constraints.
       Consider changing step sizes, starting values or providing a manual design.")
    }
  } else {
    stop("Method must be one of 'manual, 'tilted', or 'corner'.")
  }

  if (!is.null(simplex_to_write)){
    simplex_to_write[["Vertex_ID"]] = 1:(k + 1)

    private$next_vertex_id = k + 2

    self$simplexes[[1]] = private$append_responses(simplex_to_write, responses)

    message("Initial Simplex:")
    print(self$simplexes[[1]][c("Vertex_ID", treatments)], row.names = FALSE)
  } else {
    stop("Uh oh, something went wrong!")
  }
  invisible()
}
