submit_responses = function(data = NULL){
  #' Method to enable user to submit responses to experiment interactively
  #'
  #' @param data optional input to pass in responses
  #'
  #' @return invisible
  #' @details
  #' If data is manually passed in, it must be a data.frame with one column per resonse
  #' plus one column named "Vertex_ID" which must match
  if (length(self$simplexes) == 0) stop("You must generate an initial simplex first.")
  n = length(self$simplexes)
  current = self$simplexes[[n]]
  treatments = names(self$treatments)
  responses = names(self$responses)
  if (!any(is.na(current[responses]))) stop("You must generate a new simplex first.")

  if (is.null(data)){
    message("Entering interactive data entry mode...")
    for (i in 1:nrow(current)){
      for (j in responses){
        if (is.na(current[i, j])){
          message(paste0("For Vertex_ID ", current[["Vertex_ID"]][i]))
          print(current[i, c("Vertex_ID", treatments)])
          val = as.numeric(readline(paste0("Value for ", j, ": ")))
          if (is.na(val)) stop("Invalid data entry")
          check_response_range(val, self$responses, j)
          current[i, j] = val
        }
      }
    }
    self$simplexes[[n]] = current
  } else {
    if (!identical(c("Vertex_ID", responses), names(data))){
      stop("Input data must have exactly 'Vertex_ID' and each response")
    }
    if (any(vapply(data, function(x) any(is.na(x)) | any(!is.numeric(x)), FUN.VALUE = logical(1)))){
      stop("Input data must have a numeric value for each response.")
    }
    vapply(responses, function(j){check_response_range(data[[j]], self$responses, j)}, FUN.VALUE = logical(1)) #nolint
    new_rows = merge(current[c("Vertex_ID", treatments)], data, by = "Vertex_ID")
    old_rows = current[!current[["Vertex_ID"]] %in% new_rows[["Vertex_ID"]], ]
    self$simplexes[[n]] = rbind(old_rows, new_rows)
  }
  invisible()
}


check_response_range = function(val, responses, r){
  #' Check if provided response is in stated range
  #'
  #' @param val The numeric response value provided
  #' @param responses The list of responses from self
  #' @param r The name of the response being evaluated
  #'
  #' @return TRUE if it doesn't error out
  range = responses[[r]][["range"]]
  if (!all(val %between% range)){
    stop(paste0("Response value for ", r, " is not within stated boundaries (", range[1], ", ", range[2], ")"))
  }
  message("Responses recorded!")
  TRUE
}
