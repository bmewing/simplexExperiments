#' Class for managing sequential simplex experiments
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export

experiment = R6::R6Class("Simplex Experiment",
                         public = list(
                           k = NULL,
                           simplexes = list(),
                           treatments = list(),
                           constraints = list(),
                           responses = list(),
                           initialize = init,
                           add_response = add_response
                         ),
                         private = list(
                           valid = valid_experiment
                         ))
