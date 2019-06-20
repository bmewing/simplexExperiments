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
                           add_response = add_response,
                           drop_response = drop_response,
                           add_treatment = add_treatment,
                           drop_treatment = drop_treatment,
                           add_constraint = add_constraint,
                           drop_constraint = drop_constraint,
                           generate_initial_simplex = generate_initial_simplex
                         ),
                         private = list(
                           next_vertex_id = 1,
                           valid_experiment = valid_experiment,
                           valid_simplex = valid_simplex
                         ))
