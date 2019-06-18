#' Class for managing sequential simplex experiments
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @param k dimensionality of space being experimented

experiment = R6::R6Class("Simplex Experiment",
                         public = list(
                           k = NULL,
                           initialize = function(k = NA_integer_) {
                             self$k = k
                           }
                         ))
