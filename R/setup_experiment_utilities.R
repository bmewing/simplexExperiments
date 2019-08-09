determine_cohypoplanarity = function(simplex){
  #' Determine if simplex is degenerate (cohypolanar)
  #'
  #' @param simplex data frame with k+1 rows and k columns
  #' @return logical indicating if simplex is cohypoplanar
  if (nrow(simplex) != ncol(simplex) + 1) stop("Incorrect dimensions")
  simplex = as.matrix(simplex)
  for (i in 1:nrow(simplex)){
    tmp = simplex[-i, ]
    mat = tmp - matrix(rep(simplex[i, ], nrow(tmp)), nrow = nrow(tmp), byrow = TRUE)
    if (det(mat) == 0) return(TRUE)
  }
  return(FALSE)
}


gen_fake_treatments = function(names){
  out = as.data.frame(matrix(1, ncol = length(names)))
  names(out) = names
  return(out)
}
