# a function which creates code to enforce that a variable is fully, and really fully, evaluated

.force0 <- function(prefix, n) {
  c(paste0(prefix, n, " <- force(", n , ");"),
   paste0(prefix, n, " <- do.call(force, list(", n, "));"));
}


.force <- function(prefix="  ", n, n2 = NA) {
  res <- .force0(prefix, n);
  if(is.na(n2)) {
    return(res);
  }
  return(unlist(c(res, .force0(prefix, n2), res)));
}
