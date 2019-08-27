
#' @title Try to convert a numeric vector to an integer vector
#' @description Try to convert a numeric vector to an integer vector
#' @param x the vector
#' @param stopIfFails stop if the conversion fails
#' @param canRound is it permitted to floor the integre values?
#' @return the integer vector
#' @export try.convert.numeric.to.int
try.convert.numeric.to.int <- function(x, stopIfFails=FALSE, canFloor=FALSE) {
  stopifnot(is.logical(stopIfFails),
            isTRUE(stopIfFails) || isFALSE(stopIfFails),
            is.logical(canFloor),
            isTRUE(canFloor) || isFALSE(canFloor));

  x <- unname(unlist(x));
  stopifnot(is.vector(x),
            is.numeric(x));

  if(all(is.integer(x))) {
    return(x);
  }

  stopifnot(length(x) > 0L);
  x.is.na <- is.na(x);
  if(all(x.is.na)) {
    return(as.integer(rep.int(NA_integer_, length(x))));
  }

  if(!(all(x.is.na | ((x >= (-.Machine$integer.max)) &
                      (x <= ( .Machine$integer.max)))))) {
    if(stopIfFails) {
      stop("some values are outside of the permitted range.");
    }
    return(x);
  }

  x.2 <- as.integer(x);
  stopifnot(all(is.integer(x.2)),
            length(x.2) == length(x));
  if(!(all( (x.is.na == is.na(x.2)) &
            (is.finite(x) == is.finite(x.2))))) {
    if(stopIfFails) {
      stop("some values become na or infinite for no good reason");
    }
    return(x);
  }

  stopifnot(all(x.is.na | (x.2 <= x)),
            all(x.is.na | (x.2 >= (x - 1))));
  if(canFloor) {
    return(x.2);
  }

  if(all(x.is.na | (x == x.2))) {
    return(x.2);
  }

  if(stopIfFails) {
    stop("some values are not integers");
  }

  return(x);
}
