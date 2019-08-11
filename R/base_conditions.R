.int.bound <- .Machine$integer.max - 1L;
.max.runs <- as.integer(min(.int.bound, 10000000L));

# .has.columns.and <- function(...) {
#   v <- c(...);
#   stopifnot(is.character(v),
#             length(v) > 0L);
#   if(length(v) > 1L) {
#     paste0("all(c(\"", paste(v, sep="\", \"", collapse="\", \""), "\") %in% colnames(x)) && ")
#   } else {
#     paste0("(\"", v[[1L]], "\" %in% colnames(x)) && ");
#   }
# }
#
# .hasnt.columns.or <- function(...) {
#   v <- c(...);
#   stopifnot(is.character(v),
#             length(v) > 0L);
#   if(length(v) > 1L) {
#     paste0("(!all(c(\"", paste(v, sep="\", \"", collapse="\", \""), "\") %in% colnames(x))) || ")
#   } else {
#     paste0("(!(\"", v[[1L]], "\" %in% colnames(x))) || ");
#   }
# }

.numeric.conditions <- function(n, is.col.integer=TRUE) {
  stopifnot(is.character(n),
            length(n) == 1L,
            nchar(n) > 0L,
            is.logical(is.col.integer),
            length(is.col.integer) == 1L,
            !is.na(is.col.integer));
  a <- c(paste0("all(is.na(x$", n, ") | is.finite(x$", n, "))"),
         paste0("is.numeric(x$", n, ")"));
  if(is.col.integer) {
    return(c(a, paste0("is.integer(x$", n, ")")));
  } else {
    return(a);
  }
}

.bound.conditions <- function(n, is.col.integer=TRUE,
                              lower.bound=0L, upper.bound=.int.bound) {
  stopifnot(is.character(n),
            length(n) == 1L,
            nchar(n) > 0L,
            is.logical(is.col.integer),
            length(is.col.integer) == 1L,
            !is.na(is.col.integer),
            length(lower.bound) == 1L,
            length(upper.bound) == 1L);

  cond <- character(0);
# add lower bound condition
  if(!is.na(lower.bound)) {
    if(is.character(lower.bound)) {
      stopifnot(nchar(lower.bound) > 0L);
      cond <- c(cond, paste0("all(is.na(x$", n, ") | is.na(", lower.bound, ") | (x$", n, " >= ", lower.bound, "))"));
    } else {
      stopifnot(is.finite(lower.bound));
      if(is.col.integer) {
        stopifnot(is.integer(lower.bound));
        cond <- c(cond, paste0("all(is.na(x$", n, ") | (x$", n, " >= ", lower.bound, "L))"));
      } else {
        stopifnot(is.numeric(lower.bound));
        cond <- c(cond, paste0("all(is.na(x$", n, ") | (x$", n, " >= ", lower.bound, "))"));
      }
    }
  }

# add upper bound condition
  if(!is.na(upper.bound)) {
    if(!is.na(lower.bound)) {
      if(!(is.character(lower.bound) | is.character(upper.bound))) {
        stopifnot(upper.bound >= lower.bound);
      }
    }
    if(is.character(upper.bound)) {
      stopifnot(nchar(upper.bound) > 0L);
      cond <- c(cond, paste0("all(is.na(x$", n, ") | is.na(", upper.bound, ") | (x$", n, " <= ", upper.bound, "))"));
    } else {
      stopifnot(is.finite(upper.bound));
      if(is.col.integer) {
        stopifnot(is.integer(upper.bound));
        cond <- c(cond, paste0("all(is.na(x$", n, ") | (x$", n, " <= ", upper.bound, "L))"));
      } else {
        stopifnot(is.numeric(upper.bound));
        cond <- c(cond, paste0("all(is.na(x$", n, ") | (x$", n, " <= ", upper.bound, "))"));
      }
    }
  }

  return(cond);
}
