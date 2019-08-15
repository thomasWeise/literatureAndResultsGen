#' @title Generate R Code for a Function which can Check a Result Data Frame
#' @description  well, exactly that
#' @param columns, the columns resulting from \code{create.columns} or
#'   \code{join.columns}
#' @param function.name the name of the function
#' @return a character array with the generated code
#' @include create_column.R
#' @export generate.result.data.frame.validate.function
generate.result.data.frame.validate.function <- function(columns,
                                                         function.name=.func.result.data.frame.validate) {
  stopifnot(is.character(function.name),
            length(function.name) == 1L,
            nchar(function.name) > 0L,
            !is.na(columns),
            is.list(columns),
            length(columns) == 3L);
  .check.columns(columns$columns, columns$conditions, columns$mergers);

  code <- c("#' @title Validate a Data Frame with Results",
            "#' @description Check whether all data in the frame \\code{x} is consistent.",
            "#' @param x the data frame",
            "#' @return either \\code{TRUE} or an error is thrown.",
            paste0(function.name, " <- function(x) {"));

  cond <- unlist(columns$conditions);
  l <- length(cond);
  if(l > 0L) {
    cond[[1L]] <- paste0("  stopifnot(", cond[[1L]]);
    cond[-1L] <- paste0("    ", cond[-1L]);
    cond[-l] <- paste0(cond[-l], ",");
    cond[[l]] <- paste0(cond[[l]], ");");
    code <- unlist(c(code, cond));
  }

  code <- unlist(c(code, "  return(TRUE);", "}"));

  p <- parse(text=paste(code, sep="\n", collapse="\n"));
  stopifnot(!is.null(p),
            is.expression(p));

  return(code);
}

#' @title Generate R Code for a Function which can Expand a Result Data Frame
#' @description  well, exactly that
#' @param columns, the columns resulting from \code{create.columns} or
#'   \code{join.columns}
#' @param function.name the name of the function
#' @param validate.function.name the name of the validation function
#' @return a character array with the generated code
#' @include create_column.R
#' @export generate.result.data.frame.expand.function
generate.result.data.frame.expand.function <- function(columns,
                                                       function.name=.func.result.data.frame.expand,
                                                       validate.function.name=.func.result.data.frame.validate) {
  stopifnot(is.character(function.name),
            length(function.name) == 1L,
            nchar(function.name) > 0L,
            is.character(validate.function.name),
            length(validate.function.name) == 1L,
            nchar(validate.function.name) > 0L,
            !is.na(columns),
            is.list(columns),
            length(columns) == 3L);
  .check.columns(columns$columns, columns$conditions, columns$mergers);

  code <- unname(unlist(c("#' @title Expand a Data Frame with Results",
            "#' @description Infer all data that can be infered from frame \\code{x} is and return the expanded frame.",
            "#' @param x the data frame",
            "#' @return the expanded data frame.",
            paste0(function.name, " <- function(x) {"),
            paste0("  while(", validate.function.name, "(x)) {"),
            "    changed <- FALSE;",
            paste0("    ", unname(unlist(columns$mergers))),
            "    if(!changed) {",
            .force("      ", "x"),
            "      return(x);",
            "    }",
            "  }",
            "}")));

  p <- parse(text=paste(code, sep="\n", collapse="\n"));
  stopifnot(!is.null(p),
            is.expression(p));

  return(code);
}
