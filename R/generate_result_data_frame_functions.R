#' @title Generate R Code for a Function which can Check a Result Data Frame
#' @description  well, exactly that
#' @param columns, the columns resulting from \code{create.columns} or
#'   \code{join.columns}
#' @param function.name the name of the function
#' @return a character array with the generated code
#' @include create_column.R
#' @include names.R
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
            "#' @param instances the instance data frame",
            "#' @return either \\code{TRUE} or an error is thrown.",
            paste0(function.name, " <- function(x, instances) {",
            "  old.options <- options(warn=2);",
            "  stopifnot(is.data.frame(x),",
            "            nrow(x) > 0L,",
            "            ncol(x) > 0L,",
            "            is.data.frame(instances),",
            "            nrow(instances) > 0L,",
            "            ncol(instances) > 0L);"));

  cond <- unlist(columns$conditions);

  names <- vapply(columns$columns, function(col) col$title, NA_character_);
  stopifnot(length(names) > 0L,
            all(nchar(names) > 0L),
            sum(names == .col.inst.id) == 1L);

  check.cols <- c(paste0(.col.best.f, ".", .col.min.name),
                  paste0(.col.best.f, ".", .col.mean.name),
                  paste0(.col.best.f, ".", .col.med.name),
                  paste0(.col.best.f, ".", .col.mode.name),
                  paste0(.col.best.f, ".", .col.max.name));

  cond <- unname(unlist(c(cond,
            paste0("all(c(\"", .col.inst.id, "\", \"",
                    .col.inst.opt.bound.lower, "\") %in% colnames(instances))"),
            paste0("\"", .col.inst.id, "\" %in% colnames(x)"),
            paste0("all(vapply(x$", .col.inst.id, ", function(cn) { sum(instances$",
                   .col.inst.id, " == cn) == 1L }, FALSE))"),
            unlist(unname(lapply(check.cols,
             function(col) {
               if(!(col %in% names)) {
                 return(character(0));
               }
               return(c(paste0("\"", col, "\" %in% colnames(x)"),
                       paste0("all(vapply(seq_len(nrow(x)), function(i) { is.na(x$",
                             col, "[[i]]) || (x$", col, "[[i]] >= instances$",
                             .col.inst.opt.bound.lower, "[instances$",
                             .col.inst.id, " == x$", .col.inst.id, "[[i]]]) }, FALSE))")));
             })))
            )));

  l <- length(cond);
  if(l > 0L) {
    cond[[1L]] <- paste0("  stopifnot(", cond[[1L]]);
    cond[-1L] <- paste0("    ", cond[-1L]);
    cond[-l] <- paste0(cond[-l], ",");
    cond[[l]] <- paste0(cond[[l]], ");");
    code <- unlist(c(code, cond));
  }

  code <- unlist(c(code,
                   "  options(old.options);",
                   "  return(TRUE);",
                   "}"));

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
#' @include names.R
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
            "#' @param instances the instance data frame",
            "#' @return the expanded data frame.",
            paste0(function.name, " <- function(x, instances) {"),
            "  old.options <- options(warn=2);",
            "  stopifnot(is.data.frame(x),",
            "            nrow(x) > 0L,",
            "            ncol(x) > 0L,",
            "            is.data.frame(instances),",
            "            nrow(instances) > 0L,",
            "            ncol(instances) > 0L);",
            paste0("  while(", validate.function.name, "(x, instances)) {"),
            "  stopifnot(is.data.frame(x),",
            "            nrow(x) > 0L,",
            "            ncol(x) > 0L,",
            "            is.data.frame(instances),",
            "            nrow(instances) > 0L,",
            "            ncol(instances) > 0L);",
            "    changed <- FALSE;",
            paste0("    ", unname(unlist(columns$mergers))),
            "    if(!changed) {",
            .force("      ", "x"),
            "      options(old.options);",
            "      return(x);",
            "    }",
            "  }",
            "}")));

  p <- parse(text=paste(code, sep="\n", collapse="\n"));
  stopifnot(!is.null(p),
            is.expression(p));

  return(code);
}

#' @title Generate a Function that can Load a Result Data Frame from a CSV File
#' @description Generate a loader function from a description of meta and result
#'   columns.
#' @param meta.columns the meta-data columns. Their value can either be
#'   specified as parameter of the generated function or inside the result csv
#'   file.
#' @param result.columns the values of these columns can only be specified in
#'   the csv file
#' @param function.name the function name to use for the generated function
#' @param validate.function.name the name of the function generated by
#'   \code{generate.result.data.frame.validate.function}
#' @param expand.function.name the name of the function generated by
#'   \code{generate.result.data.frame.expand.function}
#' @return a function for loading a data frame from a csv file
#' @export generate.result.data.frame.load.function
#' @include names.R
#' @include create_column.R
generate.result.data.frame.load.function <- function(meta.columns,
                                            result.columns,
                                            function.name=.func.result.data.frame.load,
                                            validate.function.name=.func.result.data.frame.validate,
                                            expand.function.name=.func.result.data.frame.expand) {

  stopifnot(is.character(function.name),
            length(function.name) == 1L,
            nchar(function.name) > 0L,
            is.character(validate.function.name),
            length(validate.function.name) == 1L,
            nchar(validate.function.name) > 0L,
            is.character(expand.function.name),
            length(expand.function.name) == 1L,
            nchar(expand.function.name) > 0L,
            !is.na(meta.columns),
            is.list(meta.columns),
            length(meta.columns) == 3L,
            !is.na(result.columns),
            is.list(result.columns),
            length(result.columns) == 3L);

  .check.columns(meta.columns$columns, meta.columns$conditions, meta.columns$mergers);
  .check.columns(result.columns$columns, result.columns$conditions, result.columns$mergers);
  all.columns <- join.columns(meta.columns, result.columns);

  .get.na <- function(xx) {
    if(xx$type %in% c("character", "integer", "real")) {
      return(paste0("NA_", xx$type, "_"));
    } else {
      if(xx$type == "double") {
        return("NA_real_");
      } else {
        if(xx$type == "logical") {
          return("NA");
        } else {
          stop(paste0("unknown type ", xx$type));
        }
      }
    }
  }

  code <-c("#' @title Load a Result Data Frame from a CSV-File",
           "#' @description Load a data frame from a CSV-File and fully expand it.",
           "#' @param file the path to the file to load",
           "#' @param bibliography the bibliography data frame, used to check/infere the references and reference years",
           "#' @param instances the instance data frame, used to check/infere the instances and instance lower bounds",
           vapply(meta.columns$columns,
                  function(cc) {
                    paste0("#' @param ", cc$title, " ", cc$description)
                  }, ""),
           "#' @result the fully expanded data frame",
           paste(unlist(c(function.name,
                 " <- function(file=NA_character_, bibliography=NULL, instances=NULL",
                 vapply(meta.columns$columns,
                        function(cc) paste0(", ", cc$title, " = ", .get.na(cc)),
                        ""),
                 ") {")), sep="", collapse=""),
           "  old.options <- options(warn=2);",
           "  stopifnot(!is.na(file),",
           "            is.character(file),",
           "            nchar(file) > 0L,",
           "            !is.null(bibliography),",
           "            is.data.frame(bibliography),",
           "            nrow(bibliography) > 0L,",
           "            !is.null(instances),",
           "            is.data.frame(instances),",
           "            nrow(instances) > 0L);",
           "  file <- normalizePath(file, mustWork = TRUE);",
           "  stopifnot(file.exists(file),",
           "            file.size(file) > 0L);",
           "  if(exists(\"logger\")) { logger(\"Now loading file '\", file, \"'.\"); }",
           "  frame <- read.csv(file=file, check.names = FALSE, stringsAsFactors = FALSE);",
           "  stopifnot(is.data.frame(frame), ",
           "            nrow(frame) > 0L,",
           "            ncol(frame) > 0L);",
           "  if(exists(\"logger\")) { logger(\"Done loading file '\", file, \"', got columns '\", paste(colnames(frame), sep=\"', '\", collapse=\"', '\"), \"'.\"); }",
           .force("  ", "frame"),
           paste0("  stopifnot(\"", .col.inst.id, "\" %in% colnames(frame));"),
           paste0("  remove.rows <- startsWith(trimws(frame$", .col.inst.id, "), \"#\");"),
           "  if(any(remove.rows)) {",
           "     if(exists(\"logger\")) { logger(\"Found \", sum(remove.rows), \", rows commented out, removing them.\"); }",
           "     frame <- frame[!remove.rows, ];",
           .force(prefix="    ", "frame"),
           "  }",
           "  stopifnot(nrow(frame) > 0L);",
           paste(unlist(c("  permitted <- c(\"",
                          all.columns$columns[[1L]]$title,
                          "\"",
                          vapply(all.columns$columns[-1L],
                                 function(cc) paste0(", \"", cc$title, "\""), ""),
                          ");")), sep="", collapse=""),
           paste(unlist(c("  needed <- c(\"",
                          result.columns$columns[[1L]]$title,
                          "\"",
                          vapply(result.columns$columns[-1L],
                                 function(cc) paste0(", \"", cc$title, "\""), ""),
                          ");")), sep="", collapse=""),
           "  hasNeeded <- FALSE;",
           "  for(col in colnames(frame)) {",
           "    stopifnot(col %in% permitted);",
           "    if(col %in% needed) { hasNeeded <- TRUE; }",
           "  }",
           "  stopifnot(hasNeeded);",
           "  rows <- nrow(frame);",
           "  stopifnot(rows > 0L);",
           unlist(vapply(meta.columns$columns,
                         function(cc) paste0("if(is.null(", cc$title, ")) { ", cc$title, " <- as.", cc$type, "(", .get.na(cc), "); }"),
                         "")),
           paste(unlist(c("  x <- data.frame(stringsAsFactors = FALSE, check.names=FALSE",
           unlist(vapply(meta.columns$columns,
                         function(cc) paste0(", ", cc$title, " = as.", cc$type, "(rep_len(", cc$title, ", rows))"),
                         "")),
           unlist(vapply(result.columns$columns,
                         function(cc) paste0(", ", cc$title, " = as.", cc$type, "(rep_len(", .get.na(cc), ", rows))"),
                         "")), ");")), sep="", collapse=""),
           "  hasNeeded <- FALSE;",
           "  for(col in colnames(frame)) {",
           "    cc <- unname(unlist(frame[col]));",
           "    stopifnot(any(!is.na(cc)), ",
           "              col %in% permitted);",
           "    if(col %in% needed) { hasNeeded <- TRUE; }",
           paste0("    x[col] <- cc;"),
           .force("    ", "x[col]", "x"),
           "  }",
           "  stopifnot(hasNeeded);",
           .force("    ", "x"),
           paste0("instance.ids <- as.character(unname(unlist(instances$", .col.inst.id, ")));"),
           "  stopifnot(!any(is.na(instance.ids)),",
           "            length(instance.ids) == nrow(instances),",
           "            is.character(instance.ids),",
           "            all(nchar(instance.ids) > 0L));",
           paste0("  instance.search <- as.character(unname(unlist(x$", .col.inst.id, ")));"),
           "  stopifnot(!any(is.na(instance.search)),",
           "            length(instance.search) == nrow(x),",
           "            is.character(instance.search),",
           "            all(nchar(instance.search) > 0L));",
           "  instance.rows <- vapply(instance.search, function(ii) {",
           "    found <- instance.ids == ii;",
           "    stopifnot(!any(is.na(found)),",
           "              sum(found) == 1L);",
           "    found <- which(found);",
           "    stopifnot(length(found) == 1L,",
           "              is.integer(found),",
           "              is.finite(found[[1L]]));",
           "    return(found[[1L]]);",
           "   }, NA_integer_);",
           "  stopifnot(all(is.finite(instance.rows)),",
           "            is.integer(instance.rows),",
           "            !any(is.na(instance.rows)),",
           "            all(instance.rows > 0L),",
           "            all(instance.rows <= nrow(instances)),",
           paste0("            all(as.character(x$", .col.inst.id, ") == as.character(instances$", .col.inst.id, "[instance.rows])),"),
           paste0("            all(is.na(x$", .col.inst.opt.bound.lower, ") | ",
                  "                      (x$", .col.inst.opt.bound.lower, " == instances$", .col.inst.opt.bound.lower, "[instance.rows])));"),
           paste0("  x$", .col.inst.opt.bound.lower, " <- instances$", .col.inst.opt.bound.lower, "[instance.rows];"),
           .force("    ", paste0("x$", .col.inst.opt.bound.lower), "x"),
           paste0("  stopifnot(all(is.finite(x$", .col.inst.opt.bound.lower, ")));"),
           paste0("bibliography.ids <- as.character(unname(unlist(bibliography$", .col.ref.id, ")));"),
           "  stopifnot(!any(is.na(bibliography.ids)),",
           "            length(bibliography.ids) == nrow(bibliography),",
           "            is.character(bibliography.ids),",
           "            all(nchar(bibliography.ids) > 0L));",
           paste0("  bibliography.search <- as.character(unname(unlist(x$", .col.ref.id, ")));"),
           "  stopifnot(!any(is.na(bibliography.search)),",
           "            length(bibliography.search) == nrow(x),",
           "            is.character(bibliography.search),",
           "            all(nchar(bibliography.search) > 0L));",
           "  bibliography.rows <- vapply(bibliography.search, function(ii) {",
           "    found <- bibliography.ids == ii;",
           "    stopifnot(!any(is.na(found)),",
           "              sum(found) == 1L);",
           "    found <- which(found);",
           "    stopifnot(length(found) == 1L,",
           "              is.integer(found),",
           "              is.finite(found[[1L]]));",
           "    return(found[[1L]]);",
           "   }, NA_integer_);",
           "  stopifnot(all(is.finite(bibliography.rows)),",
           "            is.integer(bibliography.rows),",
           "            !any(is.na(bibliography.rows)),",
           "            all(bibliography.rows > 0L),",
           "            all(bibliography.rows <= nrow(bibliography)),",
           paste0("            all(as.character(x$", .col.ref.id, ") == as.character(bibliography$", .col.ref.id, "[bibliography.rows])),"),
           paste0("            all(is.na(x$", .col.ref.year, ") | ",
                  "                      (x$", .col.ref.year, " == bibliography$", .col.ref.year, "[bibliography.rows])));"),
           paste0("  x$", .col.ref.year, " <- bibliography$", .col.ref.year, "[bibliography.rows];"),
           .force("    ", paste0("x$", .col.ref.year), "x"),
           paste0("  stopifnot(all(is.finite(x$", .col.ref.year, ")));"),
           "  if(exists(\"logger\")) { logger(\"Validating data from file '\", file, \"'.\"); }",
           paste0("  ", validate.function.name, "(x, instances);"),
           "  if(exists(\"logger\")) { logger(\"Expanding data from file '\", file, \"'.\"); }",
           paste0("  x <- ", expand.function.name, "(x, instances);"),
           .force("  ", "x"),
           "  if(exists(\"logger\")) { logger(\"Re-validating data from file '\", file, \"'.\"); }",
           paste0("  ", validate.function.name, "(x, instances);"),
           "  if(exists(\"logger\")) { logger(\"Done loading, expanding, and validating file '\", file, \"'.\"); }",
           "  options(old.options);",
           "  return(x);",
           "}");

  p <- parse(text=paste(code, sep="\n", collapse="\n"));
  stopifnot(!is.null(p),
            is.expression(p));

  return(code);
}


#' @title Generate Code to Load All Result Data and Create a Data Frame
#' @description This function generate code to load all the single result
#' data frames and merges   them into one big one.
#' @param meta.columns the meta-data columns. Their value can either be
#'   specified as parameter of the generated function or inside the result csv
#'   file.
#' @param result.columns the values of these columns can only be specified in
#'   the csv file
#' @param function.name the function name to use for the generated function
#' @param validate.function.name the name of the function generated by
#'   \code{generate.result.data.frame.validate.function}
#' @param expand.function.name the name of the function generated by
#'   \code{generate.result.data.frame.expand.function}
#' @param frame.load.function.name the name of the single frame loading function
#' @return a function for loading a data frame from a csv file
#' @export generate.result.load.all.function
#' @include names.R
#' @include create_column.R
generate.result.load.all.function <- function(meta.columns,
                                          result.columns,
                                          function.name=.func.result.load.all,
                                          validate.function.name=.func.result.data.frame.validate,
                                          expand.function.name=.func.result.data.frame.expand,
                                          frame.load.function.name=.func.result.data.frame.load) {

  stopifnot(is.character(function.name),
            length(function.name) == 1L,
            nchar(function.name) > 0L,
            is.character(validate.function.name),
            length(validate.function.name) == 1L,
            nchar(validate.function.name) > 0L,
            is.character(expand.function.name),
            length(expand.function.name) == 1L,
            nchar(expand.function.name) > 0L,
            is.character(frame.load.function.name),
            length(frame.load.function.name) == 1L,
            nchar(frame.load.function.name) > 0L,
            !is.na(meta.columns),
            is.list(meta.columns),
            length(meta.columns) == 3L,
            !is.na(result.columns),
            is.list(result.columns),
            length(result.columns) == 3L);

  .check.columns(meta.columns$columns, meta.columns$conditions, meta.columns$mergers);
  .check.columns(result.columns$columns, result.columns$conditions, result.columns$mergers);

  stopifnot(sum(vapply(meta.columns$columns, function(ii) ii$title == .col.algo.id, TRUE)) == 1L);
  stopifnot(sum(vapply(meta.columns$columns, function(ii) ii$title == .col.inst.id, TRUE)) == 1L);
  stopifnot(sum(vapply(meta.columns$columns, function(ii) ii$title == .col.ref.id, TRUE)) == 1L);

  all.cols <- unlist(list(meta.columns$columns, result.columns$columns), recursive = FALSE);

  code <-c("#' @title Load All Result Data Frames based on a Meta-Data Frame",
           "#' @description Load a all result data frames based on a meta-data frame.",
           "#' @param file the path to the meta-data file to load",
           "#' @param directory the directory with the single result frames",
           "#' @param bibliography the bibliography data frame, used to check/infere the references and reference years",
           "#' @param instances the instance data frame, used to check/infere the instances and instance lower bounds",
           "#' @return a fully expanded and validated data frame with all the results from all references frames",
           paste0(function.name, " <- function(file, directory, bibliography, instances) {"),
           "  old.options <- options(warn=2);",
           "  stopifnot(is.character(file),",
           "            !is.na(file),",
           "            nchar(file) > 0L,",
           "            is.character(directory),",
           "            !is.na(directory),",
           "            nchar(directory) > 0L,",
           "            is.data.frame(bibliography),",
           "            !is.na(bibliography),",
           "            nrow(bibliography) > 0L,",
           "            ncol(bibliography) > 0L,",
           "            nchar(directory) > 0L,",
           "            is.data.frame(instances),",
           "            !is.na(instances),",
           "            nrow(instances) > 0L,",
           "            ncol(instances) > 0L);",
           "  file <- normalizePath(file, mustWork=TRUE);",
           "  stopifnot(file.exists(file),",
           "            file.size(file) > 0L);",
           "  directory <- normalizePath(directory, mustWork=TRUE);",
           "  stopifnot(dir.exists(directory));",
           "  if(exists(\"logger\")) { logger(\"Loading meta data from file '\", file, \"'.\"); }",
           "  meta <- read.csv(file=file, stringsAsFactors=FALSE, check.names=FALSE);",
           "  if(exists(\"logger\")) { logger(\"Done loading meta data from file '\", file, \"'.\"); }",
           "  stopifnot(is.data.frame(meta),",
           "            nrow(meta) > 0L,",
           "            ncol(meta) > 0L,",
           paste0("            sum(colnames(meta) == \"", .col.algo.id, "\") == 1L,"),
           paste0("            sum(colnames(meta) == \"", .col.ref.id, "\") == 1L,"),
           paste0("            nrow(meta) == length(unique(unname(unlist(meta$", .col.algo.id, ")))));"),
           "  if(exists(\"logger\")) { logger(\"Now loading the referenced files from directory '\", directory, \"'.\"); }",
           paste(unname(unlist(c("  x <- lapply(seq_len(nrow(meta)), function(i) ",
             frame.load.function.name, "(file.path(directory, paste0(",
                "meta$", .col.algo.id, "[[i]], \".txt\")), bibliography, instances",
                vapply(meta.columns$columns, function(mc)
                  paste0(", meta$", mc$title, "[[i]]"), ""),
           "));"))), sep="", collapse=""),
           "  if(exists(\"logger\")) { logger(\"Done loading the referenced files from directory '\", directory, \"', now merging the frames.\"); }",
           .force("  ", "x"),
           "  x <- do.call(rbind, x);",
           .force("  ", "x"),
           "  if(exists(\"logger\")) { logger(\"Now validating joint frame.\"); }",
           paste0("  ", validate.function.name, "(x, instances);"),
           "  if(exists(\"logger\")) { logger(\"Now expanding joint frame.\"); }",
           paste0("  x <- ", expand.function.name, "(x, instances);"),
           .force("  ", "x"),
           "  if(exists(\"logger\")) { logger(\"Now re-validating joint frame.\"); }",
           paste0("  ", validate.function.name, "(x, instances);"),
           "  if(exists(\"logger\")) { logger(\"Now finalizing data.\"); }",
           paste0("x <- x[order(as.character(x$", .col.algo.id, "), as.character(x$",
                          .col.ref.id, "), as.character(x$", .col.inst.id, ")),];"),
           .force("  ", "x"),
           #paste0("  x$", .col.algo.id, " <- as.factor(x$", .col.algo.id, ");"),
           #paste0("  x$", .col.inst.id, " <- as.factor(x$", .col.inst.id, ");"),
           #paste0("  x$", .col.ref.id, " <- as.factor(x$", .col.ref.id, ");"),
           #.force("  ", "x"),
           "  keep <- vapply(seq_len(ncol(x)), function(i) any(!is.na(x[, i])), FALSE);",
           "  stopifnot(any(keep));",
           "  x <- x[, keep];",
           .force("  ", "x"),
           unname(unlist(lapply(all.cols,
              function(column) {
                if((column$type == "real") |
                   (column$type == "double") |
                   (column$type == "numeric")) {
                  return(c(paste0("  if(\"", column$title, "\" %in% colnames(x)) {"),
                           paste0("    tempcol.a <- unname(unlist(x$", column$title, "));"),
                           "    tempcol.b <- try.convert.numeric.to.int(x=tempcol.a, stopIfFails=FALSE, canFloor=FALSE);",
                           "    if(!identical(tempcol.a, tempcol.b)) {",
                           paste0("      x$", column$title, " <- tempcol.b;"),
                           .force("      ", paste0("x$", column$title), "x"),
                           "    }",
                           "  }"));
                }
                return(character(0));
              }))),
           .force("  ", "x"),
           "  options(old.options);",
           "  if(exists(\"logger\")) { logger(\"Done with loading, merging, and finalizing the data.\"); }",
           "  return(x);",
           "}"
  );


  p <- parse(text=paste(code, sep="\n", collapse="\n"));
  stopifnot(!is.null(p),
            is.expression(p));

  return(code);
}

#' @title Generate All Code Necessary for Fully Loading, Expanding, and Validating a Set of Results
#' @description This function invokes all the single code generators needed to build the loader code
#' @param meta.columns the meta-data columns. Their value can either be
#'   specified as parameter of the generated function or inside the result csv
#'   file.
#' @param result.columns the values of these columns can only be specified in
#'   the csv file
#' @param function.name the function name to use for the generated function
#' @param validate.function.name the name of the function generated by
#'   \code{generate.result.data.frame.validate.function}
#' @param expand.function.name the name of the function generated by
#'   \code{generate.result.data.frame.expand.function}
#' @param frame.load.function.name the name of the single frame loading function
#' @return a list with two members: \code{name}, the name of the generated function and \code{code}, the code of the generated functions
#' @export generate.loader.functions
#' @include names.R
#' @include create_column.R
#' @include create_standard_meta_columns.R
#' @include create_standard_result_columns.R
generate.loader.functions <- function(meta.columns=create.standard.meta.columns(),
                                      result.columns=create.standard.result.columns(),
                                      function.name=.func.result.load.all,
                                      validate.function.name=.func.result.data.frame.validate,
                                      expand.function.name=.func.result.data.frame.expand,
                                      frame.load.function.name=.func.result.data.frame.load) {

  stopifnot(is.character(function.name),
            length(function.name) == 1L,
            nchar(function.name) > 0L,
            is.character(validate.function.name),
            length(validate.function.name) == 1L,
            nchar(validate.function.name) > 0L,
            is.character(expand.function.name),
            length(expand.function.name) == 1L,
            nchar(expand.function.name) > 0L,
            is.character(frame.load.function.name),
            length(frame.load.function.name) == 1L,
            nchar(frame.load.function.name) > 0L,
            !is.na(meta.columns),
            is.list(meta.columns),
            length(meta.columns) == 3L,
            !is.na(result.columns),
            is.list(result.columns),
            length(result.columns) == 3L);

  .check.columns(meta.columns$columns, meta.columns$conditions, meta.columns$mergers);
  .check.columns(result.columns$columns, result.columns$conditions, result.columns$mergers);

  stopifnot(sum(vapply(meta.columns$columns, function(ii) ii$title == .col.algo.id, TRUE)) == 1L);
  stopifnot(sum(vapply(meta.columns$columns, function(ii) ii$title == .col.inst.id, TRUE)) == 1L);
  stopifnot(sum(vapply(meta.columns$columns, function(ii) ii$title == .col.ref.id, TRUE)) == 1L);

  all <- join.columns(meta.columns, result.columns);
  .check.columns(all$columns, all$conditions, all$mergers);

  code <- unname(unlist(c(
    generate.result.data.frame.validate.function(all, validate.function.name),
    "",
    generate.result.data.frame.expand.function(all, expand.function.name, validate.function.name),
    "",
    generate.result.data.frame.load.function(meta.columns, result.columns, frame.load.function.name,
                                             validate.function.name, expand.function.name),
    "",
    generate.result.load.all.function(meta.columns,
                                      result.columns,
                                      function.name,
                                      validate.function.name,
                                      expand.function.name,
                                      frame.load.function.name)
  )));


  p <- parse(text=paste(code, sep="\n", collapse="\n"));
  stopifnot(!is.null(p),
            is.expression(p));

  result <- list(name=function.name, code=code);
  result <- force(result);

  return(result);
}
