
.check.column <- function(title,
                          description,
                          type) {
  stopifnot(is.character(title),
            length(title) == 1L,
            nchar(title) > 0L,
            is.character(description),
            length(description) == 1L,
            nchar(description) > 0L,
            type %in% c("integer",
                        "double",
                        "character"));
}

#' @title Create a Column Descriptor
#' @description Create a column descriptor as used by our code generation
#' @param title the column title
#' @param description the column description
#' @param type the column type
#' @return the column description
#' @export create.column
create.column <- function(title,
                          description,
                          type) {
  .check.column(title, description, type);
  return(list(title=title,
              description=description,
              type=type));
}

.check.columns <- function(columns, conditions, mergers) {
  stopifnot(is.list(columns),
            length(columns) > 0L,
            is.character(conditions),
            length(conditions) >= 0L,
            !is.na(conditions),
            is.character(mergers),
            length(mergers) >= 0L,
            !is.na(mergers));
  for(col in columns) {
    .check.column(col$title, col$description, col$type);
  }
  stopifnot(length(unique(vapply(columns, function(l) l$title, ""))) == length(columns));
  if(length(conditions) > 0L) {
    p <- parse(text=conditions);
    stopifnot(all(is.language(p)),
              all(!is.null(p)),
              all(length(p) > 0L));
  }
  if(length(mergers) > 0L) {
    p<-parse(text=paste(mergers, sep="\n", collapse="\n"));
    stopifnot(is.language(p),
              !is.null(p),
              length(p) > 0L);
  }
}

#' @title Create a Set of Column Descriptors along with Auto-Generated Code
#' @description  Well, create such a set.
#' @param columns the column descriptors
#' @param conditions the auto-generated validation code
#' @param mergers the auto-generated merger code
#' @return a set of column descriptors
#' @export create.columns
create.columns <- function(columns,
                           conditions=character(0),
                           mergers=character(0)) {
  .check.columns(columns, conditions, mergers);
  l <- (list(columns=columns,
             conditions=unique(conditions),
             mergers=mergers));
  .check.columns(l$columns, l$conditions, l$mergers);
  return(l);
}

#' @title Merge Several Sets of Columns
#' @description Merge the sets stemming from \code{create.columns}
#' @param ... the columns
#' @return the merged column sets
#' @export join.columns
join.columns <- function(...) {

  all <- list(...);
  for(c in all) {
    .check.columns(c$columns, c$conditions, c$mergers);
  }

  columns    <- unlist(lapply(all, function(l) l$columns), recursive=FALSE);
  names(columns) <- vapply(columns, function(l) l$title, "");
  conditions <- unlist(lapply(all, function(l) unname(unlist(l$conditions))));
  mergers    <- unlist(lapply(all, function(l) unname(unlist(l$mergers))));

  # combine
  return(create.columns(columns = columns,
                        conditions = conditions,
                        mergers = mergers));

}
