#' @title Generate an R-Documentation Style References Section
#' @description Generate an R-Documentation style references section that can
#'   readily be inserted as an R documentation comment.
#' @param refs an arbitrary collection of vectors with reference IDs
#' @param bibliography a bibliography data set
#' @param logger an optional logger function
#' @return the multi-line reference text
#' @include names.R
#' @export make.r.doc.references
make.r.doc.references <- function(refs, bibliography, logger=NULL) {

  refs <- as.character(unique(unname(unlist(refs))));
  stopifnot(is.character(refs),
            length(refs) > 0L,
            all(!is.na(refs)),
            nrow(bibliography) > 0L);
  if(!is.null(logger)) {
    logger("Generating reference for following works: ", paste(refs, sep=", ", collapse=", "));
  }

  stopifnot(.col.ref.id %in% colnames(bibliography));
  map <- as.character(unname(unlist(bibliography[.col.ref.id])));
  stopifnot(is.character(map),
            length(map) == nrow(bibliography),
            all(!is.na(map)));

  found <- vapply(refs, function(ii) {
    find <- (map == ii);
    stopifnot(sum(find) == 1L);
    find <- which(find);
    stopifnot(length(find) == 1L,
              is.finite(find),
              find > 0L,
              find <= length(map));
    return(find);
  }, NA_integer_);
  stopifnot(is.integer(found),
            all(is.finite(found)),
            length(unique(found)) == length(refs));

  refs <- sort(found);

  stopifnot(.col.ref.text %in% colnames(bibliography));
  texts <- as.character(unname(unlist(bibliography[.col.ref.text])));
  stopifnot(length(texts) == length(map),
            all(nchar(texts) > 0L));

  refs <- unname(unlist(lapply(refs,
                               function(ref) {
                                 text <- paste(unlist(texts[[ref]]), sep="\n", collapse="\n");
                                 stopifnot(nchar(text) > 0L);
                                 text <- strsplit(text, "\n", fixed=TRUE);
                                 stopifnot(length(text) == 1L);
                                 text <- text[[1L]];
                                 stopifnot(length(text) >= 1L);
                                 text <- paste0("#' ", text);
                                 return(unname(unlist(c(text, "#' "))));
                               })));

  refs[[1L]] <- paste0("#' @references ", substr(refs[[1L]], 3L, nchar(refs[[1L]])));

  refs <- force(refs);
  refs <- do.call(force, list(refs));
  refs <- force(refs);

  stopifnot(all(nchar(refs) > 0L));

  if(!is.null(logger)) {
    logger("Done generating documentation references.");
  }

  return(refs);
}
