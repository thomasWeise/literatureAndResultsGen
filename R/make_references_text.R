.return.empty.string <- function(ref.id) ""

#' @title Generate an References List
#' @description Generate an a textual list of references based on the given
#'   formatting instructions. This function is used by
#'   \link{make.r.doc.references} and \link{make.md.references}.
#' @param refs an arbitrary collection of vectors with reference IDs, if
#'   \code{NULL}, then all bibliographic entries are used
#' @param bibliography a bibliography data set
#' @param logger an optional logger function
#' @param subst.doi a substitute regular expression for dois: "\\1" is the doi,
#'   "\\2" is the full link
#' @param subst.url a substitute regular expression for urls, "\\1" is the url
#' @param first.line.start the starting string of the first line
#' @param first.line.end the ending string of the first line
#' @param first.line.as.separate.line should \code{first.line.start} be a
#'   separate line (\code{TRUE}) or should it be the beginning pasted in front
#'   of a normal line of references (\code{FALSE})
#' @param make.text.before a function to which the reference id is passed and
#'   which should return a string to be pasted before the actual reference text
#'   (or \code{""}); if \code{NULL} is supplied, no strings are added
#' @param make.text.after a function to which the reference id is passed and
#'   which should return a string to be pasted after the actual reference text
#'   (or \code{""}); if \code{NULL} is supplied, no strings are added
#' @param normal.line.start a string to paste at the beginning of every normal
#'   line
#' @param normal.line.end a string to paste at the end of every normal line
#' @param between.two.lines a string to be pasted as separate line between two
#'   normal lines
#' @param after.first.line the string to be inserted as line after the first
#'   line
#' @return the multi-line reference text
#' @include names.R
#' @export make.references.text
make.references.text <- function(refs=NULL, bibliography, logger=NULL,
                                 subst.doi="doi:\\\\href\\{\\2\\}\\{\\1\\}",
                                 subst.url="\\\\url\\{\\1\\}",
                                 first.line.start="#' @references ",
                                 first.line.end="",
                                 first.line.as.separate.line=FALSE,
                                 make.text.before=NULL,
                                 make.text.after=NULL,
                                 normal.line.start="#' ",
                                 normal.line.end="",
                                 between.two.lines ="#'",
                                 after.first.line=between.two.lines) {

  stopifnot(is.data.frame(bibliography),
            .col.ref.id %in% colnames(bibliography),
            .col.ref.text %in% colnames(bibliography),
            nrow(bibliography) > 0L);

  if(is.null(refs)) {
    refs <- unname(unlist(bibliography[.col.ref.id]));
  }
  if(is.null(make.text.after)) { make.text.after <- .return.empty.string; }
  stopifnot(is.function(make.text.after));
  if(is.null(make.text.before)) { make.text.before <- .return.empty.string; }
  stopifnot(is.function(make.text.before));

  refs <- as.character(unique(unname(unlist(refs))));
  stopifnot(is.character(refs),
            length(refs) > 0L,
            all(!is.na(refs)));
  refs <- as.character(unique(unname(unlist(strsplit(unname(unlist(refs)), ";")))));
  stopifnot(is.character(refs),
            length(refs) > 0L,
            all(!is.na(refs)));

  if(!is.null(logger)) {
    logger("Generating reference for following works: ", paste(refs, sep=", ", collapse=", "));
  }

  bibliography <- bibliography[order(unname(unlist(bibliography[.col.ref.text]))), ];

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

  texts <- unique(as.character(unname(unlist(bibliography[.col.ref.text]))));
  stopifnot(length(texts) == length(map),
            all(nchar(texts) > 0L));

  between.two.lines <- force(between.two.lines);
  between.two.lines <- do.call(force, list(between.two.lines));
  after.first.line <- force(after.first.line);
  after.first.line <- do.call(force, list(after.first.line));
  first.line.start <- force(first.line.start);
  first.line.start <- do.call(force, list(first.line.start));
  normal.line.start <- force(normal.line.start);
  normal.line.start <- do.call(force, list(normal.line.start));
  normal.line.end <- force(normal.line.end);
  normal.line.end <- do.call(force, list(normal.line.end));
  first.line.end <- force(first.line.end);
  first.line.end <- do.call(force, list(first.line.end));

  stopifnot(is.character(between.two.lines) | is.null(between.two.lines),
            is.character(after.first.line) | is.null(after.first.line),
            is.character(first.line.start) | is.null(first.line.start),
            is.character(first.line.end) | is.null(first.line.end),
            is.character(normal.line.start) | is.null(normal.line.start),
            is.character(normal.line.end) | is.null(normal.line.end));

  if(is.null(first.line.start)) { first.line.start <- ""; }
  if(is.null(first.line.end)) { first.line.end <- ""; }
  if(is.null(normal.line.start)) { normal.line.start <- ""; }
  if(is.null(normal.line.end)) { normal.line.end <- ""; }

  refs <- unname(unlist(lapply(refs,
                               function(ref) {
                                 ref.id <- bibliography$ref.id[[ref]];
                                 stopifnot(!is.na(ref),
                                           !is.null(ref),
                                           length(ref) == 1L,
                                           is.integer(ref),
                                           ref > 0L,
                                           ref <= nrow(bibliography),
                                           is.character(ref.id),
                                           nchar(ref.id) > 0L,
                                           length(ref.id) == 1L);
                                 text <- paste(unlist(texts[[ref]]), sep=" ", collapse=" ");
                                 stopifnot(nchar(text) > 0L);
                                 text <- sub("doi:\\s+(.*?)\\s+\\(URL\\:\\s+(.*?)\\)", subst.doi, text);
                                 stopifnot(nchar(text) > 0L);
                                 text <- sub("<URL\\:\\s+(.+?)>", subst.url, text);
                                 stopifnot(nchar(text) > 0L);
                                 text <- sub("\\(URL\\:\\s+(.+?)\\)", subst.url, text);
                                 stopifnot(nchar(text) > 0L);

                                 before <- unname(unlist(make.text.before(ref.id)));
                                 if(is.null(before)) {
                                   before <- "";
                                 } else {
                                   before <- paste(before, sep="", collapse="");
                                 }
                                 stopifnot(is.character(before),
                                           nchar(before) >= 0L,
                                           !is.na(before));

                                 after <- unname(unlist(make.text.after(ref.id)));
                                 if(is.null(after)) {
                                   after <- "";
                                 } else {
                                   after <- paste(after, sep="", collapse="");
                                 }
                                 stopifnot(is.character(after),
                                           nchar(after) >= 0L,
                                           !is.na(after));

                                 text <- paste0(normal.line.start,
                                                before,
                                                text,
                                                after,
                                                normal.line.end);
                                 return(unname(unlist(c(text, between.two.lines))));
                               })));

  if(first.line.as.separate.line) {
    refs <- unname(unlist(c(paste0(first.line.start, first.line.end), after.first.line, refs)));
  } else {
    refs[[1L]] <- paste0(first.line.start,
                         substr(refs[[1L]],
                                nchar(normal.line.start) + 1L,
                                nchar(refs[[1L]]) - nchar(normal.line.end)),
                         first.line.end);

    if(!identical(between.two.lines, after.first.line)) {
      if(is.null(between.two.lines)) {
        if(is.null(after.first.line)) {
          # nothing to be done here
        } else {
          refs <- unname(unlist(c(refs[[1L]], after.first.line, refs[-1L])));
        }
      } else {
        if(is.null(after.first.line)) {
          refs <- refs[-2L];
        } else {
          refs[[2L]] <- after.first.line;
        }
      }
    }
  }

  refs <- force(refs);
  refs <- do.call(force, list(refs));
  refs <- force(refs);

  stopifnot(all(nchar(refs) > 0L));

  if(!is.null(logger)) {
    logger("Done generating documentation references.");
  }

  return(refs);
}
