#' @title Generate an R-Documentation Style References Section
#' @description Generate an R-Documentation style references section that can
#'   readily be inserted as an R documentation comment.
#' @param refs an arbitrary collection of vectors with reference IDs
#' @param bibliography a bibliography data set
#' @param logger an optional logger function
#' @param subst.doi a substitute regular expression for dois: "\\1" is the doi,
#'   "\\2" is the full link (optional, default value is supplied)
#' @param subst.url a substitute regular expression for urls, "\\1" is the url
#'   (optional, default value is supplied)
#' @param first.line.start the starting string of the first line (optional,
#'   default value is supplied)
#' @param first.line.end the ending string of the first line (optional, default
#'   value is supplied)
#' @param first.line.as.separate.line should \code{first.line.start} be a
#'   separate line (\code{TRUE}) or should it be the beginning pasted in front
#'   of a normal line of references (\code{FALSE}) (optional, default value is
#'   supplied)
#' @param make.text.before a function to which the reference id is passed and
#'   which should return a string to be pasted before the actual reference text
#'   (or \code{""}); \code{NULL} is supplied by default, meaning that no strings
#'   are added
#' @param make.text.after a function to which the reference id is passed and
#'   which should return a string to be pasted after the actual reference text,
#'   (or \code{""}); \code{NULL} is supplied by default, meaning that
#' @param normal.line.start a string to paste at the beginning of every normal
#'   line (optional, default value is supplied)
#' @param normal.line.end a string to paste at the end of every normal line
#'   (optional, default value is supplied)
#' @param between.two.lines a string to be pasted as separate line between two
#'   normal lines (optional, default value is supplied)
#' @param after.first.line the string to be inserted as line after the first
#'   line (optional, default value is supplied)
#' @param sort should the bibliographic references be sorted?
#' @return the multi-line reference text
#' @include names.R
#' @include make_references_text.R
#' @export make.r.doc.references
make.r.doc.references <- function(refs, bibliography, logger=NULL,
                                  subst.doi="doi:\\\\href\\{\\2\\}\\{\\1\\}",
                                  subst.url="\\\\url\\{\\1\\}",
                                  first.line.start="#' @references ",
                                  first.line.end="",
                                  first.line.as.separate.line=FALSE,
                                  make.text.before=NULL,
                                  make.text.after=NULL,
                                  normal.line.start="#' ",
                                  normal.line.end="",
                                  between.two.lines="#'",
                                  after.first.line="#'",
                                  sort=TRUE) {
  return(make.references.text(refs=refs,
                              bibliography=bibliography,
                              logger=logger,
                              subst.doi=subst.doi,
                              subst.url=subst.url,
                              first.line.start=first.line.start,
                              first.line.end=first.line.end,
                              first.line.as.separate.line=first.line.as.separate.line,
                              make.text.before=make.text.before,
                              make.text.after=make.text.after,
                              normal.line.start=normal.line.start,
                              normal.line.end=normal.line.start,
                              between.two.lines=between.two.lines,
                              after.first.line=after.first.line,
                              sort=sort));
}
