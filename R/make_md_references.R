#' @title Generate an Markdown Style References
#' @description Generate an Markdown formatted references.
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
#' @return the multi-line reference text
#' @include names.R
#' @include make_references_text.R
#' @export make.md.references
make.md.references <- function(refs, bibliography,  logger=NULL,
                               subst.doi="doi:\\[\\1\\]\\(\\2\\)",
                               subst.url="\\<\\1\\>",
                               first.line.start="# References",
                               first.line.end="",
                               first.line.as.separate.line=TRUE,
                               make.text.before=NULL,
                               make.text.after=NULL,
                               normal.line.start="* ",
                               normal.line.end="",
                               between.two.lines=NULL,
                               after.first.line=if(is.null(first.line)) { NULL } else { "" }) {
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
                              normal.line.end=normal.line.end,
                              between.two.lines=between.two.lines,
                              after.first.line=after.first.line));
}
