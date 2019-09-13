#' @title Generate an R-Documentation Style References Section
#' @description Generate an R-Documentation style references section that can
#'   readily be inserted as an R documentation comment.
#' @param refs an arbitrary collection of vectors with reference IDs
#' @param bibliography a bibliography data set
#' @param logger an optional logger function
#' @return the multi-line reference text
#' @include names.R
#' @include make_references_text.R
#' @export make.r.doc.references
make.r.doc.references <- function(refs, bibliography, logger=NULL) {
  return(make.references.text(refs=refs,
                              bibliography=bibliography,
                              logger=logger,
                              subst.doi="doi:\\\\href\\{\\2\\}\\{\\1\\}",
                              subst.url="\\\\url\\{\\1\\}",
                              first.line.start="#' @references ",
                              first.line.as.separate.line=FALSE,
                              normal.line.start="#' ",
                              between.two.lines="#'",
                              after.first.line="#'"));
}
