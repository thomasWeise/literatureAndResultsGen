#' @title Generate an Markdown Style References
#' @description Generate an Markdown formatted references.
#' @param refs an arbitrary collection of vectors with reference IDs
#' @param bibliography a bibliography data set
#' @param first.line the first line
#' @param logger an optional logger function
#' @return the multi-line reference text
#' @include names.R
#' @include make_references_text.R
#' @export make.md.references
make.md.references <- function(refs, bibliography, first.line="# References", logger=NULL) {
  return(make.references.text(refs=refs,
                              bibliography=bibliography,
                              logger=logger,
                              subst.doi="doi:\\[\\1\\]\\(\\2\\)",
                              subst.url="\\<\\1\\>",
                              first.line.start=first.line,
                              first.line.as.separate.line=TRUE,
                              normal.line.start="* ",
                              between.two.lines=NULL,
                              after.first.line=if(is.null(first.line)) { NULL } else { "" }));
}
