#' Calculation of (signed) eigengene-based connectivity, without perturbing gene names.
#'
#' This is a wrapper for signedKME, with the additional step of reverting the gene names in the
#' result to the input gene names. signedKME converts all special characters in the gene names to
#' ".", and this causes name mismatches downstream.
#' @param datExpr a data frame containing the gene expression data, passed to signedKME
#' @param datME a data frame containing module eigengenes, passed to signedKME
#' @param ... additional optional arguments to be passed to signedKME
#' @export
#' @return A data frame, as returned by \code{signedKME}, with any "_" in the gene names of \code{datExpr} retained as "_" in the result.
#' @usage signedKME2(datExpr, datME, ...)
signedKME2 <- function(datExpr, datME, ...) {
  result <- WGCNA::signedKME(datExpr, datME, ...)
  rownames(result) <- colnames(datExpr)
  return(result)
}
