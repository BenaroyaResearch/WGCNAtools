#' Calculation of (signed) eigengene-based connectivity, without perturbing gene names.
#'
#' This is a wrapper for signedKME, with the additional step of converting "." in the gene names of the result
#' back to "_". signedKME converts these "_" to ".", and this causes name mismatches downstream.
#' All such genes are assigned to a new module, which can be named by the user.
#' @param datExpr a data frame containing the gene expression data. Passed to signedKME.
#' @param dateME a data frame containing module eigengenes. Passed to signedKME.
#' @export
#' @return A data frame, as returned by \code{signedKME}, with any "_" in the gene names of \code{datExpr} retained as "_" in the result.
#' @usage \code{signedKME2(datExpr, datME, ...)}
signedKME2 <- function(datExpr, datME, ...) {
  result <- WGCNA::signedKME(datExpr, datME, ...)
  rownames(result) <- stringr::str_replace_all(rownames(result), "\\.", "-")
  return(result)
}