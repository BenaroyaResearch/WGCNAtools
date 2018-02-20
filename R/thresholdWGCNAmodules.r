#' Remove genes from modules if connectivity is below a specified threshold
#'
#' Remove genes from WGCNA modules if connectivity to the module (kME) is below a specified threshold.
#' All such genes are assigned to a new module, which can be named by the user.
#' @param gene_modules A data frame genes, with columns for the assigned module and connectivity to module.
#' @param threshold The connectivity threshold; genes with connectivity below this value will be removed. Defaults to 0.25.
#' @param color.no_mod The name for the new module, to which genes not meeting the threshold will be assigned.
#' @param color_col Optional, the name or number of the column in \code{gene_modules} which contains the module names.
#' @param kME_col Optional, the name or number of the column in \code{gene_modules} which contains the connectivity values.
#' @export
#' @return A new data frame with the same dimension and names as \code{gene_modules}, with 0 or more genes assigned to the new module.
#' @usage \code{thresholdWGCNAmodules(gene_modules, threshold=0.25, color.no_mod="white",
#'   color_col="color_assigned", kME_col="kME_color_assigned")}
thresholdWGCNAmodules <-
  function(
    gene_modules, threshold=0.25, color.no_mod="white", 
    color_col="color_assigned", kME_col="kME_color_assigned") {
    gene_modules[gene_modules[,kME_col] < threshold, color_col] <- color.no_mod
    gene_modules[gene_modules[,color_col]==color.no_mod, kME_col] <- NA
    cat(sum(gene_modules[,color_col]==color.no_mod), " genes set to no module.\n", sep="")
    return(gene_modules)
  }
