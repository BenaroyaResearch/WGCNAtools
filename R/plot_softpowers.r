#' Calculate and plot soft thresholding power diagnostics
#'
#' Calculate and plot diagnostics for a range of values of the soft thresholding power, as described
#' in the online WGCNA tutorials. It is a wrapper for WGCNA::pickSoftThreshold.
#' @param counts A matrix or data frame of counts, with genes in columns and samples in rows
#' @param powers A vector of soft thresholding powers; passed to \code{WGCNA::pickSoftThreshold}
#' @param networkType Character value, the network type, passed to \code{WGCNA::pickSoftThreshold}. Allowed values are (unique abbreviations of) "unsigned", "signed", "signed hybrid". See \code{WGCNA::adjacency}
#' @param verbose Integer level of verbosity; passed to \code{WGCNA::pickSoftThreshold}
#' @importFrom graphics abline text
#' @export
#' @usage plot_softpowers(counts, powers = c(1:10, seq(from = 12, to = 20, by = 2)), 
#'   networkType = "unsigned", verbose = 5)
plot_softpowers <- function(counts, powers = c(1:10, seq(from = 12, to = 20, by = 2)),
                            networkType = "unsigned", verbose = 5) {

  # Call the network topology analysis function
  sft.tmp <- WGCNA::pickSoftThreshold(counts, powerVector = powers, networkType = networkType, verbose = verbose)

  # Scale-free topology fit index as a function of the soft-thresholding power
  plot(sft.tmp$fitIndices[,1],
       -sign(sft.tmp$fitIndices[,3]) * sft.tmp$fitIndices[,2],
       xlab = "Soft Threshold (power)", ylab = "Scale Free Topology Model Fit,signed R^2", type = "n",
       main = paste("Scale independence"));
  text(sft.tmp$fitIndices[,1],
       -sign(sft.tmp$fitIndices[,3]) * sft.tmp$fitIndices[,2],
       labels = powers, cex = 0.9, col = "red");
  # this line corresponds to using an R^2 cut-off of 0.9
  abline(h = 0.90, col = "red")

  # Mean connectivity as a function of the soft-thresholding power
  plot(sft.tmp$fitIndices[,1], sft.tmp$fitIndices[,5],
       xlab = "Soft Threshold (power)", ylab = "Mean Connectivity", type = "n",
       main = paste("Mean connectivity"))
  text(sft.tmp$fitIndices[,1], sft.tmp$fitIndices[,5], labels = powers, cex = 0.9, col = "red")
}