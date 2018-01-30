#' Calculate and plot soft thresholding power diagnostics
#'
#' Calculate and plot diagnostics for a range of values of the soft thresholding power, as described
#' in the online WGCNA tutorials. It is a wrapper for WGCNA::pickSoftThreshold.
#' @param counts A matrix or data frame of counts, with genes in columns and samples in rows
#' @param powers A vector of soft thresholding powers; passed to \code{pickSoftThreshold}
#' @param verbose Integer level of verbosity; passed to \code{pickSoftThreshold}
#' @export
#' @usage \code{pickSoftThrehold(counts, powers = c(c(1:10), seq(from = 12, to=20, by=2)), verbose=5))}
plot_softpowers <- function(counts, powers = c(c(1:10), seq(from = 12, to=20, by=2)), verbose=5) {
  
  # Call the network topology analysis function
  sft.tmp <- WGCNA::pickSoftThreshold(counts, powerVector = powers, verbose = verbose)
  
  # Plot the results:
  quartz(w=9,h=5)
  par(mfrow = c(1,2));
  
  # Scale-free topology fit index as a function of the soft-thresholding power
  plot(sft.tmp$fitIndices[,1],
       -sign(sft.tmp$fitIndices[,3])*sft.tmp$fitIndices[,2],
       xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
       main = paste("Scale independence"));
  text(sft.tmp$fitIndices[,1],
       -sign(sft.tmp$fitIndices[,3])*sft.tmp$fitIndices[,2],
       labels=powers,cex=0.9,col="red");
  # this line corresponds to using an R^2 cut-off of h
  abline(h=0.90,col="red")
  
  # Mean connectivity as a function of the soft-thresholding power
  plot(sft.tmp$fitIndices[,1], sft.tmp$fitIndices[,5],
       xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
       main = paste("Mean connectivity"))
  text(sft.tmp$fitIndices[,1], sft.tmp$fitIndices[,5], labels=powers, cex=0.9,col="red")
}