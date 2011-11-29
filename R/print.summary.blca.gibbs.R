print.summary.blca.gibbs <-
function(x, ...){
	printnames<- c("Chain Lengths:", "Burn-In:", "Acceptance Rate:", "Log-Posterior Mean:", "Log-Posterior Mode:", "DIC:", "BICM:", "AICM:")
	cat("__________________\n")
	cat("Bayes-LCA\n")
	cat("Diagnostic Summary\n")
	cat("__________________\n")
	cat("Hyper-Parameters: \n")
	cat("\nItem Probabilities:", x$sum2$ItemProb, "\n")
	cat("\nClass Probabilities:", x$sum2$ClassProb, "\n")
	cat("__________________\n")
	cat("Method: Gibbs Sampling \n")
	for(ind in 1:length(x$sum1)){
		cat("\n", printnames[ind], x$sum1[ind], "\n")
		#cat(, "\n")
		}
	}
