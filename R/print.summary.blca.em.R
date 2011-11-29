print.summary.blca.em <-
function(x, ...){
	printnames<- c("Number of iterations:","Log-Posterior Increase at Convergence:", "Log-Posterior:", "AIC:", "BIC:")
	cat("__________________\n")
	cat("Bayes-LCA\n")
	cat("Diagnostic Summary\n")
	cat("__________________\n")
	cat("Hyper-Parameters: \n")
	cat("\nItem Probabilities:", x$sum2$ItemProb, "\n")
	cat("\nClass Probabilities:", x$sum2$ClassProb, "\n")
	cat("__________________\n")
	cat("Method: EM Algorithm \n")
	for(ind in 1:length(x$sum1)){
		cat("\n", printnames[ind], x$sum1[ind], "\n")
		#cat(, "\n")
		}
	}
