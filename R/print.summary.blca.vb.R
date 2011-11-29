print.summary.blca.vb <-
function(x, ...){
	printnames<- c("Number of iterations:", "Lower Bound Increase at Convergence:", "Lower Bound:")
	cat("__________________\n")
	cat("Hyper-Parameters: \n")
	cat("\nItem Probabilities:", x$sum2$ItemProb, "\n")
	cat("\nClass Probabilities:", x$sum2$ClassProb, "\n")
	cat("__________________\n")
	cat("Bayes-LCA\n")
	cat("Diagnostic Summary\n")
	cat("__________________\n")
	cat("Method: Variational EM Algorithm \n")
	for(ind in 1:length(x$sum1)){
		cat("\n", printnames[ind], x$sum1[ind], "\n")
		#cat(, "\n")
		}
	}
