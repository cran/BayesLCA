summary.blca.gibbs <-
function(object, ...){
	sum0<- NULL
	sum1<- c(nrow(object$samples$classprob), object$burn.in, object$accept, mean(object$samples$logpost), object$logpost, object$DIC, object$BICM, object$AICM)
	names(sum1)<- c("IterNumber", "Burn-in", "AcceptanceRate", "Dbar", "Dhat", "DIC", "BICM", "AICM")
	sum2<-NULL
	sum2$ItemProb<- c(object$prior$alpha, object$prior$beta)
	sum2$ClassProb<- object$prior$delta

	sum0$sum1<- sum1
	sum0$sum2<- sum2
	class(sum0)<-"summary.blca.gibbs"
	return(sum0)
	}
