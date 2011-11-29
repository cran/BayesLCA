summary.blca.boot <-
function(object, ...){
	sum0<-NULL
	sum1<- c(nrow(object$samples$classprob), object$logpost, object$AIC, object$BIC)
	names(sum1)<- c("IterNumber", "Log-Posterior", "AIC", "BIC")
	sum2<-NULL
	sum2$ItemProb<- c(object$prior$alpha, object$prior$beta)
	sum2$ClassProb<- object$prior$delta
	
	sum0$sum1<- sum1
	sum0$sum2<- sum2
	class(sum0)<-"summary.blca.boot"
	return(sum0)
	}
