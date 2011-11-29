summary.blca.em <-
function(object, ...){
	sum0<-NULL
	sum1<- c(object$iter, object$poststore[length(object$poststore)] - object$poststore[length(object$poststore)-1], object$logpost, object$AIC, object$BIC)
	names(sum1)<- c("IterNumber", "ConvergenceDiff", "Log-Posterior", "AIC", "BIC")
	sum2<-NULL
	sum2$ItemProb<- c(object$prior$alpha, object$prior$beta)
	sum2$ClassProb<- object$prior$delta
	
	sum0$sum1<- sum1
	sum0$sum2<- sum2
	class(sum0)<-"summary.blca.em"
	return(sum0)
	}
