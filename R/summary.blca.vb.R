summary.blca.vb <-
function(object, ...){
	sum0<-NULL
	sum1<- c(object$iter, object$lbstore[length(object$lbstore)] - object$lbstore[length(object$lbstore)-1], object$LB)
	names(sum1)<- c("IterNumber", "ConvergenceDiff", "Lower Bound")
	sum2<-NULL
	sum2$ItemProb<- c(object$prior$alpha, object$prior$beta)
	sum2$ClassProb<- object$prior$delta
	
	sum0$sum1<- sum1
	sum0$sum2<- sum2
	class(sum0)<-"summary.blca.vb"
	return(sum0)	
	}
