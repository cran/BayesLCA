as.mcmc.blca.gibbs <-
function(fit){

	if(class(fit)[1]!= "blca.gibbs") stop("Invalid Class, must be blca.gibbs object")
	else{
  G<-length(fit$classprob)
  M<-dim(fit$itemprob)[2]
  y<-fit$samples$classprob
  colnames(y)<-paste(rep("ClassProb",G),1:G)
  for(g in 1:G) y<-cbind(y,fit$samples$itemprob[,g,])
  colnames(y)[(G+1):(G+G*M)]<-paste(rep("ItemProb",G*M), rep(1:G, each=M), rep(1:M,G) )
  as.mcmc(y)
  }
}
