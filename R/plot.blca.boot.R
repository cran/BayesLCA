plot.blca.boot <-
function(x, which=c(1L,3L), main="", ...){
	if(attr(x, "class")!="blca.boot") stop("Incorrect class")
	
	logistore<- devAskNewPage()
	devAskNewPage(TRUE)
	
	show<- rep(FALSE, 5)
	show[which]<-TRUE
	
	if(show[1]){
	 Theta<- x$itemprob
	Tau<- x$classprob
		
		
	 M<- ncol(Theta)
	G<- nrow(Theta)
	xcut<-c(0,cumsum(Tau))
	ycut<-0:M
	
	image(ycut, xcut, t(Theta), axes=FALSE, ylab="Groups", xlab="Variables", main=main, ...)
	abline(h=xcut)
	abline(v=ycut)
	
	xlabel<- (xcut[-(G+1)] + xcut[-1])*0.5
	ylabel<- (ycut[-(M+1)] + ycut[-1])*0.5
	axis(1, ylabel, 1:M)
	axis(2, xlabel, 1:G)
	
	mtext("Item Probabilities", 3, 0.25)
	}
	
		if(show[2]){ 
		N<- nrow(x$Z)
		
		ind1<- N%/%20 + 1
		o1<- order(x$count, decreasing=TRUE)
		o2<- order(x$classprob, decreasing=TRUE)
		
		Z<- (x$count*x$Z)[o1, o2]
		
		for(ind2 in 1:ind1){
		
		mmax<- min(ind2*20, N)
		
		ind3<- ((ind2 - 1)*20 + 1):mmax
		
		mosaicplot(Z[ind3, ], col= (1:length(x$classprob)+1)[o2], main=main, las=2, ...)
		}
		}
	
	if(show[3]){
	M<-dim(x$samples$itemprob)[3]
	G<-dim(x$samples$itemprob)[2]
	
	tau<- x$classprob
		 
	if(is.null(dimnames(x$itemprob)[2])){
		caption<-paste(rep("Column",M), 1:M)
		 }
	else caption<- dimnames(x$itemprob)[2][[1]]
	
	for(m in 1:M){
		
		r<- range(x$samples$itemprob[,,m])
		denmaty<-denmatx<- matrix(0,512, G)
		
		for(g in 1:G){
			dentoy<- density(x$samples$itemprob[, g, m], from=r[1], to=r[2])
			denmatx[, g]<- dentoy$x 
			denmaty[, g]<- tau[g]*dentoy$y
		}
				
		plot.mat<- cbind(denmatx[,1], rowSums(denmaty))
		colnames(plot.mat)<- c("Probability", "Density")
		
		plot(plot.mat,  type='l', main=main,...)
		mtext(caption[m], 3, 0.25)
				
		for(g in 1:G) 	lines(denmatx[, g], denmaty[, g], col=g+1, lty=2, lwd=1)
		
		}
	}
	
	if(show[4]){
		
		G<- nrow(x$itemprob)

		denmaty<- denmatx<- matrix(0,512, G)
		
		for(g in 1:G){
			dentoy<- density(x$samples$classprob[,g])
			denmatx[, g]<- dentoy$x 
			denmaty[, g]<- dentoy$y
			}
				
		plot.mat<- cbind(denmatx[,1], denmaty[,1])
		
		colnames(plot.mat)<- c("Probability", "Density")

		plot(plot.mat,  type='n', main=main, xlim=range(denmatx), ylim=c(0, max(denmaty)), ...)
		mtext("Condit. Membership", 3, 0.25)
		for(g in 1:G) 	lines(denmatx[, g], denmaty[, g], col=g+1, lty=2, lwd=1)
		
		} 
		
		if(show[5]) warning("No diagnostic plot for bootstrapping method")
devAskNewPage(logistore)

}
