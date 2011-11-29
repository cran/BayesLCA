plot.blca.vb <-
function(x, which=c(1L, 3L), main="", ...){
	
	if(attr(x, "class")!="blca.vb") stop("Incorrect class")
	
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
	
	if(any(Tau==0)){
	xcut<- xcut[Tau>0]
	warning(paste("Probability of membership to group(s)", paste((1:G)[Tau==0], collapse=","), "is exactly zero, omitted from plot."))
		}
	
	image(ycut, xcut, t(Theta[Tau>0, ]), axes=FALSE, ylab="Groups", xlab="Variables", main=main, ...)
	abline(h=xcut)
	abline(v=ycut)
	
	xlabel<- (xcut[-length(xcut)] + xcut[-1])*0.5
	ylabel<- (ycut[-(M+1)] + ycut[-1])*0.5
	axis(1, ylabel, 1:M)
	axis(2, xlabel, (1:G)[Tau>0])
	
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
	M<-dim(x$parameters$itemprob)[2]
	G<-dim(x$parameters$itemprob)[1]
	
	xseq<-seq(0,1,length.out=1000)
	
		 
	if(is.null(colnames(x$itemprob))){
		caption<-paste(rep("Column",M), 1:M)
		}else caption<- colnames(x$itemprob)
	
	for(m in 1:M){
		
		var1<- x$parameters$itemprob[,m,1]*x$parameters$itemprob[, m, 2]/((x$parameters$itemprob[,m,1]+x$parameters$itemprob[, m, 2])^2*(x$parameters$itemprob[, m, 1]+x$parameters$itemprob[, m, 2] + 1))
		
		r1<- max(min(x$itemprob[,m] - 4*sqrt(var1)),0)
		r2<- min(max(x$itemprob[,m] + 4*sqrt(var1)),1)
		
		xseq<-seq(r1,r2,length.out=1000)
		ymax<-0
		
		mden<- x$classprob[1]*dbeta(xseq, x$parameters$itemprob[1, m, 1], x$parameters$itemprob[1, m, 2] )
			
		for(g in 2:G) mden<- mden + x$classprob[g]*dbeta(xseq, x$parameters$itemprob[g,m,1], x$parameters$itemprob[g,m,2] )
		
		plot.mat<- cbind(xseq, mden)
		
		colnames(plot.mat)<- c("Probability", "Density")
		
		plot(plot.mat, main=main, type='n')
		mtext(caption[m], 3, 0.25)
		for(g in 1:G) 	lines(xseq, x$classprob[g]*dbeta(xseq, x$parameters$itemprob[g, m, 1], x$parameters$itemprob[g, m, 2] ), col=g+1 )
		
		lines(xseq,  mden, lty=2, lwd=0.5)

		}
		}
	
	if(show[4])
	{
		M<-dim(x$parameters$itemprob)[2]
		G<-dim(x$parameters$itemprob)[1]

		xseq<-seq(0,1,length.out=100)
		ymat<- matrix(0, 100, G)
		
		for(g in 1:G) ymat[,g]<- dbeta(xseq, x$parameters$classprob[g], sum(x$parameters$classprob[-g]))
		
			if(any(ymat==Inf)){
			warning("Density occuring on one point for some membership values")
			ymat[ymat==Inf]<- max(ymat[ymat!=Inf]+1)
			}
		
		plot.mat<- cbind(xseq, ymat[, 1])
		
		colnames(plot.mat)<- c("Probability", "Density")
		
		plot(plot.mat, ylim=range(ymat), type='n', main=main)
		mtext("Condit. Membership", 3, 0.25)
		for(g in 1:G) lines(xseq,  ymat[,g], lty=1, lwd=0.5, col=g+1)
		
		}
		
	if(show[5]){
		
		lbplot<- cbind(1:length(x$lbstore), x$lbstore)
		colnames(lbplot)<- c("Iteration", "Lower Bound")
		plot(lbplot, type='b')
		mtext("Algorithm Convergence", 3, 0.25)
		}
	
	devAskNewPage(logistore)
	}
