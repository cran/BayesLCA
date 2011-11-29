plot.blca.em <-
function(x, which=1L, main="", ...){
	
	if(attr(x, "class")!="blca.em") stop("Incorrect class")
	
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
	if(any(show[3:4])) warning("Density estimates not supported for em estimates.")
		if(show[5]){
		
		lbplot<- cbind(1:length(x$poststore), x$poststore)
		colnames(lbplot)<- c("Iteration", "Lower Bound")
		plot(lbplot, type='b')
		mtext("Algorithm Convergence", 3, 0.25)
		}
	devAskNewPage(logistore)
	}
