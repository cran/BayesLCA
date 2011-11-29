blca.em <-
function(X,G,alpha=1, beta=1, delta=1, start.vals= c("single","across"), counts.n=NULL, iter=500, se=FALSE, conv=1e-6, small=1e-100)
{
	if(is.null(counts.n))
	{
		if(class(X)=="data.blca"){
			counts.n<- X$counts.n
			X<- X$data
		}else{
			Xdat<- data.blca(X)
			X<- Xdat$data
			counts.n<- Xdat$counts.n
			}# else class(X)
		} else{ 
			X<- as.matrix(X)
			if(any(X[X>0]!=1))
			stop("If vector of counts is supplied separately, then data must be binary.")
			}

	N<-nrow(X); M<-ncol(X) 
	
	if(length(delta)==1) delta<-rep(delta,G)
	if(length(delta)!=G) stop("delta prior is wrong length (i.e., !=1 or G)")
	
	if(!is.matrix(alpha)){
			if(any(length(alpha)==c(1,G)) ){
				alpha<-matrix(alpha,G,M)
			} else {
				if(length(alpha)==M){
					alpha<- matrix(alpha,G,M, byrow=TRUE)
				} else stop("Item probability prior improperly specified.")
			}
	} #else {
	#	if(!is.matrix(alpha)) stop("Item probability prior improperly specified.")
	#}	
	
	if(!is.matrix(beta)){
		if(any(length(beta)==c(1,G)) ){
			beta<-matrix(beta,G,M)
			} else {
				if(length(beta)==M){
					beta<- matrix(beta,G,M, byrow=TRUE)
				} else stop("Item probability prior improperly specified.")
			}
		} #else {
			#if(!is.matrix(beta)) stop("Item probability prior improperly specified.")
	#}	
	if(any(dim(alpha)!=c(G,M), dim(beta)!=c(G,M))) stop("Item probability prior improperly specified.")

	if(2^M <= (M+1)*G) warning(paste("Model may be improperly specified. Maximum number of classes that should be run is", floor(2^M/(M+1)), "."))
	
	N1<- sum(counts.n)
	
	conv<-N1*M*conv
	eps<-N1*M
		
	counter<-0
	llstore<-0
	
	#Set Parameters
	if(is.character(start.vals)){
	  if(start.vals[1]=="single"){
	    Z<-unMAP(sample(1:G,size=N,replace=TRUE))
	    if(ncol(Z)<G) Z<-cbind(Z, matrix(0,nrow=N, ncol=(G-ncol(Z))))
	  }else{
	    if(start.vals[1]=="across"){
	      Z<- matrix(runif(N*G), N,G)
	      Z<- Z/rowSums(Z)
	      } else stop("start.vals improperly specified. See help files for more details.")
	    }
	 } else{
	      if(is.matrix(start.vals) & all(dim(as.matrix(start.vals)) == c(N,G)))  Z<- start.vals else{
		  if(is.numeric(start.vals) & length(as.numeric(start.vals))==N){ 
		    Z<- unMAP(start.vals)
		    } else stop("start.vals improperly specified. See help files for more details.")
		 }
	  }	
	
	while(abs(eps)>conv || counter< 20)
	{	
		Z.sum<-colSums(Z*counts.n)
		
		#M-step
		Taut<-(Z.sum+delta-1)/(N1 + sum(delta)-G)
		Thetat<-(t(Z)%*%(X*counts.n) + alpha-1)/(Z.sum + alpha + beta -2 + small)
		
		if(any(Taut<0)){Taut[Taut<0]<-0; Taut<-Taut/sum(Taut)}
		if(any(Thetat<0)) Thetat[Thetat<0]<-0
		if(any(Thetat>1)) Thetat[Thetat>1]<- 1 ##This should only be due to precision error
				
		#E-Step
		dum<-array(apply(Thetat,1,dbinom, size=1, x=t(X)), dim=c(M,N,G))
		Z1<-t(Taut*t(apply(dum, c(2,3), prod))) + small
		Z<-Z1/rowSums(Z1)

		#Log-Posterior
		l<-sum(log(rowSums(Z1))*counts.n)+sum(xlogy(alpha-1,Thetat)+xlogy(beta-1,1-Thetat))+sum(xlogy(delta-1, Taut))
				
		llstore[counter]<-l
		if(counter>2)
		{
			ll.inf<-(llstore[counter-1] - llstore[counter-2])/(llstore[counter] - llstore[counter-1])*(llstore[counter] - llstore[counter-2]) + llstore[counter-1]
		
			if(llstore[counter] == llstore[counter-1]){ ll.inf<- llstore[counter]}
		
			eps<- ll.inf - llstore[counter]
		}			
				
		counter<-counter+1		
		if(counter>iter){print("Max iter Reached"); break}
		}#while
		x<-NULL
		x$call<- match.call()
		#Z.return<-matrix(0, N1, G)
		#for(g in 1:G) Z.return[,g]<-rep(Z[,g], counts.n)
		
		x$itemprob<-Thetat
		if(!is.null(colnames(X))) colnames(x$itemprob)<- colnames(X)
		x$classprob<-Taut
		x$Z<-Z
		rownames(x$Z)<- names(counts.n)
		colnames(x$Z)<- paste("Group", 1:G)

		s.e.<- blca.em.se(x,X,counts.n)
		if(se){
			if(any(x$itemprob==0)) warning("Some item probability estimates are exactly zero. Standard errors in this case are undefined.")
			if(any(x$classprob==0)) warning("Some class probability estimates are exactly zero. Standard errors in this case are undefined.")	
			x$itemprob.se<- s.e.$itemprob
			x$classprob.se<- s.e.$classprob		
		} 
		
		x$logpost<-l
		x$BIC<- 2*l-(G*M + G-1)*log(N1)
		x$AIC<- 2*l - 2*(G*M + G-1)
		x$iter<-counter
		x$poststore<-llstore
		x$eps<-eps
		x$counts<- counts.n
		if(counter>iter) convergence<- 3 else convergence<- s.e.$convergence
		if(convergence==2) warning("Some point estimates likely converged at saddle-point.")
		if(convergence==4) print("Some point estimates located at boundary (i.e., are 1 or 0) - standard errors will be undefined in this case.")
		x$convergence<- convergence
		
		x$prior<-NULL
		x$prior$alpha<- alpha
		x$prior$beta<- beta
		x$prior$delta<- delta
		x$small<- small
		if((se==TRUE)&&(is.null(s.e.$classprob))) se<- FALSE
		x$se<- se
		class(x)<-"blca.em"		
		x
		}
