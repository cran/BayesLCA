blca.em.se <-
function(fit, X, counts.n){
	
	Z<- fit$Z
	tau<- fit$classprob
	theta<- fit$itemprob
	G<- length(tau)
	M<- ncol(theta)
	H00<- matrix(0, G*M+G, G*M+G)
	f<- function(X, theta)	apply(dbinom(t(as.matrix(X)), 1, prob=theta),2, prod)
	
	if(any(theta==0)|any(theta==1)){
	se<- NULL
	se$itemprob<- NULL
	se$classprob<- NULL
	se$convergence<- 4
	return(se)
	}
	
	if(any(tau==0)){
	se<- NULL
	se$itemprob<- NULL
	se$classprob<- NULL
	se$convergence<- 4
	return(se)
	}
	
	##For Tau-Tau term
	Hdenom<- 0
	for(g in 1:G) Hdenom<- Hdenom + tau[g]*f(X, theta[g,])
	Hdenom<- Hdenom^2
	
	for(g1 in 1:G){
		for(g2 in 1:G){
			
			#Theta-Theta	
			Amat<-  (t(counts.n*t(t(X) - theta[g2, ])*(as.numeric(g1==g2) - Z[,g2]))/(theta[g2, ]*(1-theta[g2, ])))%*%t(t(t(t(X) - theta[g1, ])*Z[, g1])/(theta[g1, ]*(1-theta[g1, ])))
			
			diag(Amat)<- diag((t(counts.n*t(t(X) - theta[g2, ])*( - Z[,g2]))/(theta[g2, ]*(1-theta[g2, ])))%*%t(t(t(t(X) - theta[g1, ])*Z[, g1])/(theta[g1, ]*(1-theta[g1, ]))))
			
			H00[(g1-1)*M + 1:M ,(g2-1)*M + 1:M]<- Amat	
			
			##Tau-Theta
			H00[g1 + G*M, (g2-1)*M + 1:M ]<- H00[(g2-1)*M + 1:M, g1 + G*M ]<- colSums(counts.n*t(t(X) - theta[g2, ])*Z[,g2]*(as.numeric(g1==g2)-Z[,g1]))
			
			##Tau-Tau
			H00[g1+G*M,g2+G*M]<- -sum(counts.n*f(X, theta[g1, ])*f(X, theta[g2, ])/Hdenom)
			}
		}
		
	
	e1<- eigen(H00)
	if(any(e1$values>0)){
	se<- NULL
	se$itemprob<- NULL
	se$classprob<- NULL
	se$convergence<- 2
	return(se)
	} else convergence<- 1
	

	
	H11<- H00[-(G*M+G), -(G*M+G)]
	H22<- H00[-(G*M+1), -(G*M+1)]
	
	# H11[G*M+(1:(G-1)), G*M+(1:(G-1))]<- (t(counts.n*(Z-Z[,G])/tau[-G])%*%((Z-Z[,G])/tau[-G]))[-G,-G]
	# H22[G*M+(1:(G-1)), G*M+(1:(G-1))]<- (t(counts.n*(Z-Z[,1])/tau[-1])%*%((Z-Z[,1])/tau[-1] ))[-1,-1]
	
 
	SE1<- sqrt(diag(solve(-H11)))
	SE2<- sqrt(diag(solve(-H22))) 
	
	#SE<- sqrt(diag(solve(-H00)))
	se<- NULL
	se$itemprob<- t(matrix(SE1[1:(G*M)], M,G))
	se$classprob<- c(SE1[G*M+1:(G-1)], SE2[G*M+G-1])
	se$convergence<- convergence
	se
	}
