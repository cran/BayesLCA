blca.em.se<-
function(fit, x, counts.n=1)#, grad.check=FALSE)
{
if(class(x) == "data.blca"){
counts.n<- x$counts.n
x<- x$data
}
G<- length(fit$classprob)
M<- ncol(fit$itemprob)

rm.ind<- c(which(fit$itemprob< 1e-5), which(fit$itemprob>(1 - 1e-5)))
val.ind<- fit$itemprob[rm.ind]

if(length(rm.ind)>0) parvec<- c(fit$itemprob[-rm.ind], fit$classprob[-G]) else parvec<- c(fit$itemprob, fit$classprob[-G])
H1<- fdHess(pars=parvec,fun=lp1.intern, dat=x,counts.n=counts.n, rm.ind=rm.ind, prior.list=fit$prior, val.ind=val.ind, G0=G, M0=M)

if(length(rm.ind)>0) parvec<- c(fit$itemprob[-rm.ind], fit$classprob[-1]) else parvec<- c(fit$itemprob, fit$classprob[-1])
H2<- fdHess(pars=parvec,fun=lp2.intern, dat=x, counts.n=counts.n,rm.ind=rm.ind, prior.list=fit$prior, val.ind=val.ind, G0=G, M0=M)

SE1<- sqrt(diag(solve(-H1$Hessian)))
SE2<- sqrt(diag(solve(-H2$Hessian)))

if(all(eigen(H1$Hessian, symmetric=TRUE, only.values=TRUE)$values<0)) convergence<- 1 else convergence<- 2

len.parvec.theta<- G*M - length(rm.ind)
se.theta<- rep(0, G*M)
if(length(rm.ind)>0) se.theta[-c(rm.ind)]<- SE1[1:len.parvec.theta] else se.theta<- SE1[1:len.parvec.theta]
se.theta[rm.ind]<- 0

ret<- NULL
ret$itemprob<- matrix(se.theta, G, M)
ret$classprob<- c( SE1[(len.parvec.theta + 1):(len.parvec.theta + G-1)], SE2[len.parvec.theta +G-1]   )
ret$convergence<- convergence

# if(grad.check){
#   if(all(H1$gradient< 1e-6)){
#   print("TRUE")
#   ret$grad<- H1$gradient
#   ret$grad.check<- TRUE
#   } else{
#   ret$grad<- H1$gradient
#   ret$grad.check<- FALSE
# }
# }
ret
}
