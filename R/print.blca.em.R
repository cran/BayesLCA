print.blca.em <-
function(x, ...){
z<-NULL
z$se<-NULL

btau<- x$classprob
btheta<- x$itemprob

names(btau)<- paste("Group", 1:length(btau))
if(is.null(colnames(btheta))) colnames(btheta)<- paste("Col", 1:ncol(btheta))
rownames(btheta)<- paste("Group", 1:nrow(btheta))

z$ItemProb<-btheta
z$MembershipProb<-btau

if(x$se){
z$se$ItemProb<- x$itemprob.se
z$se$MembershipProb<- x$classprob.se

names(z$se$MembershipProb)<- paste("Group", 1:length(btau))
if(is.null(colnames(z$se$ItemProb))) colnames(z$se$ItemProb)<- paste("Col", 1:ncol(btheta))
rownames(z$se$ItemProb)<- paste("Group", 1:nrow(z$se$ItemProb))
#warning("blca.em standard errors returned, but may be inaccurate. See blca.bootstrap for more reliable estimates.")
} #else{
#z$se$ItemProb<- NULL
#z$se$MembershipProb<- NULL
#warning("blca.em standard errors not returned. See blca.bootstrap for more reliable estimates.")
#}
#class(z)<-"summary.blca.em"
#return(z)
	cat("\nMAP Estimates:\n", "\n")
	cat("\nItem Probabilities:\n", "\n")
	print(round(z$ItemProb,3))
	
	cat("\nMembership Probabilities:\n", "\n")
	print(round(z$MembershipProb,3))
	
if(x$se){
	cat("\nStandard Error Estimates:\n", "\n")
	cat("\nItem Probabilities:\n", "\n")
	print(round(z$se$ItemProb, 3))
	
	cat("\nMembership Probabilities:\n", "\n")
	print(round(z$se$MembershipProb, 3))
} else warning("Standard errors not returned.", call.=FALSE)
}
