########
########  THE MULTI-THRESHOLD (MTh) ALGORITHM 
########  Algorithm 1 in "Therapeutic Limits and Predictive Risk Models for COVID-19 Patients"
########  by Delgado et. al. 
########  Copyright(c) 2023 Rosario Delgado 
########  email: Rosario.Delgado@uab.cat
########

MTh<-function(M=matrix, c=values, p=values)     
  # input: cost matrix M (r x r)
  #        labels c (vector of dimension r)
  #        distribution p (probability distribution assigned to labels, of dimension r)
  # 
{ 
  omega<-M%*%p # the vector of weights, which must be computed
  q<-p/t(omega) 
  q <- q/sum(q) # adjusted probabilities 

  L<-which(q==max(q)) 
  if (length(L)==1){
    predicted<-c[L]  # if no ties
    } else {predicted<-c[sample(L,1)]}  # if ties
  
  # equivalently (instead of lines 19-22): predicted<-c[sample(which(q==max(q)),1)]

  # output: the predicted category given by the MTh algorithm
  
  return(predicted)
}
  

### Example paper
# 
# M=matrix(c(0,1,1,6,0,3,2,1,0),nrow=3,ncol=3)  # Cost matrix (1) with alpha=6, beta=2
# c<-c("discharge","exitus","icu") # vector of categories or labels
# p<-c(0.5,0.3,0.2) # original probability distribution assigned to labels by classifier
# 
# predicted.priory<-c[sample(which(p==max(p)),1)]
# predicted.priory # predicted category before using MTh algorithm
# 
# predicted.MTh<-MTh(M,c,p)  # predicted category after using MTh algorithm
# predicted.MTh

