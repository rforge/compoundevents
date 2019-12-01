#'@title     Impact  under droughts and hot extremes
#'@description    Use the meta-Gaussian model to construct the condtional distribuiton of
#' the impact variable (Y)  give drought and hot conditions P(Y|P,T)
#'@param P Precipitation or drought indicator corresponding to the impact variable Y
#'@param T Temperature or heat indicator  corresponding to the impact variable Y
#'@param Y Impact variable (e.g., Crop yield)
#'@references Feng, S.et al. (2019). Probabilistic evaluation of the impact of compound dry-hot events on global maize yields. Sci. Total. Environ., 689: 1228-1234.
#'@references Hao, Z.  et al. (2018). A multivariate approach for statistical assessments of compound extremes. J. Hydrol., 565: 87-94.
#'@usage ImpactMG(P,T,Y)
#'@export
#'@examples
#'P=matrix(rnorm(60,0,1),ncol=1)
#'T=matrix(rnorm(60,0,1),ncol=1)
#'Y=matrix(rnorm(60,0,1),ncol=1)
#'ImpactMG(P,T,Y)
#'
ImpactMG<-function(P,T,Y)
{
  # Inputs
  AP=matrix(data=P, ncol = 1)
  AT=matrix(data=T, ncol = 1)
  AY=matrix(data=Y, ncol = 1)

  # Transform all variables into standardized variables

  n=length(AP);

  F1<-ecdf(AP)  # Get the empirical probability
  P1=F1(AP)*n/(n+1)  # Use the Weibull plotting position
  SPI<-qnorm(P1,0,1)

  F2<-ecdf(AT)
  P2=F2(AT)*n/(n+1)
  STI<-qnorm(P2,0,1)

  F3<-ecdf(AY)
  P3=F3(AY)*n/(n+1)
  SCI<-qnorm(P3,0,1)

  # Build the meta-Gaussian model

  xd=cbind(SPI,STI)
  yd=SCI;


  # Speficify the compound dry-hot condition

  u0=c(-1.2,1.2)

  # Get the conditional mean and variance

    cv<-cov(cbind(xd,yd))

    n=3;

    sig12<-cv[1:n-1,n];
    sig21<-cv[n,1:n-1];

    sig12=matrix(data=sig12, nrow=2,ncol = 1)
    sig21=matrix(data=sig21, nrow=1,ncol = 2)


    mx=cbind(mean(xd[,1]),mean(xd[,2]))
    my=mean(yd)


    mu<-my+sig21%*%solve(cov(xd))%*%t((u0-mx))

    sig<-cv[n,n]-sig21%*%solve(cov(xd))%*%t(sig21)



  res<-cbind(mu,sig)

  colnames(res)<-c("Conditional mean","Conditional Variance")

  return (res)
}
