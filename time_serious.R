
library(car)
chicago<-read.delim("C:/Users/Administrator.SC-201301200557/Desktop/chicago.txt", header = FALSE, sep = "\t")
y=chicago$V1
ts.plot(y)
#################3
lambdas <- seq(-2, 2, 0.1)
s=7
deg=4
mod <- trndseas(y, s, lambdas, 4)
ts.plot(y ^ mod$lamopt)
lines(mod$trend, col = "red")
################4
mod$lamopt
plot(mod$trend)
plot(mod$season)
plot(mod$residual)
chicago$res=mod$residual
###################6
qqnorm(chicago$res, pch = 1, frame = FALSE)
qqline(chicago$res, col = "steelblue", lwd = 2)
acf(mod$residual)
pacf(mod$residual)
####################7
library(forecast)
fitarma<-auto.arima(mod$residual)
fitarma
mod1=arima(mod$residual,order=c(1,0,0))
acf(mod1$residuals)
arma.spec(ar=0.422, log='no', main='Spectral Density')
spec.pgram(x, log="no", spans=50)
####################8
attach(mtcars)
par(mfrow=c(1,2))
arma.spec(ar=0.422, log='no', main='Spectral Density')
spec.pgram(mod$residual, log="no", spans=80)
#####################9
predit_trand=function(x){
  index=length(x)
  d1=x[index]-x[index-1]
  d2=x[index-1]-x[index-2]
  d3=d1-d2
  return(x[index]+d1+d3)
}
x=mod$trend[97:99]
x[4]=predit_trand(x)
x[5]=predit_trand(x)
x[6]=predit_trand(x)
x[7]=predit_trand(x)
x[8]=predit_trand(x)
x[9]=predit_trand(x)
x[10]=predit_trand(x)
predic=rep(0,7)
predic[1]=x[4]+mod$season[2]+mod$residual[99]*0.422
predic[2]=x[5]+mod$season[3]
predic[3]=x[6]+mod$season[4]
predic[4]=x[7]+mod$season[5]
predic[5]=x[8]+mod$season[6]
predic[6]=x[9]+mod$season[7]
predic[7]=x[10]+mod$season[1]
predic=predic^10
ts.plot(predic,type = "l", col = "red")
ts.plot(predic,type = "l", col = "red")
lines(chicago$V1[100:106],type = "l", col = "red")
##############arma.spec function#######################################################################
arma.spec <-
  function(ar=0,ma=0,var.noise=1,n.freq=500,  ...)
  { 
    plot = graphics::plot     
    check <- 0
    # check causality
    ar.poly <- c(1, -ar)
    z.ar <- base::polyroot(ar.poly)
    if(any(abs(z.ar) <= 1)) {cat("WARNING: Model Not Causal", "\n"); check <- check + 1}  
    # check invertibility
    ma.poly <- c(1, ma)
    z.ma <- base::polyroot(ma.poly)
    if(any(abs(z.ma) <= 1)) {cat("WARNING: Model Not Invertible", "\n"); check <- check + 1}
    if (check > 0) stop("Try Again")
    #
    ar.order <- length(ar)
    ma.order <- length(ma) 
    # check (near) parameter redundancy [i.e. are any roots (approximately) equal]  
    for (i in 1:ar.order) {
      if ( (ar == 0 & ar.order == 1) || (ma == 0 & ma.order ==1) ) break
      if(any(abs(z.ar[i]-z.ma[1:ma.order]) < 1e-03)) {cat("WARNING: Parameter Redundancy", "\n"); break}
    }
    #
    freq <- seq.int(0, 0.5, length.out = n.freq)
    cs.ar <- outer(freq, 1:ar.order, function(x, y) cos(2 * pi * x * y)) %*% ar
    sn.ar <- outer(freq, 1:ar.order, function(x, y) sin(2 * pi * x * y)) %*% ar
    cs.ma <- outer(freq, 1:ma.order, function(x, y) cos(2 * pi * x * y)) %*% -ma
    sn.ma <- outer(freq, 1:ma.order, function(x, y) sin(2 * pi * x * y)) %*% -ma                      
    spec <- var.noise*((1 - cs.ma)^2 + sn.ma^2)/((1 - cs.ar)^2 + sn.ar^2)
    spg.out <- list(freq=freq, spec=spec)
    class(spg.out) <- "spec"
    plot(spg.out, ci=0, ...)
    return(invisible(spg.out))
  }
#############trndsea function###########################################################################
trndseas=function(y,seas,lam,degtrnd){
  
  m=length(lam)
  n=length(y)
  
  # Part of design matrix for estimating trend
  if(degtrnd>0) {
    tm=seq(1/n,1,by=1/n)
    x1=poly(tm,degree=degtrnd,raw=TRUE)
    x1=cbind(rep(1,n),x1)
  } else {
    x1=as.matrix(rep(1,n),ncol=1)
  }
  
  # Part of design matrix for estimating seasonality
  x2=NULL
  if(seas>1){
    sn=rep(1:seas,length.out=n)
    x2=factor(sn,levels=unique(sn),ordered=TRUE)
    x2=model.matrix(~x2-1)
    m2=ncol(x2)
    m21=m2-1
    x2=x2[,1:m21]-matrix(rep(x2[,m2],m21),ncol=m21,nrow=nrow(x2),byrow=F)
  }
  
  x=cbind(x1,x2)  # design matrix
  
  xx=t(x)%*%x
  rsq=rep(1,m)
  m1=ncol(x1)     #degtrnd+1
  m11=m1+1
  mx=ncol(x)      # degtrnd+1+seas-1
  
  for(i in 1:m) {
    if (lam[i]==0) {
      yt=log(y)
    } else {
      yt=y^lam[i]
    }
    xy=t(x)%*%yt
    coef=solve(xx,xy)
    fit=x%*%coef
    res=yt-fit
    ssto=(n-1)*var(yt)
    sse=t(res)%*%res
    rsq[i]=1-((n-1)/(n-mx))*sse/ssto
  }
  
  ii=which.max(rsq)
  lamopt=lam[ii]
  if (lamopt==0) {
    yt=log(y)
  } else {
    yt=y^lamopt
  }
  xy=t(x)%*%yt
  coef=solve(xx,xy)
  fit=x%*%coef
  trnd=x1%*%coef[1:m1]
  season=NULL
  if(seas>1){
    season=c(coef[m11:mx],-sum(coef[m11:mx]))
  }
  res=yt-fit
  
  result=list(coef=coef,fitted=fit,trend=trnd,residual=res,season=season,rsq=rsq,lamopt=lamopt)
  return(result)
}