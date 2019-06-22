
### Lecture 2

# population vs sample

source('addTrans.R')

N = 5000
g = 25
x = rep(1:g,each=N/g)
y = rnorm(N,mean=3*x,sd=5)

n = 60
ind = sample(1:N,n)
x1 = x[ind]
y1 = y[ind]

par(mfrow=c(1,2))
plot(x,y,pch=19,col=addTrans('black',5),main='Population',xlab='X',ylab='Y')
abline(0,3,col='red',lwd=2,lty=1)
plot(x1,y1,pch=19,col=addTrans('black',255),main='Sample',xlab='X',ylab='Y')
abline(lm(y1~x1)$coef,col='red',lwd=2,lty=2)
par(mfrow=c(1,1))
dev.print(pdf,'lec2plot1.pdf')

# data for example
set.seed(303)
samp=sample(1:N,30)
xs=round(x[samp],0)
ys=round(y[samp],0)

mean(xs)
mean(ys)
sum((xs-mean(xs))^2)
sum((ys-mean(ys))^2)
sum((xs-mean(xs))*(ys-mean(ys)))
plot(xs,ys,xlab='X',ylab='Y',pch=19)
dev.print(pdf,'lec2plot2.pdf')

lm(ys~xs)



### Lecture 3

# different samples

N = 5000
g = 25
x = rep(1:g,each=N/g)
y = rnorm(N,mean=3*x,sd=5)

n = 60
ind1 = sample(1:N,n)
x1 = x[ind1]
y1 = y[ind1]
ind2 = sample(1:N,n)
x2 = x[ind2]
y2 = y[ind2]
ind3 = sample(1:N,n)
x3 = x[ind3]
y3 = y[ind3]
ind4 = sample(1:N,n)
x4 = x[ind4]
y4 = y[ind4]

par(mfrow=c(2,2))
mod=lm(y1~x1)
coef=lm(mod)$coef
title=paste("Sample 1 \n","b1 =",round(coef[2],2),"b0 =",round(coef[1],2),"MSE =",round(sum(mod$res^2)/(n-2),2))
plot(x1,y1,pch=19,main=title,xlab='X',ylab='Y')
abline(coef,col='red',lwd=1,lty=1)

mod=lm(y2~x2)
coef=lm(mod)$coef
title=paste("Sample 2 \n","b1 =",round(coef[2],2),"b0 =",round(coef[1],2),"MSE =",round(sum(mod$res^2)/(n-2),2))
plot(x2,y2,pch=19,main=title,xlab='X',ylab='Y')
abline(coef,col='red',lwd=1,lty=1)

mod=lm(y3~x3)
coef=lm(mod)$coef
title=paste("Sample 3 \n","b1 =",round(coef[2],2),"b0 =",round(coef[1],2),"MSE =",round(sum(mod$res^2)/(n-2),2))
plot(x3,y3,pch=19,main=title,xlab='X',ylab='Y')
abline(coef,col='red',lwd=1,lty=1)

mod=lm(y4~x4)
coef=lm(mod)$coef
title=paste("Sample 4 \n","b1 =",round(coef[2],2),"b0 =",round(coef[1],2),"MSE =",round(sum(mod$res^2)/(n-2),2))
plot(x4,y4,pch=19,main=title,xlab='X',ylab='Y')
abline(coef,col='red',lwd=1,lty=1)

dev.print(pdf,'lec3plot1.pdf')

# data for example
set.seed(303)
samp=sample(1:N,30)
xs=round(x[samp],0)
ys=round(y[samp],0)

xbar=13.57
ybar=40.77
sxx=1545.37
syy=14495.37
sxy=4647
n=30
b1=3.01
b0=-0.038

#a
sse=syy-b1^2*sxx
sse
mse = sse/(30-2)
mse

#b
sY = (1/30+(20-xbar)^2/sxx)*mse
sY

#c 
sb1 = (1/30+xbar^2/sxx)*mse
sb1

#d
qt(0.975,df=28)
b1 + c(-1,1) * sqrt(sb1) * qt(0.975,df=28)

#e
Yhat=b0+b1*20
qt(0.95,df=28)
spred=sY+mse
spredmean=sY+mse/6
Yhat + c(-1,1) * sqrt(spred) * qt(0.95,df=28)
Yhat + c(-1,1) * sqrt(spredmean) * qt(0.95,df=28)



### Lecture 5

N = 5000
g = 25
x = rep(1:g,each=N/g)
y = rnorm(N,mean=3*x,sd=5)

n = 60
ind1 = sample(1:N,n)
x1 = x[ind1]
y1 = y[ind1]

coef = lm(y1~x1)
ybar = mean(y1)
yhat = lm(y1~x1)$fitted

par(mfrow=c(1,1))
plot(x1,y1,xlab='X',ylab='Y',pch=19)
abline(coef,col='red')
abline(h=ybar)
legend(x='bottomright',legend=c('sample mean','LS regression'),col=c('black','red'),lty=1)
dev.print(pdf,'lec5plot1.pdf')

par(mfrow=c(1,3))

plot(x1,y1,xlab='X',ylab='Y',pch=19, main='SSTO')
abline(h=ybar)
segments(x0=x1,y0=ybar,x1=x1,y1=y1)

plot(x1,y1,xlab='X',ylab='Y',pch=19, main='SSR')
abline(h=ybar)
abline(coef)
segments(x0=x1,y0=ybar,x1=x1,y1=yhat)

plot(x1,y1,xlab='X',ylab='Y',pch=19, main='SSE')
abline(h=ybar)
abline(coef)
segments(x0=x1,y0=y1,x1=x1,y1=yhat)

dev.print(pdf,'lec5plot2.pdf')


# lecture 8
# diagnostics

set.seed(3)

# Linearity
N = 5000
g = 10
x = rep(1:g,each=N/g)
y = rnorm(N,mean=x^2,sd=2)+7
n = 30
ind = sample(1:N,n)
x1 = x[ind]
y1 = y[ind]
par(mfrow=c(1,3))
plot(x1,y1,pch=19,xlab='X',ylab='Y',main='Original')
abline(lm(y1~x1)$coef)
plot(x1,sqrt(y1),pch=19,xlab='X',ylab='sqrt(Y)',main='Transformation 1')
abline(lm(sqrt(y1)~x1)$coef)
plot(x1,log(y1),pch=19,xlab='X',ylab='ln(Y)',main='Transformation 2')
abline(lm(log(y1)~x1)$coef)
dev.print(pdf,'lec8_Linearity.pdf')


# Independence
par(mfrow=c(1,3))
res = rnorm(30,sd=1)
ts.plot(res,ylab='Residual',main='Independent')
abline(h=0)
res = (c(1:18,16:5))/5+rnorm(30,sd=0.4)
res = res-mean(res)
ts.plot(res,ylab='Residual',main='Not Independent')
abline(h=0)
res = (1:30)/5+rnorm(30,sd=0.4)
res = res-mean(res)
ts.plot(res,ylab='Residual',main='Not Independent')
abline(h=0)
par(mfrow=c(1,1))
dev.print(pdf,'lec8_Independence.pdf')



# Heteroscedasticity
par(mfrow=c(2,3))
layout(mat=matrix(1:6,nrow=2,byrow=F))
res = rnorm(63)
plot(res,pch=19,ylab='Residuals',xlab='X',main='Homoscedastic')
abline(h=0)
plot(abs(res),pch=19,ylab='Absolute Residuals',xlab='X',main='Homoscedastic')
res = rnorm(60,sd=rep(1:6,each=10))
plot(res,pch=19,xlab='X',ylab='Residuals',main='Heteroscedastic')
abline(h=0)
plot(abs(res),pch=19,xlab='X',ylab='Absolute Residuals',main='Heteroscedastic')
res = rnorm(60,sd=rep(c(1,3,2,0.5,1),times=c(10,20,5,10,15)))
plot(res,pch=19,ylab='Residuals',xlab='X',main='Heteroscedastic')
abline(h=0)
plot(abs(res),pch=19,ylab='Absolute Residuals',xlab='X',main='Heteroscedastic')
dev.print(pdf,'lec8_Homoscedasticity.pdf')


# Normality
par(mfrow=c(2,3))
layout(mat=matrix(1:6,nrow=2,byrow=F))
res = rnorm(63)
qqnorm(res,pch=19,main='Normal')
qqline(res)
hist(res,main='Normal')
res = rgamma(63,2,27)
res = res-mean(res)
qqnorm(res,pch=19,main='Right-Skewed')
qqline(res)
hist(res,main='Right-Skewed')
res = -rgamma(63,2,27)
res = res-mean(res)
qqnorm(res,pch=19,main='Left-Skewed')
qqline(res)
hist(res,main='Left-Skewed')
par(mfrow=c(1,1))
dev.print(pdf,'lec8_Normality.pdf')

set.seed(3)
# Outlier
N = 5000
g = 10
x = rep(1:g,each=N/g)
y = rnorm(N,mean=3*x,sd=5)
n = 17
ind = sample(1:N,n)
x1 = x[ind]
y1 = y[ind]
x1[1] = 9
y1[1] = 3*x1[1]+60
par(mfrow=c(1,2))
plot(x1,y1,pch=19,xlab='X',ylab='Y',main='With Outlier')
abline(lm(y1~x1)$coef)
plot(x1,y1,pch=19,col=rep(c('white','black'),times=c(1,29)),xlab='X',ylab='Y',main='Without Outlier')
abline(lm(y1[-1]~x1[-1])$coef)
dev.print(pdf,'lec8_Outlier.pdf')

# lecture 18

#dose/growth data
y = c(73,78,85,88,90,91,87,86,91,75,65,63)
mean(y)
var(y)
x = c(10,10,15,15,20,20,25,25,25,30,35,35)
mean(x)

plot(x,y,pch=18,xlab='Dose of Supplement',ylab='Growth Rate')
dev.print(pdf,'lec18_Scatter.pdf',width=7)


