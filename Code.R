data <- read.csv("adjhours.csv")
attach(data)
count <- matrix(0, nrow = 17379, ncol = 25)
count[,1] <- data$cnt
for(i in 2:25)
{
	for(j in 1:length(data$cnt))
	{
		if((j+1) > 17379)
			break
		count[j,i] <- count[j+1, i-1]
	}
}
count <- as.data.frame(count)
temp2 <- data[,35]^2
hum2 <- data[,37]^2
newdata <- data.frame(count[,-1], data[,c(43,35:36,38:39)], temp2, hum2, data[,20:23])
newdata <- newdata[1808:17379,]
newdata.log<-log(1+newdata)
y<-log(1+cnt)[1808:17379]
newdata.log<-cbind(y,newdata.log)
library(leaps)
library(boot)
# regfit<-regsubsets(y~.,force.in=c(1:26),nvmax=35,data=newdata.log)
# summary(regfit)
valtotal.errors=rep(0,92)
for(i in 1:91)
{
	train<-newdata.log[1:(168*i),]    
	test<-newdata.log[((168*i)+1):(168*i+168),]
	regfit<-regsubsets(y~.,force.in=c(1:26),nvmax=35,data=train)
	test.mat=model.matrix(y~.,data=test)
	val.errors=rep(NA,9)
		for(j in 1:9)
		{
		   coefi=coef(regfit,id=j)
		   pred=test.mat[,names(coefi)]%*%coefi
		   val.errors[j]=mean( ( exp(test$y) - exp(pred) ) ^2)
		}
	valtotal.errors[j]=val.errors[which.min(val.errors)]*10+which.min(val.errors)
}
which.min(valtotal.errors)
valtotal.errors[which.min(valtotal.errors)]
