data <- read.csv("adjhours.csv")
attach(data)
count <- matrix(0, nrow = 17379, ncol = 25)
count[,1] <- data$cnt
for(i in 2:25)
{
	for(j in 1:length(data$cnt)) {
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
regfit<-regsubsets(y~.,force.in=c(1:26),nvmax=35,data=newdata.log)
# summary(regfit)
name <- data.frame(c(1:36), colnames(newdata.log))
#### method 1 rollng windows (window size not fixed) weekly fold
valtotal.errors=rep(0,9)
for(i in 1:91)
{
	 train<-newdata.log[1:(168*i),]    
	 test<-newdata.log[((168*i)+1):(168*i+168),]
	 test.mat=model.matrix(y~.,data=test)
	 val.errors=rep(NA,9)
	 for(j in 1:9)
	 {
	    feature <- name[name[,2]==names(coef(regfit,id = j)),1] 
	    sf.data <- train[,c(1,feature)]
	    model<-lm(y~.,data=sf.data)
	    coefi=coef(model)
	    pred=test.mat[,names(coefi)]%*%coefi
	  	val.errors[j]=mean( (exp(test$y)-exp(pred))^2)
	    valtotal.errors[j]=valtotal.errors[j]+val.errors[j]
	 }
}
realmse<-valtotal.errors/91
realmse
###### method 2 rolling windows  (window size fixed) weekly fold
valtotalm2.errors=rep(0,9)
for(i in 1:91)
{
	 train<-newdata.log[168*(i-1)+1:(168*i),]    
	 test<-newdata.log[((168*i)+1):(168*i+168),]
	 test.mat=model.matrix(y~.,data=test)
	 val.errors=rep(NA,9)
	 for(j in 1:9)
	 {
	    feature <- name[name[,2]==names(coef(regfit,id = j)),1] 
	    sf.data <- train[,c(1,feature)]
	    model<-lm(y~.,data=sf.data)
	    coefi=coef(model)
	    pred=test.mat[,names(coefi)]%*%coefi
	  	val.errors[j]=mean( (exp(test$y)-exp(pred))^2)
	    valtotalm2.errors[j]=valtotalm2.errors[j]+val.errors[j]
	 }
}
realmse2<-valtotalm2.errors/91
realmse2
###### method 3 rolling windows (windows size fixed) daily fold
valdfm3.errors=rep(0,9)
for(i in 5:647)
{
	 train<-newdata.log[24*1:(24*i),]    
	 test<-newdata.log[((24*i)+1):(24*i+24),]
	 test.mat=model.matrix(y~.,data=test)
	 val.errors=rep(0,9)
	 for(j in 1:9)
	 {
	    feature <- name[name[,2]==names(coef(regfit,id = j)),1] 
	    sf.data <- train[,c(1,feature)]
	    model<-lm(y~.,data=sf.data)
	    coefi=coef(model)
	    pred=test.mat[,names(coefi)]%*%coefi
	  	val.errors[j]=mean( (exp(test$y)-exp(pred))^2)
	    valdfm3.errors[j]=valdfm3.errors[j]+val.errors[j]
	 }
}
realmse3<-valdfm3.errors/647
realmse3	