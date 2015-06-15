setwd("C:/Users/user/Desktop/MSLFP")
data <- read.csv("adjhours.csv")
attach(data)
regmodel<-rep(0,10)
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
c10unt <- as.data.frame(count)
temp2 <- data[,35]^2
hum2 <- data[,37]^2
newdata <- data.frame(count[,-1], data[,c(43,35:36,38:39)], temp2, hum2, data[,20:23])
newdata <- newdata[1808:17379,]
newdata.log<-log(1+newdata)
y<-log(1+cnt)[1808:17379]
newdata.log<-cbind(y,newdata.log)
library(leaps)
library(boot)
regfit<-regsubsets(y~.,force.in=c(1:25),nvmax=35,data=newdata.log)
Sumofreg<-summary(regfit)
regmodel[1]<-which.min(Sumofreg$cp)
regmodel[2]<-which.min(Sumofreg$bic)
regmodel[3]<-which.min(Sumofreg$adjr2)
name <- data.frame(c(1:36), colnames(newdata.log))
#### method 4 rollng windows (window size not fixed) weekly fold
valtotal.errors=rep(0,10)
for(i in 1:91)
{
	 train<-newdata.log[1:(168*i),]    
	 test<-newdata.log[((168*i)+1):(168*i+168),]
	 test.mat=model.matrix(y~.,data=test)
	 val.errors=rep(NA,10)
	 for(j in 1:10)
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
regmodel[4]<-which.min(realmse)
###### method 5 rolling windows  (window size fixed) weekly fold
valtotalm2.errors=rep(0,10)
for(i in 1:91)
{
	 train<-newdata.log[168*(i-1)+1:(168*i),]    
	 test<-newdata.log[((168*i)+1):(168*i+168),]
	 test.mat=model.matrix(y~.,data=test)
	 val.errors=rep(NA,10)
	 for(j in 1:10)
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
regmodel[5]<-which.min(realmse2)
###### method 6 rolling windows (windows size no fixed) daily fold
valdfm3.errors=rep(0,10)
for(i in 5:647)
{
	 train<-newdata.log[24*1:(24*i),]    
	 test<-newdata.log[((24*i)+1):(24*i+24),]
	 test.mat=model.matrix(y~.,data=test)
	 val.errors=rep(0,10)
	 for(j in 1:10)
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
realmse3<-valdfm3.errors/643
regmodel[6]<-which.min(realmse3)
###### method 7  none fixed window size mothly fold
valfxmthfd.errors=rep(0,10)
for(i in 1:20)
{
	 train<-newdata.log[1:(720*i),]    
	 test<-newdata.log[((720*i)+1):(720*i+720),]
	 test.mat=model.matrix(y~.,data=test)
	 val.errors=rep(NA,10)
	 for(j in 1:10)
	 {
	    feature <- name[name[,2]==names(coef(regfit,id = j)),1] 
	    sf.data <- train[,c(1,feature)]
	    model<-lm(y~.,data=sf.data)
	    coefi=coef(model)
	    pred=test.mat[,names(coefi)]%*%coefi
	  	val.errors[j]=mean( (exp(test$y)-exp(pred))^2)
	    valfxmthfd.errors[j]=valfxmthfd.errors[j]+val.errors[j]
	 }
}
realmse4<-valfxmthfd.errors/20
regmodel[7]<-which.min(realmse4)
###### method 8 rolling windows (windows size fixed) mothly fold
valmtlyfold.errors=rep(0,10)
for(i in 1:20)
{
	 train<-newdata.log[720*(i-1)+1:(720*i),]    
	 test<-newdata.log[((720*i)+1):(720*i+720),]
	 test.mat=model.matrix(y~.,data=test)
	 val.errors=rep(NA,10)
	 for(j in 1:10)
	 {
	    feature <- name[name[,2]==names(coef(regfit,id = j)),1] 
	    sf.data <- train[,c(1,feature)]
	    model<-lm(y~.,data=sf.data)
	    coefi=coef(model)
	    pred=test.mat[,names(coefi)]%*%coefi
	  	val.errors[j]=mean( (exp(test$y)-exp(pred))^2)
	    valmtlyfold.errors[j]=valmtlyfold.errors[j]+val.errors[j]
	 }
}
realmse5<-valmtlyfold.errors/20
regmodel[8]<-which.min(realmse5)
##### Two week non fixed
valfxhmth.errors=rep(0,10)
for(i in 1:40)
{
	 train<-newdata.log[1:(360*i),]    
	 test<-newdata.log[((360*i)+1):(360*i+360),]
	 test.mat=model.matrix(y~.,data=test)
	 val.errors=rep(NA,10)
	 for(j in 1:10)
	 {
	    feature <- name[name[,2]==names(coef(regfit,id = j)),1] 
	    sf.data <- train[,c(1,feature)]
	    model<-lm(y~.,data=sf.data)
	    coefi=coef(model)
	    pred=test.mat[,names(coefi)]%*%coefi
	  	val.errors[j]=mean( (exp(test$y)-exp(pred))^2)
	    valfxhmth.errors[j]=valfxhmth.errors[j]+val.errors[j]
	 }
}
realmse6<-valfxhmth.errors/40
regmodel[9]<-which.min(realmse6)
###### method 10 rolling windows (windows size fixed) 2week fold
valhmthfd.errors=rep(0,10)
for(i in 1:40)
{
	 train<-newdata.log[360*(i-1)+1:(360*i),]    
	 test<-newdata.log[((360*i)+1):(360*i+360),]
	 test.mat=model.matrix(y~.,data=test)
	 val.errors=rep(NA,10)
	 for(j in 1:10)
	 {
	    feature <- name[name[,2]==names(coef(regfit,id = j)),1] 
	    sf.data <- train[,c(1,feature)]
	    model<-lm(y~.,data=sf.data)
	    coefi=coef(model)
	    pred=test.mat[,names(coefi)]%*%coefi
	  	val.errors[j]=mean( (exp(test$y)-exp(pred))^2)
	    valhmthfd.errors[j]=valhmthfd.errors[j]+val.errors[j]
	 }
}
realmse7<-valhmthfd.errors/40
regmodel[10]<-which.min(realmse7)
regmodel
realmse[regmodel[4]]
realmse2[regmodel[5]]
realmse3[regmodel[6]]
realmse4[regmodel[7]]
realmse5[regmodel[8]]
realmse6[regmodel[9]]
realmse7[regmodel[10]]

