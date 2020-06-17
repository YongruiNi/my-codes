### FINAL PROJECT STA 141A
##
##
## Data from: https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
##
## Key Question:
## Who will be likely to subscribe to a term deposit?
##
## Note: 'bank-additiona-full.csv' is much larger than 'bank.csv' 
## so the smaller file (bank.csv) can be used for explorative analysis 
## Three input missing so full set is still helpful

## Libraries


## Read data
df = read.csv("bank-additional-full.csv", sep = ";")
 

## Factors must be converted to numeric if used in k-means
## uncomment for knn to work
## df$contact = as.numeric(df$contact)



## subset data
n = dim(df)[1]
set.seed(12)
train_p=0.2;
train = sample(1:n,train_p*n)
train = sort(train)
test = sort(setdiff(1:n,train))
df.train = df[train,]
df.test = df[test,]


## Who Will likely subscribe to a term deposit??

## Logistic Regression


## Best Logistic Model
## Additionally Codes from STA 138. 
## This file in paritular, "disc8.R":
library(bestglm)


fullModel = glm(y ~.- default, df.train, family=binomial())
nullModel = glm(y ~ 1, df.train, family = binomial())

## Three algorithms for finding model with best fit... similar performance
bestForwardAIC=step(nullModel,scope = list(lower = nullModel, upper = fullModel),direction = "forward")

summary(bestForwardAIC)

## From best forward AIC, removed var. 'default' which was causing problems with predict()
testModel = glm(y ~duration + nr.employed + month + pdays + emp.var.rate + 
                  previous + cons.price.idx +  contact + cons.conf.idx
                , df.train, family = binomial() )

## Test different models to assign to xy.glm 
xy.glm = bestForwardAIC

## Which models have signicant variables?
summary(xy.glm)


xy.glm.pred.prob = predict(xy.glm, df.test,  type = "response")
# Convert predictions to class labels

#########up date
getp=function(mod,testdata){# return the best cut-probility
  error= seq(0.1,0.8,by=0.01)
  p=seq(0.1,0.8,by=0.01)
  preds <- predict(mod, testdata,type="response")
  ln=length(testdata[,1])
  for(i in 1:71){
    pre=rep(0,ln)
    pre[which(preds>p[i])]=1
    error[i]=sum(pre!=testdata$y)
  }
  return(mean(p[which(error==min(error))]))
}
p=getp(xy.glm,data)
xy.glm.pred = (xy.glm.pred.prob > p) 
###############
xy.glm.pred = (xy.glm.pred.prob > 0.5) 
# Create the confusion matrix by tabulating true classes against predicted classes.
xy.glm.conf = table(Subsribed = df.test$y, predicted = xy.glm.pred)


## Clustering 
## From previous cluster analysis and logistic regression
## Are smaller subsets of predictors able to predict subscription? 
library(class)

##
# Use the kNN classifier with k = 4
## Try different predictors. If predictor is factor level,
## convert to numberic for knn() to work
xy.knn = knn(
  train = df.train[c("contact","duration")], # training data for features used in classification
  test = df.test[c("contact","duration")], # test data data for features used in classification
  cl = df.train$y, # vector of class labels for training data
  k = 4)

## true and predicted class lables, togther with features, for the test data
cbind(df.test,xy.knn)

xy.knn.conf = table(true = df.test$y, predicted = xy.knn)

#####clustering
library(clustMixType)
library(MASS)
df=read.csv(file="C:/Users/Administrator.SC-201301200557/Downloads/bank-additional/bank-additional/bank-additional-full.csv", header=TRUE, sep=";")
mod=stepAIC(glm(y~.-cl, data = df, family = binomial))

train=sample(41188,15000,replace = FALSE)
traindata=df[train,]
within=rep(0,7)##find out the n
for(i in 1:7){
  within[i]=kproto(traindata[,1:21], i, nstart = 10,iter.max = 50 )$tot.withinss
  print(i)
  print(within[i])
}

plot(within)

proto=kproto(df[,1:21], 5, nstart = 5,iter.max = 50 )#put cl in to the data
df$cl <- predict(proto, df)$cluster


gone=stepAIC(glm(y~.-cl, data = df[which(df$cl==1),], family = binomial))#model of clustering 1
gtwo=stepAIC(glm(y~.-cl, data = df[which(df$cl==2),], family = binomial))#model of clustering 2
gthree=stepAIC(glm(y~.-cl, data = df[which(df$cl==3),], family = binomial))#model of clustering 3
gfour=stepAIC(glm(y~.-cl, data = df[which(df$cl==4),], family = binomial))#model of clustering 4
gfive=stepAIC(glm(y~.-cl, data = df[which(df$cl==5),], family = binomial))#model of clustering 5

geterror=function(mod,testdata){###calculate the number of error
  error= seq(0.1,0.8,by=0.01) #set error for every cut probility from 0.1 to 0.8, every 0.01
  p=seq(0.1,0.8,by=0.01)#set cut probility as an array for 0.1 to 0.8, every 0.01
  preds <- predict(mod, testdata,type="response") #get the predicted probility
  ln=nrow(testdata) #get the length of test data
  for(i in 1:71){
    pre=rep(1,ln)
    pre[which(preds>p[i])]=2
    error[i]=sum(pre!=as.numeric(testdata$y))# get the number of error for ith p
  }
  prob=p[which(error==min(error))]#get the lowerest error p
  pre2=rep(1,ln)
  pre2[which(preds>prob)]=2
  return(sum(pre2!=as.numeric(testdata$y))) #return number of error
}
error=rep(0,5)
error[1]=geterror(gone,df[which(df$cl==1),])
error[2]=geterror(gtwo,df[which(df$cl==2),])
error[3]=geterror(gthree,df[which(df$cl==3),])
error[4]=geterror(gfour,df[which(df$cl==4),])
error[5]=geterror(gfive,df[which(df$cl==5),])
sum(error)## total number of error of 5 models
geterror(mod,df) #total number of error without cluthing

ggplot(df, aes(x=as.factor(cl), y=age)) + 
  geom_violin(trim=FALSE)+ggtitle("age vs group")
ggplot(df, aes(x=as.factor(cl), fill=job)) + 
  geom_bar(position = "fill")+ggtitle("job vs group")
ggplot(df, aes(x=as.factor(cl), fill=default)) + 
  geom_bar(position = "fill")+ggtitle("groupvs defult")
ggplot(df, aes(x=as.factor(cl), fill=contact)) + 
  geom_bar(position = "fill")+ggtitle("group vs contact")
ggplot(df, aes(x=as.factor(cl), fill=month)) + 
  geom_bar(position = "fill")+ggtitle("group vs month")
ggplot(df, aes(x=as.factor(cl), fill=day_of_week)) + 
  geom_bar(position = "fill")+ggtitle("group vs day of week")
ggplot(df, aes(x=as.factor(cl), duration)) + 
  geom_boxplot()+ggtitle("group vs duration")
ggplot(df, aes(x=as.factor(cl), campaign)) + 
  geom_boxplot()+ggtitle("group vs campaign")+ylim(0,10)
ggplot(df, aes(x=as.factor(cl), fill=poutcome)) + 
  geom_bar(position = "fill")+ggtitle("group vs poutcome")
ggplot(df, aes(x=as.factor(cl), fill=education)) + 
  geom_bar(position = "fill")+ggtitle("group vs education")