#dec520Q-B-team26finalproject: Customer Lifetime Value Analysis
#Ziyi (Summer) Fu, Chunyu (Sarah) Ji, Aastha Thakkar, Ning Xu, Yifan Zhou

library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("ElemStatLearn")
library(ElemStatLearn)
install.packages("class")
library(class)
library(RColorBrewer) 
install.packages("magrittr")
library(magrittr)
library(neuralnet)
install.packages("caret")
library(caret)
library(data.table)
library(mltools)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
install.packages("e1071")
install.packages("rpart")
library(e1071)
library(rpart)
install.packages("keras")
library(keras)
install_keras()
install_keras(method="conda")
install.packages("ModelMetrics")
library(ModelMetrics)
install.packages("glmnet")
library(glmnet)
install.packages("ltm")
library(ltm)
library(MASS)
library(magrittr)



auto <- read.csv('auto.csv')
summary(auto)

###########################Data Understanding and Exploration###############################
############################################################################################
# to make sure there is no NULL value
sum(is.na(auto))

# we find normal distribution after log the Customer Lifetime Value
ggplot(aes(Customer.Lifetime.Value),data=auto)+
  geom_histogram(fill = "light blue", colour = "black",binwidth = 5000)+
  xlim(0,55000)+
  ggtitle('Customer Lifetime Value Distribution')+xlab('Customer Lifetime Value')+
  theme(plot.title = element_text(size = 16, face = "bold"),legend.title = element_text(size = 16), text = element_text(size=16))

ggplot(aes(log(Customer.Lifetime.Value)),data=auto)+
  geom_histogram(fill = "light blue", colour = "black")+
  ggtitle('Customer Lifetime Value Distribution (logged)')+
  xlab('Customer Lifetime Value')+
  theme(plot.title = element_text(size = 16, face = "bold"),legend.title = element_text(size = 16), text = element_text(size=16))

#we noticed that a quarter of observations have 0 income, and it turns out exactly to be those Unemployed
sum(auto$Income == 0)/nrow(auto)
sum((auto$Income == 0)&(auto$EmploymentStatus=="Unemployed"))
sum((auto$Income == 0)&(auto$EmploymentStatus!="Unemployed"))
auto %>% 
  ggplot()+geom_boxplot(aes(EmploymentStatus,y=Income, color = EmploymentStatus))+
  theme(plot.title = element_text(size = 20, face = "bold"),legend.title = element_text(size = 20), text = element_text(size=20))+
  ggtitle('Relationship between Income and Employment Status')

# explore the relation between Response and Customer.Lifetime.Value
# the point-biserial correlation < 0.2, meaning they are independent
# so we will not take Response into account
install.packages('ltm')
library(ltm)
biserial.cor(auto$Customer.Lifetime.Value, auto$Response, level = 1)
auto %>% 
  ggplot()+geom_boxplot(aes(Response,y=log(Customer.Lifetime.Value), color = Response))+
  theme(plot.title = element_text(size = 16, face = "bold"),legend.title = element_text(size = 14), text = element_text(size=14))+
  ggtitle('Response and Customer Lifetime Value')+xlab('Response')+ylab('Customer Lifetime Value')+labs(color = "Response")+ scale_color_brewer(palette="Blues")

#Vehicle.Class vs Customer.Lifetime.Value
ggplot(auto,aes(x=as.factor(Vehicle.Class),y=log(Customer.Lifetime.Value), color = as.factor(Vehicle.Class)))+geom_boxplot()+
  theme(plot.title = element_text(size = 16, face = "bold"),legend.title = element_text(size = 14), text = element_text(size=14))+
  ggtitle('Vehicle Class and Customer Lifetime Value')+xlab('Vehicle Class')+ylab('Customer Lifetime Value')+ scale_color_brewer(palette="Blues")+labs(color = "Vehicle Class")

#CLV and gender
auto %>% 
  ggplot()+geom_boxplot(aes(as.factor(Gender),y=log(Customer.Lifetime.Value), color = as.factor(Gender)))+
  theme(plot.title = element_text(size = 16, face = "bold"),legend.title = element_text(size = 14), text = element_text(size=14))+
  ggtitle('Gender and Customer Lifetime Value')+xlab('Gender')+ylab('Customer Lifetime Value')+labs(color = "Gender")+ scale_color_brewer(palette="Blues")

# explore the relation between Customer.Lifetime.Value and Monthly.Premium.Auto
auto %>% 
  ggplot()+geom_point(aes(Monthly.Premium.Auto,y=log(Customer.Lifetime.Value), color = Monthly.Premium.Auto))+
  theme(plot.title = element_text(size = 16, face = "bold"),legend.title = element_text(size = 14), text = element_text(size=14))+
  ggtitle('Monthly Premium and Customer Lifetime Value')+xlab('Monthly Premium')+ylab('Customer Lifetime Value')+labs(color = "Monthly Premium")

# explore the relation between Customer.Lifetime.Value and Coverage
auto %>% 
  ggplot()+geom_boxplot(aes(Coverage,y=log(Customer.Lifetime.Value), color = Coverage))+
  theme(plot.title = element_text(size = 16, face = "bold"),legend.title = element_text(size = 14), text = element_text(size=14))+
  ggtitle('Coverage Type and Customer Lifetime Value')+xlab('Coverage Type')+ylab('Customer Lifetime Value')+labs(color = "Coverage Type")+ scale_color_brewer(palette="Blues")

# explore the relation between Customer.Lifetime.Value and Number.of.Policies
auto %>% 
  ggplot()+geom_boxplot(aes(as.factor(Number.of.Policies),y=log(Customer.Lifetime.Value), color = as.factor(Number.of.Policies)))+
  theme(plot.title = element_text(size = 16, face = "bold"),legend.title = element_text(size = 14), text = element_text(size=14))+  
  ggtitle('Number of Policies and Customer Lifetime Value')+xlab('Number of Policies')+ylab('Customer Lifetime Value')+labs(color = "Number of Policies")+ scale_color_brewer(palette="Blues")

# explore the relation between Customer.Lifetime.Value and Renew.Offer.Type
auto %>% 
  ggplot()+geom_boxplot(aes(as.factor(Sales.Channel),y=log(Customer.Lifetime.Value), color = Sales.Channel))+
  theme(plot.title = element_text(size = 16, face = "bold"),legend.title = element_text(size = 14), text = element_text(size=14))+
  ggtitle('Sales Channel and Customer Lifetime Value')+xlab('Sales Channel')+ylab('Customer Lifetime Value')+labs(color = "Sales Channel")+ scale_color_brewer(palette="Blues")

# explore the relation between Customer.Lifetime.Value and Total.Claim.Amount
auto %>% 
  ggplot()+geom_smooth(aes(Total.Claim.Amount,y=log(Customer.Lifetime.Value), color = 'red'))+
  theme(plot.title = element_text(size = 20, face = "bold"),legend.title = element_text(size = 20), text = element_text(size=20))+
  ggtitle('Relationship between CLV and Monthly.Premium.Auto')
lm_TCA<- lm(Customer.Lifetime.Value~Total.Claim.Amount, data = auto)
summary(lm_TCA)

# explore the relation between Customer.Lifetime.Value and Gender
# we don't see any pattern in terms of gender, so we drop that variable
auto %>% 
  ggplot()+geom_boxplot(aes(Gender,y=log(Customer.Lifetime.Value), color = Gender))+
  theme(plot.title = element_text(size = 20, face = "bold"),legend.title = element_text(size = 20), text = element_text(size=20))+
  ggtitle('Relationship between Customer Lifetime Value and Gender')
ggplot(data = auto, aes(log(Customer.Lifetime.Value),Gender))+geom_jitter()
ggplot(data = auto, aes(log(Customer.Lifetime.Value), fill=Gender))+geom_density()
biserial.cor(auto$Customer.Lifetime.Value, auto$Gender, level = 1)


#####################################################
### PCA

# transfer data to numeric variables and normalization
auto1=setDT(auto[,c(2,4,5,6,8,9,11,12,18,19,20,21,23,24)])
auto1=auto1 %>%
  mutate_all(funs(match(., unique(.))))
autonew<-cbind(auto1,auto[,c(10,13,14,15,16,17,22)])

# Plot PCA
auto.pca <- prcomp(scale(autonew), center = TRUE,scale. = TRUE)
summary(auto.pca)
plot(auto.pca,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)
ggbiplot(auto.pca, ellipse=TRUE, labels=rownames(auto))

#################
loadings <- auto.pca$rotation[,1:4]
loadings

#### Loading 1
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:27],1]
loadingfit <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
#### Looking at which are large positive and large negative
#### Fisrt factor is Luxurious/Status/High-Tech Cars
####
#### Loading 2
v<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:27],2]
loadingfit <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

#### Loading 3
v<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:27],3]
loadingfit <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

#### Loading 4
v<-loadings[order(abs(loadings[,4]), decreasing=TRUE)[1:27],4]
loadingfit <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]




####################################Data Preparation########################################
############################################################################################
set.seed(123)
autodatatable=setDT(auto[,c(2,5:6,8,10:13,17:19,21,23:24)])
finalauto<-cbind(autodatatable,auto[,3])
head(finalauto)
colnames(finalauto)[colnames(finalauto)=="V2"] <- "Customer.Lifetime.Value"
training.samples <- finalauto$Customer.Lifetime.Value %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- finalauto[training.samples, ]
test.data <- finalauto[-training.samples, ]
nrow(train.data)

summary(train.data)

########################################Modeling############################################
############################################################################################
#####################################################
###basic model
basic.model<- lm(log(Customer.Lifetime.Value) ~., data = train.data)
summary(basic.model)


###interaction model
inter.model<- lm(log(Customer.Lifetime.Value) ~.+Coverage*Education, data = train.data)


###stepwise model
fitall <- lm(log(Customer.Lifetime.Value) ~. +Coverage*Education, data = train.data)
summary(fitall)
step <- stepAIC (fitall, direction = 'both')
step.model <- lm(log(Customer.Lifetime.Value) ~ Coverage + EmploymentStatus + 
                   Marital.Status + Monthly.Premium.Auto + Number.of.Policies + 
                   Vehicle.Class + Vehicle.Size + Policy.Type, data = train.data)
summary(step.model)


#####################################################
### Neural Network
autodatatable=setDT(auto[,c(2,5:6,8,10:13,17:19,21,23:24)])
autobinary=one_hot(autodatatable, cols = "auto", sparsifyNAs = FALSE, naCols = FALSE,
                   dropCols = TRUE, dropUnusedLevels = FALSE)
autobinary=data.frame(autobinary)
set.seed(123)
finalauto1<-cbind(autobinary,auto[,3])
head(finalauto1)
colnames(finalauto1)[colnames(finalauto1)=="auto[, 3]"] <- "Customer.Lifetime.Value"
training.samples1 <- finalauto1$Customer.Lifetime.Value %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data1 <- finalauto1[training.samples, ]
test.data1 <- finalauto1[-training.samples, ]
train.data_df1 = scale(train.data1)
test.data_df1= scale(test.data1)
nrow(train.data1)


#building the neural network
model=keras_model_sequential()
model %>% 
  layer_dense(units = 64, activation = "relu", input_shape = c(52)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1)

summary(model)
model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam()
)
x = as.matrix(train.data1[,1:52])

#training and evaluation
history_train=model %>% fit(
  x, train.data1[,53], 
  epochs = 30)
plot(history_train)

#Evaluate the model’s performance on the test data
model %>% evaluate(x, train.data1[,53],verbose = 0)

#Generate predictions on new data
predicted_neural=model %>% predict(x)
predicted_neural

#compare with base linear regression model
lm=lm(log(Customer.Lifetime.Value)~.,data=train.data1)
summary(lm)
actual=train.data1$Customer.Lifetime.Value
rmse(actual,predicted_neural)
predicted_reg=exp(predict(lm, newdata = train.data1))
rmse(actual,predicted_reg)

test_data <- test.data1$Customer.Lifetime.Value
test_neural <- model %>% predict(as.matrix(test.data1[,1:52]))
test_reg <- exp(predict(lm, newdata = test.data1))
rmse(test_data, test_neural)
rmse(test_data, test_reg)

#testing and evaluation
y = as.matrix(test.data1[,1:52])
history_test=model %>% fit(
  y, test.data1[,53], 
  epochs = 30)
plot(history_test)


#####################################################
### SVM
set.seed(123)
autodatatable=setDT(auto[,c(2,5:6,8,10:13,17:19,21,23:24)])
finalauto<-cbind(autodatatable,auto[,3])
head(finalauto)
colnames(finalauto)[colnames(finalauto)=="V2"] <- "Customer.Lifetime.Value"
training.samples <- finalauto$Customer.Lifetime.Value %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- finalauto[training.samples, ]
test.data <- finalauto[-training.samples, ]
nrow(train.data)
summary(train.data)

svm_result <- svm(Customer.Lifetime.Value~EmploymentStatus+Coverage+Monthly.Premium.Auto, data=train.data, type="eps-regression", kernel="linear")
summary(svm_result)

pre <- predict(svm_result,newdata=train.data)
pre_frame <- cbind(train.data$Customer.Lifetime.Value, pre)
pre_frame <- as.data.frame(pre_frame)
pre_frame$error = pre_frame$V1 - pre_frame$pre
summary(pre_frame)
mean(pre_frame$V1)

R2<- 1-(sum(pre_frame$error ^2) / sum((pre_frame$V1-mean(pre_frame$V1))^2))

head(finalauto)

beta_svm <- t(svm_result$coefs) %*% svm_result$SV
beta_svm


####Out of Sample R2 compare
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(nrow(train.data)/nfold))[sample(1:nrow(train.data))]
### create an empty dataframe of results
OOS <- data.frame(basic=rep(NA,nfold), inter=rep(NA,nfold),stepwise=rep(NA,nfold),svm=rep(NA,nfold))

R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }
  }
  dev <- deviance(y, pred, family=fam)
  dev0 <- deviance(y, mean(y), family=fam)
  return(1-dev/dev0)
}

for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  basic.model<- lm(log(Customer.Lifetime.Value) ~., data = train.data)
  inter.model<- lm(log(Customer.Lifetime.Value) ~. +Coverage*Education, data = train.data)
  step.model <- lm(log(Customer.Lifetime.Value) ~ Coverage + EmploymentStatus + 
                     Marital.Status + Monthly.Premium.Auto + Number.of.Policies + 
                     Vehicle.Class + Vehicle.Size + Policy.Type, data = train.data)
  svm.model <- svm(Customer.Lifetime.Value~EmploymentStatus+Coverage+Monthly.Premium.Auto, data=train.data, type="eps-regression", kernel="linear")
  
  pred.basic<- exp(predict(basic.model, newdata=train.data[-train,], type="response"))
  pred.inter<- exp(predict(inter.model, newdata=train.data[-train,], type="response"))
  pred.stepwise <- exp(predict(step.model, newdata=train.data[-train,], type="response"))
  pred.svm <- predict(svm.model, newdata=train.data[-train,], type="response")

  OOS$basic[k] <- R2(y=train.data$Customer.Lifetime.Value[-train], pred=pred.basic,family = "gaussian")
  OOS$basic[k]
  OOS$inter[k] <- R2(y=train.data$Customer.Lifetime.Value[-train], pred=pred.inter,family = "gaussian")
  OOS$inter[k]
  OOS$stepwise[k] <- R2(y=train.data$Customer.Lifetime.Value[-train], pred=pred.stepwise,family = "gaussian")
  OOS$stepwise[k]
  OOS$svm[k] <- R2(y=train.data$Customer.Lifetime.Value[-train], pred=pred.svm,family = "gaussian")
  OOS$svm[k]
  
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=-0.1, yjust=0.5),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))

#####################################################
### Lasso
Mx<- model.matrix(log(Customer.Lifetime.Value) ~.+Coverage*Education, data = train.data)[,-1]
My<- log(train.data$Customer.Lifetime.Value)

lasso <- glmnet(Mx,My)
summary(lasso)
support(lasso$beta)
colnames(Mx)[support(lasso$beta)]
length(support(lasso$beta))
lasso <- glmnet(Mx,My)
summary(lasso)

lasso$lambda[1:5]

par(mar=c(1.5,1.5,0.75,1.5))
par(mai=c(1.5,1.5,0.75,1.5))

par(mfrow=c(1,2))
coef_ind <- 5
par(mar=c(1.5,0.5,0.75,0.5))
par(mai=c(1.5,0.5,0.75,0.5))
plot(log(lasso$lambda),lasso$beta[coef_ind,], ylab="Coefficient value", main=paste("Coefficient for",colnames(Mx)[coef_ind]),xlab = expression(paste("log(",lambda,")")),type="l")
coef_ind <- 2
par(mar=c(1.5,0.5,0.75,0.5))
par(mai=c(1.5,0.5,0.75,0.5))

plot(log(lasso$lambda),lasso$beta[coef_ind,], ylab="Coefficient value", main=paste("Coefficient for",colnames(Mx)[coef_ind]),xlab = expression(paste("log(",lambda,")")),type="l")

par(mfrow=c(1,1))
par(mar=c(1.5,1.5,1.5,1.5))
par(mai=c(1.5,1.5,1.5,1.5))


plot(lasso, xvar="lambda", main="# of non-zero coefficients", ylab ="Coefficient values", xlab = expression(paste("log(",lambda,")")))

lassoCV <- cv.glmnet(Mx,My)

par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))

lassoCV$lambda[which.min(lassoCV$cvm)]

text(log(lassoCV$lambda.min), .95,"min",cex=1)
text(log(lassoCV$lambda.1se), 1,"1se",cex=1)

####Post Lasso
nfold <- 10
PL.OOS <- data.frame(PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold)) 
L.OOS <- data.frame(L.min=rep(NA,nfold), L.1se=rep(NA,nfold)) 
features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 

data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)

nfold <- 10
foldid <- rep(1:nfold,each=ceiling(nrow(train.data)/nfold))[sample(1:nrow(train.data))]

for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'

  rmin <- glm(My~., data=data.min, subset=train)
  if ( length(features.1se) == 0){  r1se <- glm(log(Customer.Lifetime.Value)~1, data=train.data, subset=train) 
  } else {r1se <- glm(My~., data=data.1se, subset=train)
  }
  
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  pred1se  <- predict(r1se, newdata=data.1se[-train,], type="response")
  
  PL.OOS$PL.min[k] <- R2(y=My[-train], pred=predmin)
  PL.OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se)
  
  lassomin  <- glmnet(Mx[train,],My[train], lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train], lambda = lassoCV$lambda.1se)
  
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  L.OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin)
  L.OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se)
  
  print(paste("Iteration",k,"of",nfold,"completed"))
}

R2performance <- cbind(OOS,PL.OOS,L.OOS)
par( mar=  c(8, 4, 4, 2) + 0.1 )
barplot(colMeans(R2performance), las=2,xpd=FALSE, ylim=c(0.2,0.3) , xlab="", ylab = bquote( "Average Out of Sample " ~ R^2))
m.OOS <- as.matrix(R2performance)
rownames(m.OOS) <- c(1:nfold)
par(mar=c(1.5,1.5,1.5,3))
par(mai=c(1.5,1.5,1.5,3))
barplot(t(as.matrix(m.OOS)), beside=TRUE, ylim=c(0,0.35) ,legend=TRUE, xpd = FALSE, xlim = c(0, 120), args.legend=list(x= "topright", y=0.8,cex=0.6,bty = "n",inset=c(0.05,0.1)),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10),col=c("#CACFD2", "#AAB7B8", "#99A3A4","#5D6D7E","#AED6F1","#3498DB","#2471A3","#0C19F6"))


###Apply Final Model to Prediction####################
#final model
rmin <- glm(My~., data=data.min, subset=train)
test.DATA <- test.data

test.Mx<- model.matrix( ~ .+Coverage*Education, data=test.DATA)[,-1]
testt = as.data.frame(test.Mx)
names(testt)[9] = "EducationHigh.School.or.Below"
names(testt)[25] = "Policy.TypeSpecial.Auto"
names(testt)[26] = "PolicyCorporate.L2"
names(testt)[39] = "Vehicle.ClassSports.Car"
names(testt)[46] = "CoveragePremium.EducationCollege"
names(testt)[48] = "CoveragePremium.EducationDoctor"
names(testt)[50] = "CoveragePremium.EducationHigh.School.or.Below"
names(testt)[52] = "CoveragePremium.EducationMaster"
######model.matrix function may have error when transforming all varaiables into column matrix
######when transforming, R sometimes will include ".", sometimes will not
######the name differences therefore may cause error in running
######we tested this in several computers. Normally, above codes should be working

finalmodpred <- predict(rmin, newdata=testt, type="response")
finalmodpredR2<- R2(y=log(test.data$Customer.Lifetime.Value), pred=finalmodpred)
finalmodpredR2


