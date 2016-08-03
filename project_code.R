library("RCurl")
library("ggplot2")
#library(dplyr)
#import
data <- read.csv("C:/Users/DB/Desktop/CKME136-CAPSTONE/Capstone_Dataset.csv")


#adding new headers to the dataset
names(data) <- c ("Year", "Gender", "Age", "Total_Pop", "Incidents", "Prevalence", "Mortality", "Hospitalized", "Hospital_Days", "Physician_Visit", "No_Visits")

#----------EXPLORING DATA-----------#

#display few rows of the dataset
head(data)

#show data summary
summary(data)

#find number of rows and columns
dim (data)

#show number of NULL values in dataset
sum(is.na(data))

#Reorder Age Group 
x = factor(data$Age)
#shows the levels of x
print(levels(x))
#Reorder the x levels
age_order <- factor(x,levels(x)[c(1,10,2:9,11:18)])
#show the reordered levels
print(levels(age_order))


#Group Bar Plot for Prevalence vs. Age (grouped by gender)
ggplot(data, aes(age_order, Prevalence, fill=Gender)) + geom_bar(stat="identity", position="dodge") + theme(text = element_text(size = 20)) + scale_fill_brewer(palette="Set1") + xlab("Age Groups") + ylab("Prevalent Cases") + ggtitle("Prevalence vs. Age (grouped by Gender)")
#Group Bar Plot for Mortality vs. Age (grouped by gender)
ggplot(data, aes(factor(age_order), Mortality, fill=Gender)) + geom_bar(stat="identity", position="dodge") + theme(text = element_text(size = 20)) + scale_fill_brewer(palette="Set1")+ xlab("Age Groups") + ylab("Mortality") + ggtitle("Mortality vs. Age (grouped by Gender)")


#Convert Gender to numeric
data$gender_num[data$Gender=="F"] <- 1
data$gender_num[data$Gender=="M"] <- 2
data$gender_num <- factor(data$gender_num)
data$gender_num <- as.integer (as.character(data$gender_num))
head (data,10)

#Convert Gender to numeric
data$age_num[data$Age=="1 to 4"] <- 1
data$age_num[data$Age=="5 to 9"] <- 2
data$age_num[data$Age=="10 to 14"] <- 3
data$age_num[data$Age=="15 to 19"] <- 4
data$age_num[data$Age=="20 to 24"] <- 5
data$age_num[data$Age=="25 to 29"] <- 6
data$age_num[data$Age=="30 to 34"] <- 7
data$age_num[data$Age=="35 to 39"] <- 8
data$age_num[data$Age=="40 to 44"] <- 9
data$age_num[data$Age=="45 to 49"] <- 10
data$age_num[data$Age=="50 to 54"] <- 11
data$age_num[data$Age=="55 to 59"] <- 12
data$age_num[data$Age=="60 to 64"] <- 13
data$age_num[data$Age=="65 to 69"] <- 14
data$age_num[data$Age=="70 to 74"] <- 15
data$age_num[data$Age=="75 to 79"] <- 16
data$age_num[data$Age=="80 to 84"] <- 17
data$age_num[data$Age=="85+"] <- 18
data$age_num <- factor(data$age_num)
data$age_num <- as.integer (as.character(data$age_num))

head(data,10)

#show data type for age_num
class(data$age_num)
#find data type for all 
sapply(data, class)
sapply(data,mode)

#Plot all columns agains oneanother
plot(data[,5:13], main="Plotting Dataset with All Attributes")
plot (data$Mortality ~ data$Year, xlab="Year", ylab="Mortality", main="Mortality by Year from 2000-2011", col="darkmagenta")

#Boxplots Comparisons
boxplot (data$Prevalence ~ data$age_num, xlab="Age Categories", ylab="Prevalent Cases", main="Boxplot Comparison of Age Categories with Prevalent Cases")
boxplot (data$Mortality ~ data$age_num, xlab="Age Categories", ylab="Mortality Cases", main="Boxplot Comparison of Age Categories with Death Cases")

#Plot Mortality vs Age Group
ggplot (data, aes(age_order,Mortality)) + geom_point (position="jitter") + xlab("Age Group") + theme(text = element_text(size = 20)) + ggtitle("Scatter Plot of Age and Mortality")

#Histogram for Year
hist(data$Year, xlab="Year", ylab="Frequency", main="Histogram for Year 2000-2011")

#Histogram for Frequency Distribution of all attributes
par(mfrow=c(4,3))
par(mar=rep(2,4))
for (i in 4:13) {
  hist(data[,i], main=names(data) [i], col="purple")
}



#Pie Chart for Age Groups
install.packages('plotrix')
library(plotrix)
new_age1 <- which(data$age_num==1) 
new_age2 <- which(data$age_num==2)
new_age3 <- which(data$age_num==3)
new_age4 <- which(data$age_num==4)
new_age5 <- which(data$age_num==5)
new_age6 <- which(data$age_num==6)
new_age7 <- which(data$age_num==7)
new_age8 <- which(data$age_num==8)
new_age9 <- which(data$age_num==9)
new_age10 <- which(data$age_num==10)
new_age11 <- which(data$age_num==11)
new_age12 <- which(data$age_num==12)
new_age13 <- which(data$age_num==13)
new_age14 <- which(data$age_num==14)
new_age15 <- which(data$age_num==15)
new_age16 <- which(data$age_num==16)
new_age17 <- which(data$age_num==17)
new_age18 <- which(data$age_num==18)

A <- sum ((data[new_age1,7]), (data[new_age2,7]), (data[new_age3,7]), (data[new_age5,7]))
A
B <- sum ((data[new_age5,7]), (data[new_age6,7]), (data[new_age7,7]), (data[new_age8,7]), (data[new_age9,7]), (data[new_age10,7]))
C <- sum ((data[new_age11,7]), (data[new_age12,7]), (data[new_age13,7]))
D <- sum ((data[new_age14,7]), (data[new_age15,7]), (data[new_age16,7]), (data[new_age17,7]))
E <- sum (data[new_age18,7])
A
D
E
slices <- c(A,B,C,D,E) 
lbls <- c("1-19", "20-49","50-64" ,"65-84", "85+")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # add % to labels 
pie3D(slices,labels=lbls,radius = 0.8, explode=0.1, start=1, labelcex=1.1, shade=0.5, main="Mortality Representation Based on Age Groups: 2000-2011")

################################################################
#Pie Chart for Prevalance
G <- sum ((data[new_age,4]), (data[new_age2,4]), (data[new_age3,4]), (data[new_age5,4]))
A
H <- sum ((data[new_age5,4]), (data[new_age6,4]), (data[new_age7,4]), (data[new_age8,4]), (data[new_age9,4]), (data[new_age10,4]))
I <- sum ((data[new_age11,4]), (data[new_age12,4]), (data[new_age13,4]))
J <- sum ((data[new_age14,4]), (data[new_age15,4]), (data[new_age16,4]), (data[new_age17,4]), (data[new_age18,4]))
A
D
slices <- c(G,H,I,J) 
lbls <- c("1-19", "20-49","50-64" ,"65+")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"\n", pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # add % to labels 
pie3D(slices,labels=lbls,radius = 0.8, explode=0.1, start=1, labelcex=1.1, shade=0.5, main="Mortality Representation Based on Age Groups: 2000-2011")

################################################################
#Pie Chart for Gender
new_f <- which(data$gender_num==1) 
new_m <- which(data$gender_num==2)
fem <- sum ((data[new_f,7]))
male <- sum ((data[new_m,7]))
slices <- c(fem,male) 
lbls <- c("Female", "Male")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"\n",pct) # add percents to labels 
lbls <- paste(lbls,"%",sep ="") # add % to labels 
pie3D(slices,labels=lbls,radius = 0.8, explode=0.1, start=1.1, labelcex=1.1, shade=0.5, main="Mortality Percentage Based on Gender: 2000-2011")

#------------------------------------

#Clustering

#graph Prevalence vs. Mortality by Age Group
ggplot(data, aes(Prevalence, Mortality, color = age_order)) + geom_point(position="jitter") + theme(text = element_text(size = 20)) + ggtitle("Plot of Prevalence vs Mortality by Age Groups")
#graph Gender vs. Mortality by Age Group
ggplot(data, aes(age_order, Mortality, color = Gender)) + geom_point()+ theme(text = element_text(size = 20)) + xlab("Age Groups") + ggtitle("Plot of Age Groups vs Mortality by Gender")

#create clusters to split age groups into two
set.seed(20)
data2Cluster <- kmeans(data[,7], 2, nstart=20)
data2Cluster
data2Cluster$size
data2Cluster$cluster
table(data$Age, data2Cluster$cluster)

#As we observe, the data belonging to ages 1-69 got grouped into cluster 1, ages 75-85+ into cluster 2. Half the data for ages 70-74 was grouped in cluster 1 and the other half in cluster 2.

#plot the data to see the clusters
data2Cluster$cluster <- as.factor(data2Cluster$cluster)
ggplot(data, aes(Prevalence, Mortality, colour = data2Cluster$cluster)) + geom_point() + theme(text = element_text(size = 20)) + ggtitle("Plot of Prevalence vs Mortality grouped by Clusters")


library(cluster)
install.packages ("fpc")
library(fpc)
#plotcluster(data$Mortality, data2Cluster$cluster)


clusplot(data, data2Cluster$cluster, main='2D representation of the Cluster solution',color=TRUE, shade=TRUE,labels=2, lines=0)
plotcluster(data[,5:13], data2Cluster$cluster)



#----------Correlation----------#
library (corrplot)

#Finding correlation coefficients
cor(data[,5:13], method="pearson")
#cor(data[,5:13], method="spearman")

#Visualizing correlation
corrplot (cor(data[,5:13]), method="ellipse")

#Significance of correlation coefficients
cor.test(data$age_num, data$Mortality, method=c("pearson"))




#-----------BUILDING PREDICTION MODEL----------#
# Using Linear Regression 
# First building a Multivariate Linear Regression Model based on the original full correlation matrix

#Linear Regression: fitting the model
plot(data$age_num, data$Mortality, main="Mortality and Age Group Scatterplot", xlab="Age Group", ylab="Mortality")
fit_mort <- lm(Mortality~age_num, data=data)
abline(fit_mort)

#Creating the training model based on the original full correlation matrix
model_mlr <- lm(Mortality~Incidents+Prevalence+Hospitalized+Hospital_Days+Physician_Visit+No_Visits+gender_num+age_num, data=data)
#Looking at the model
summary(model_mlr)

# Creating training and testing set using 80/20 random split rows
rn_train <- sample(nrow(data[,5:13]), floor(nrow(data[,5:13])*0.8))
train <- data[rn_train,]
test <- data[-rn_train,]
model_mlr <- lm(Mortality~Incidents+Prevalence+Hospitalized+Hospital_Days+Physician_Visit+No_Visits+gender_num+age_num, data=train)


#Running the prediction on test set
prediction <- predict(model_mlr, interval="prediction", newdata=test)

# Running RMSE to evaluate the prediction
rmse <- sqrt(sum((prediction [,"fit"] - test$Mortality)^2) / nrow(test))
paste("RMSE:", rmse)
# on average we have error of 613 deaths for each prediction

errors <- prediction[,"fit"] - test$Mortality
#Histogram of errors
hist(errors)

rel_change <- 1 - ((test$Mortality - abs(errors)) / test$Mortality)
pred25 <- table(rel_change<0.25) ["TRUE"] / nrow(test)
paste("PRED(25):", pred25)
#40% of cases have less than 25% error.


#----------MODEL SELECTION----------#

library(MASS) #for stepwise regression
install.packages ("leaps")
library(leaps) #for all subsets regression
install.packages("glmnet")
library(glmnet) #for regularized linear regression

#Forward stepwise regression
full <- lm(Mortality~Incidents+Prevalence+Hospitalized+Hospital_Days+Physician_Visit+No_Visits+gender_num+age_num, data=data[,5:13])
null <- lm(Mortality~1, data=data[,5:13])
stepF <- stepAIC (null, scope=list(lower=null, upper=full), direction="forward", trace=FALSE)
summary(stepF)

#Backward stepwise regression
full <- lm(Mortality~Incidents+Prevalence+Hospitalized+Hospital_Days+Physician_Visit+No_Visits+gender_num+age_num, data=data[,5:13])
stepB <- stepAIC (full, scope=list(lower=null, upper=full), direction="backward", trace=FALSE)
summary(stepB)

#Finding combination of the best 5 attributes (best subset of the variables)
subsets <- regsubsets (Mortality~Incidents+Prevalence+Hospitalized+Hospital_Days+Physician_Visit+No_Visits+gender_num+age_num, data=data[,5:13], nbest=1)
sub.sum <- summary (subsets)
as.data.frame(sub.sum$outmat)
#The best combination of 5 attributes is: Incidents, Prevalence, Hospital_Days, Physician_Visit, age_num
#The best combination of 6 attributes is: Incidents, Prevalence, Hospitalized, Hospital_Days, Physician_Visit, age_num
#The best combination of 7 attributes is: Incidents, Prevalence, Hospitalized, Hospital_Days, Physician_Visit, gender_num, age_num

#Plot regularized linear regression error rate as a function of lambda
cv.fit <- cv.glmnet(as.matrix(data[,c("Incidents","Prevalence","Hospitalized","Hospital_Days","Physician_Visit","No_Visits","gender_num","age_num")]), as.vector(data[,"Mortality"]), nlambda=100)
plot(cv.fit)

#Choosing a NEW MODEL 
new_model <- lm(Mortality~Prevalence+Hospital_Days+Physician_Visit+age_num, data=data[,5:13])
summary(new_model)
#Using train and test for our new model
rn_train <- sample(nrow(data[,5:13]), floor(nrow(data[,5:13])*0.8))
train <- data[rn_train,]
test <- data[-rn_train,]
#Creating a new training model
new_model <- lm(Mortality~Prevalence+Hospital_Days+Physician_Visit+age_num, data=train)

#Looking at the model
summary(new_model)

#Running the prediction on test set
prediction <- predict(new_model, interval="prediction", newdata=test)

# Running RMSE to evaluate the prediction
rmse <- sqrt(sum((prediction [,"fit"] - test$Mortality)^2) / nrow(test))
paste("RMSE:", rmse)
# on average we have error of 613 deaths for each prediction

errors <- prediction[,"fit"] - test$Mortality
#Histogram of errors
hist(errors)

rel_change <- 1 - ((test$Mortality - abs(errors)) / test$Mortality)
pred25 <- table(rel_change<0.25) ["TRUE"] / nrow(test)
paste("PRED(25):", pred25)
#37% of cases have less than 25% error.