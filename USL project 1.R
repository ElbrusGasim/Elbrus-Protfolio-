source('libraries.R')

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
download.file(url, 'wdbc.csv')
#install.packages("readr")
#library(readr)
#library(dplyr)
#glimpse(mtcars)
# use read_csv to the read into a dataframe
# columnNames are missing in the above link, so we need to give them manually.
columnNames <- c("id","diagnosis","radius_mean","texture_mean","perimeter_mean",
                 "area_mean","smoothness_mean","compactness_mean","concavity_mean",
                 "concave_points_mean","symmetry_mean","fractal_dimension_mean",
                 "radius_se","texture_se","perimeter_se","area_se","smoothness_se",
                 "compactness_se","concavity_se","concave_points_se","symmetry_se",
                 "fractal_dimension_se","radius_worst","texture_worst","perimeter_worst",
                 "area_worst","smoothness_worst","compactness_worst","concavity_worst",
                 "concave_points_worst","symmetry_worst","fractal_dimension_worst")
wdbc <- read_csv2(url, col_names = columnNames, col_types = NULL)
wdbc <- read.csv('wdbc.csv', header = FALSE, col.names = columnNames)
glimpse(wdbc)

# Convert the features of the data: wdbc.data
wdbc.data <- as.matrix(wdbc[,c(3:32)])

# Set the row names of wdbc.data
row.names(wdbc.data) <- wdbc$id

# Create diagnosis vector
diagnosis <- as.numeric(wdbc$diagnosis == "M")
nrow(wdbc.data)
# How many variables/features in the data are suffixed with _mean, _se, _worst?
sum(endsWith(colnames(wdbc.data), "_mean"))
sum(endsWith(colnames(wdbc.data), "_se"))
sum(endsWith(colnames(wdbc.data), "_worst"))
table(wdbc$diagnosis)

#What is the mean of each of the numeric columns ?
round(colMeans(wdbc.data),2)

#What is the sd of each of the numeric columns ?
roundSD <- function(x){
  round(sd(x),2)
}
apply(wdbc.data, 2, roundSD)

#How are the variables related to each other ?
corMatrix <- wdbc[,c(3:32)]


# Rename the colnames
cNames <- c("rad_m","txt_m","per_m",
            "are_m","smt_m","cmp_m","con_m",
            "ccp_m","sym_m","frd_m",
            "rad_se","txt_se","per_se","are_se","smt_se",
            "cmp_se","con_se","ccp_se","sym_se",
            "frd_se","rad_w","txt_w","per_w",
            "are_w","smt_w","cmp_w","con_w",
            "ccp_w","sym_w","frd_w")

colnames(corMatrix) <- cNames

# Create the correlation matrix
M <- round(cor(corMatrix), 2)

install.packages("ggplot2")
install.packages("corrplot")
install.packages("xlsx")

library(ggplot2)
library(corrplot)
library(xlsx)

# Create corrplot
corrplot(M, diag = FALSE, method="color", order="FPC", tl.srt = 90)

#Running a PCA using covariance matrix

wdbc.pcov <- princomp(wdbc.data, scores = TRUE)
summary(wdbc.pcov)

#Bi-plot using covariance matrix
cex.before <- par("cex")
par(cex = 0.7)
biplot(wdbc.pcov)

par(cex = cex.before)

#Scree plots can be useful in deciding how many PC’s we should keep in the model. Let’s create the scree-plots in R

#Set up 1 x 2 plotting grid
par(mfrow = c(1, 1))

# Calculate variability of each component
pr.cvar <- wdbc.pcov$sdev ^ 2

# Variance explained by each principal component: pve
pve_cov <- pr.cvar/sum(pr.cvar)
#Eigen values
round(pr.cvar, 2)
# Percent variance explained
round(pve_cov, 2)
# Cummulative percent explained
round(cumsum(pve_cov), 2)
#Create a plot of variance explained for each principal component.
# Plot variance explained for each principal component
plot(pve_cov, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve_cov), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")
#Running PCA using correlation matrix:
wdbc.pr <- prcomp(wdbc.data, scale = TRUE, center = TRUE)
summary(wdbc.pr)

#Get the eigen values of correlation matrix:
# Eigen values using covariance matrix

round(wdbc.pr$sdev ^2,4)

#Bi-Plot
#Let’s create a bi-plot to visualize this:
cex.before <- par("cex")
par(cex = 0.7)
biplot(wdbc.pr)

par(cex = cex.before)
#Create a scatter plot of observations by components 1 and 2
# Scatter plot observations by components 1 and 2
plot(wdbc.pr$x[, c(1, 2)], col = (diagnosis + 1),
     xlab = "PC1", ylab = "PC2")
legend(x="topleft", pch=1, col = c("red", "black"), legend = c("B", "M"))

#Let’s also take PC1 vs PC3 plot:
# Repeat for components 1 and 3
plot(wdbc.pr$x[, c(1,3)], col = (diagnosis + 1),
     xlab = "PC1", ylab = "PC3")
legend(x="topleft", pch=1, col = c("red", "black"), legend = c("B", "M"))

# Repeat for components 1 and 3
plot(wdbc.pr$x[, c(1,4)], col = (diagnosis + 1),
     xlab = "PC1", ylab = "PC4")
legend(x="topleft", pch=1, col = c("red", "black"), legend = c("B", "M"))

# Repeat for components 1 and 3
plot(wdbc.pr$x[, c(1,5)], col = (diagnosis + 1),
     xlab = "PC1", ylab = "PC5")
legend(x="topleft", pch=1, col = c("red", "black"), legend = c("B", "M"))

# Repeat for components 1 and 6
plot(wdbc.pr$x[, c(1,6)], col = (diagnosis + 1),
     xlab = "PC1", ylab = "PC6")
legend(x="topleft", pch=1, col = c("red", "black"), legend = c("B", "M"))
#Scree plots 
# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))

# Calculate variability of each component
pr.var <- wdbc.pr$sdev ^ 2

# Assign names to the columns to be consistent with princomp.
# This is done for reporting purposes.
names(pr.var) <- names(pr.cvar)

# Variance explained by each principal component: pve
pve <- pr.var/sum(pr.var)

# Assign names to the columns as it is not done by default.
# This is done to be consistent with princomp.
names(pve) <- names(pve_cov)

# Eigen values
round(pr.var, 2)

# Percent variance explained
round(pve, 2)

# Cummulative percent explained
round(cumsum(pve), 2)

#Create a plot of variance explained for each principal component.
# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

#From the wdbc.pr object, we need to extract the first five PC’s. To do this, let’s first check the variables available for this object.
ls(wdbc.pr)

wdbc.pcs <- wdbc.pr$x[,1:6]
head(wdbc.pcs, 20)


wdbc.pcst <- wdbc.pcs
wdbc.pcst <- cbind(wdbc.pcs, diagnosis)
head(wdbc.pcst)

#Model Validation

#Splitting the dataset into training/test data

# Calculate N
N <- nrow(wdbc.pcst)

# Create a random number vector
rvec <- runif(N)

# Select rows from the dataframe
wdbc.pcst.train <- wdbc.pcst[rvec < 0.75,]
wdbc.pcst.test <- wdbc.pcst[rvec >= 0.75,]

# Check the number of observations
nrow(wdbc.pcst.train)


nrow(wdbc.pcst.test)


library(MASS)

wdbc.pcst.train.df <- wdbc.pcst.train

# convert matrix to a dataframe
wdbc.pcst.train.df <- as.data.frame(wdbc.pcst.train)

# Perform LDA on diagnosis
wdbc.lda <- lda(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = wdbc.pcst.train.df)

#Let’s summarize the LDA output:

wdbc.lda

#Let’s use this to predict by passing the predict function’s newdata as the testing dataset.

wdbc.pcst.test.df <- wdbc.pcst.test

# convert matrix to a dataframe
wdbc.pcst.test.df <- as.data.frame(wdbc.pcst.test)

wdbc.lda.predict <- predict(wdbc.lda, newdata = wdbc.pcst.test.df)

#Let’s check what functions we can invoke on this predict object:

ls(wdbc.lda.predict)

#Our predictions are contained in the class attribute.

# print the predictions
(wdbc.lda.predict.class <- wdbc.lda.predict$class)


(confusionMat <- table(wdbc.lda.predict.class, wdbc.pcst.test.df$diagnosis))


#3-fold cross validation
install.packages("vtreat)")
library(vtreat)

# convert wdbc.pcst to a dataframe
wdbc.pcst.df <- as.data.frame(wdbc.pcst)

nRows <- nrow(wdbc.pcst.df)
splitPlan <- kWayCrossValidation(nRows, 3, NULL, NULL)

# examine the split plan
str(splitPlan)

#Here, k is the number of folds and splitplan is the cross validation plan

# Run a 3-fold cross validation plan from splitPlan
k <- 3

for ( i in 1:k ) {
  split <- splitPlan[[i]]
  model <- lda(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = wdbc.pcst.df[split$train,])
  model.pred.cv <- predict(model, newdata = wdbc.pcst.df[split$app,])
  
  confMat <- table(model.pred.cv$class, wdbc.pcst.df$diagnosis[split$app])
  print(confMat)
}


#Running a 10-fold cross validation:


# Run a 10-fold cross validation plan from splitPlan
k <- 10
nRows <- nrow(wdbc.pcst.df)
splitPlan <- kWayCrossValidation(nRows, 10, NULL, NULL)

# Run a 10-fold cross validation plan from splitPlan

for ( i in 1:k ) {
  split <- splitPlan[[i]]
  model <- lda(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = wdbc.pcst.df[split$train,])
  model.pred.cv <- predict(model, newdata = wdbc.pcst.df[split$app,])
  
  confMat <- table(model.pred.cv$class, wdbc.pcst.df$diagnosis[split$app])
  print(confMat)
}
