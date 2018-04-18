library(recommenderlab)
library(reshape2)
library(ggplot2)
library("R.matlab")
library("skmeans")
library(spam)
library("Matrix")
library("skmeans")
install.packages("SNMF")

#datasongs=readMat("C:/Users/Le novo/Desktop/donnee_l2.mat")
#songs =datasongs$norm
dataBrut=read.csv("C:/Users/anisboyka/Desktop/projettutore/songsDataset.csv")
set.seed(1)

data_package <- data(package = "recommenderlab")
realsongs100=as(dataBrut[1:100000,1:3],"realRatingMatrix")
dim(realsongs100)


#mysample <- realsongs100[sample(1:nrow(realsongs100),200,
 #                         replace=FALSE),]


dim(mysample)

##################################################################################################################################""

##### / * partie NMF ET CLUSTERING /
library(NMF)
library("fastICA")


install.packages('reshape')
library(reshape)
s=melt(songs100,id.vars=songs100$X.songID.)
cast(songs100, songs100$X.userID.~songs100$X.songID.)

help(melt)


songs100
meth <- nmfAlgorithm(version = "R")
meth <- c(names(meth), meth)
d=table(songs100)

res.nmf=nmf(z,1:20,seed="ica",nrun=20)
plot(res.nmf)
(recc_data_test)
t <- sapply(res.nmf, runtime)[3, ]
t


is.matrix(w)




#####################################################################################################################################





dim(realsongs100)
methods(class = class(realsongs100))
which_train <- sample(x = c(TRUE, FALSE), size = nrow(realsongs100),
                      replace = TRUE, prob = c(0.8, 0.2))

head(which_train)

recc_data_train <- realsongs100[which_train, ] #ensemple d'apprentissage 
recc_data_test <- realsongs100[!which_train, ] #ensemple de test 
dim(recc_data_train)
recc_data_test=recc_data_test[1:100]
recc_data_train=recc_data_train[1:2000]
dim(recc_data_test)
#colnames(recc_data_train) <- paste("songs", 1:311 , sep = " ")
recommender_models <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommender_models$UBCF_realRatingMatrix$parameters

recc_model <- Recommender(data = recc_data_train, method = "UBCF")
recc_model
class(recc_model)
recc_model
model_details <- getModel(recc_model)
model_details
names(model_details)
n_recommended <- 2
recc_data_test
#colnames(recc_data_test) <- paste("songs", 1:311 , sep = " ")
gc()
dim(recc_data_test)
recc_predicted <- predict(object = recc_model,newdata = recc_data_test, n = n_recommended)
recc_predicted
z=getList(recc_predicted)
z
as(recc_predicted, "list")
dim(recc_data_test)
H=z[!sapply(z,identical,character(0))]
H
class(recc_predicted)

rmse_popular <- calcPredictionAccuracy(recc_predicted, getData(e, "unknown"))[1]












library(spam)
library("Matrix")
library("skmeans")
#x=read.MM("C:/Users/anisboyka/Desktop/projettutore/sparse_matrix1.mtx")
#x
library("R.matlab")
l=readMat("C:/Users/Le novo/Desktop/sparse5.mat")
summary(l)
j=as(l,"dgTMatrix")
dim(j)
help(readMM)
str(j)

summary(l)
dim(l)
library("skmeans")
hparty1 <- skmeans(j, 2, control = list(verbose = TRUE))
hparty2 <- skmeans(j, 3, control = list(verbose = TRUE))
hparty3 <- skmeans(j, 4, control = list(verbose = TRUE))
hparty4 <- skmeans(j, 5, control = list(verbose = TRUE))
hparty5 <- skmeans(j, 6, control = list(verbose = TRUE))
hparty6 <- skmeans(j, 7, control = list(verbose = TRUE))
hparty7 <- skmeans(j, 8, control = list(verbose = TRUE))
hparty8 <- skmeans(j, 9, control = list(verbose = TRUE))
hparty9 <- skmeans(j, 10, control = list(verbose = TRUE))

hparty
hparty$value
library("cluster")
library(fpc)
help("plotcluster")
hparty1$cluster



library(mclust)
fit <- Mclust(j[,1:1000])
plot(fit$BIC)
fit$bic
# plot results 
summary(fit) # display the best model
plot(fit, what = c("BIC", "classification"))
plotcluster(j[,1:1000],hparty5$cluster)