library(recommenderlab)
library(reshape2)
library(ggplot2)
library("R.matlab")
library("skmeans")
library(spam)
library("Matrix")
library("skmeans")
install.packages("NMF")

#datasongs=readMat("C:/Users/Le novo/Desktop/donnee_l2.mat")
#songs =datasongs$norm
dataBrut=read.csv("C:/Users/anisboyka/Desktop/projettutore/songsDataset.csv")
dim(dataBrut)
set.seed(1)
data_package <- data(package = "recommenderlab")
realsongs100=as(dataBrut[1:1000,1:3],"realRatingMatrix")
dim(realsongs100)
class(realsongs100)
library(reshape2)
row=dataBrut$X.userID.
column=dataBrut$X.songID.
note=dataBrut$X.rating.
jeu=dataBrut[1:1000,1:3]
library("reshape2")
s=table(jeu$X.userID.,jeu$X.songID.)
dim(s)
p=as.data.frame.matrix(s)
dim(p)
class(p)
##################################################################################################################################""

##### / * partie NMF ET CLUSTERING /
library(NMF)
library("fastICA")
install.packages("fastICA")

install.packages('reshape')
library(reshape)

dim(databrut)
songs100=databrut[1:100,1:3]
songs100
meth <- nmfAlgorithm(version = "R")
meth <- c(names(meth), meth)
d=table(songs100)

dim(p)
res <- nmf(p,1:3,meth, seed = 123456)
resultat=nmfEstimateRank(p,1:3,meth,model = NULL,verbose = TRUE, stop = FALSE)

plot(res$lee,resultat$fit)


t <- sapply(res, runtime)[3, ]

summary(res)


plot(res)


res.nmf=nmf(p,seq(1:20),seed="ica")

plot(res.nmf)
(recc_data_test)
t <- sapply(res.nmf, runtime)[3, ]
t


is.matrix(w)




#####################################################################################################################################






methods(class = class(realsongs100))
which_train <- sample(x = c(TRUE, FALSE), size = nrow(realsongs100),
                      replace = TRUE, prob = c(0.01, 0.001))

head(which_train)

recc_data_train <- realsongs100[which_train, ] #ensemple d'apprentissage 
recc_data_test <- realsongs100[!which_train, ] #ensemple de test 
colnames(recc_data_train) <- paste("M", 1:1000 , sep = " ")

recommender_models <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommender_models$UBCF_realRatingMatrix$parameters

recc_model <- Recommender(data = recc_data_train, method = "UBCF")
recc_model
class(recc_model)
recc_model
model_details <- getModel(recc_model)
model_details$data
names(model_details)
n_recommended <- 1
is.null(recc_data_test)
new_data=as(recc_data_test,"realRatingMatrix")
new_data
colnames(new_data) <- paste("M", 1:1000 , sep = " ")

recc_predicted <- predict(object = recc_model,newdata = new_data, n = n_recommended) 
recc_predicted2 <- predict(object = recc_model,newdata = new_data, n = n_recommended)
as(list(recc_predicted)[[1]],"list")
recc_predicted@items[1]

recc_data_test