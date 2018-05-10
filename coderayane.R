library(recommenderlab)
library("clue")
install.packages("CLUTO")
library("Matrix")
library("skmeans")
install.packages("skmeans")
dataBrut=read.csv("C:/Users/anisboyka/Desktop/projettutore/songsDataset.csv")
set.seed(1)
dim(dataBrut)
data_package <- data(package = "recommenderlab")
realsongs100=as(dataBrut[1:100000,1:3],"realRatingMatrix")
dim(realsongs100)
methods(class = class(realsongs100))
which_train <- sample(x = c(TRUE, FALSE), size = nrow(realsongs100),
                      replace = TRUE, prob = c(0.8, 0.2))


realsongs100=realsongs100[1:10000,1:10000]
head(which_train)

recc_data_train <- realsongs100[which_train, ] #ensemple d'apprentissage 
recc_data_test <- realsongs100[!which_train, ] #ensemple de test 
dim(recc_data_test)
dim(recc_data_train)
recommender_models <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommender_models$UBCF_realRatingMatrix$parameters

recc_model <- Recommender(data = recc_data_train, method = "UBCF")
recc_model
class(recc_model)
model_details <- getModel(recc_model)
model_details
names(model_details)
n_recommended <- 10
recc_data_test
gc()
dim(recc_data_test)
recc_predicted <- predict(object = recc_model,newdata = recc_data_test, n = n_recommended,type="ratings")
recc_predicted
z=getList(recc_predicted)
z
as(recc_predicted, "list")
dim(recc_data_test)
H=z[!sapply(z,identical,character(0))]
H
class(recc_predicted)

jeu=dataBrut[1:1000000,1:3]
s=table(jeu$X.userID.,jeu$X.songID.)
dim(s)
p=as.data.frame.matrix(s)
dim(p)
class(p)
meth <- nmfAlgorithm(version = "R")
meth <- c(names(meth), meth)
res <- nmf(p,1:5,meth, seed = 123456)
resultat=nmfEstimateRank(p,1:3,meth,model = NULL,verbose = TRUE, stop = FALSE)

res.nmf=nmf(p,seq(1:20),seed="ica")
plot(res.nmf)

str(recc_data_test)

users=names(H)
users
users=as(users,"list")
items=list()
for (name in names(H)) {
  print(H[[name]])
  items[i]=H[[name]]
}
 ############# code items
items=as(items,"integer")
items=list()
k=0
  for (name in names(H)) {
   
    for(i in H[[name]]) {items[k] =i
  k=k+1
      }
  
  }
print(items)
length(users)
usetslist=list()

userslist=vector()
o=0
for(j in 1:length(users)){
  
  for(i in 1:10){
   userslist[o]=users[j]
  o=o+1
   }
}
length(vecteur)
  



dim(recc_data_test)
recc_data_test=as(recc_data_test,"dgTMatrix")
seed <- as.integer(runif(1) * 2147483647)
help("skmeans")
geneticSkmeans =skmeans(recc_data_test,k = 10,method = "genetic",control = list(maxiter=20,verbose = TRUE))


pclustSkmeans =skmeans(recc_data_test,k = 10,method = "pclust", control = list(nruns=20, verbose = TRUE))
require("cluster")



geneticSkmeans$cluster
plot(silhouette(geneticSkmeans))
pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)
library(factoextra)
library(NbClust)
class(recc_data_test)
fviz(recc_data_test[1:100,1:15], geneticSkmeans$cluster, method = c("silhouette", "wss", "gap_stat"))
