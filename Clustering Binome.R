install.packages("spam")
library(spam)
install.packages("Matrix")
library("Matrix")
library("skmeans")
#x=read.MM("C:/Users/anisboyka/Desktop/projettutore/sparse_matrix1.mtx")
#x
l=readMM(file = "C:/Users/anisboyka/Desktop/projettutore/sparse_matrix1.mtx")
summary(l)
j=as(l,"dgTMatrix")
j
help(readMM)
str(j)

summary(l)
dim(l)
install.packages("skmeans")
library("skmeans")
hparty <- skmeans(j, 8, control = list(verbose = TRUE))


hparty$value
library("cluster")
library(fpc)
help("plotcluster")
plotcluster(l[,1:1000],hparty$cluster)