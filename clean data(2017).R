DATA <- read.table("~/Desktop/kamikim/(Dylan FINAL) Machine_Matrix_Mi_MP_2015-17_v12July2019.csv",row.names=1, sep=',', header=TRUE)

DATA <- DATA[-which(rowMeans(is.na(DATA)) > 0.7) ,]

#knn imputation to replac NA
library('lattice')
library('grid')
library('DMwR')
DATA[,c(56:99)] <- knnImputation(DATA[,c(56:99)][, !names(DATA[,c(56:99)]) %in% "medv"])  # perform knn imputation.
#anyNA(knnOutput)


#clean <- clean[, -which(colMeans(is.na(primers)) > 0.5)]

#Normalize all numeric variables
normalize <- function(x){
  x <- log2(x)
  return((x - mean(x))/sd(x))
}
for(i in 56:99){
  if (is.numeric(DATA[,i]) == "TRUE") {
    DATA[,i] <- normalize(DATA[,i])
  }
}

primers <- DATA[,c(56:99)]
#heatmap
distance <- get_dist(t(primers), method = "spearman")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

###pca 
myPCA <- prcomp(primers, scale = F, center = F)
plot(myPCA$x)

