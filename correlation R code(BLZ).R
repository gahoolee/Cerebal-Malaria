##########################
###################
#clean data 
DATA <- read.table("~/Desktop/kamikim/BLZtemplate_postReview.csv",row.names=1, sep=',', header=TRUE)

    #Delete columns from dataframe where ALL values are NA
DATA <- Filter(function(x)!all(is.na(x)), DATA)

    #Correct class
for(i in 1:ncol(DATA)){
  if (class(DATA[,i]) == "factor") {
    if (nlevels(DATA[,i]) > 10){
      DATA[,i] = as.numeric(DATA[,i])
    }
  }
  if (class(DATA[,i]) == "integer") {
    if (length(unique(DATA[,i])) < 10) {
      DATA[,i] = as.factor(DATA[,i])
    }
  }
}
    #Replace all NA with mean(numeric) and mode(factor) 
library(dplyr)
Mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
DATA <- DATA %>% mutate_if(is.numeric, funs(replace(.,is.na(.), mean(., na.rm = TRUE)))) %>%
  mutate_if(is.factor, funs(replace(.,is.na(.), Mode(na.omit(.)))))

 #Normalize all numeric variables
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
for(i in 1:nrow(DATA)){
  if (is.numeric(DATA[i,]) == "TRUE") {
    DATA[i,] <- normalize(DATA[i,])
  }
}



# correlation Model
primers <- DATA[, c(1:53)]
res <- cor(primers,use = "pairwise.complete.obs")
correlation1 <- round(res, 2)
quantile(unlist(correlation1),na.rm = T)
    #histogram
hist(unlist(correlation1),na.rm = F, breaks = 100)    
    #heatmap
distance <- get_dist(t(primers), method = "spearman")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


        