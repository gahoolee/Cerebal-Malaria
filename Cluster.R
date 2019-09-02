source('correlation R code.R') #or ('clean data(2017).R')
library(randomForest)
g <- randomForest(DATA, keep.forest=FALSE, proximity=TRUE)
mds <- MDSplot(g, DATA$Retinopathy, k=2, pch=16, palette=c("skyblue", "orange"))
library(cluster)
clusters_pam <- pam(1-g$proximity, k=7, diss=TRUE)
plot(mds$points[, 1], mds$points[, 2], pch=clusters_pam$clustering+14, col=c("skyblue","orange")[as.numeric(DATA$Retinopathy)])
legend("bottomleft", legend=unique(clusters_pam$clustering), pch = 15:17, title = "PAM cluster")
legend("topleft", legend=unique(DATA$Retinopathy), pch = 16, col=c("skyblue", "red"), title = "Retinopathy")


DATA.pc <- prcomp(primers, center = FALSE, scale. = FALSE)$x %>% as.data.frame()
# k means cluster: noramlize the DATA before cluster
k2 <- kmeans(primers, centers = 2, iter.max = 20, nstart = 3)
DATA.pc$kmeans.cluster <- k5$cluster
mjs_plot(DATA.pc, x=PC1, y=PC2) %>%
  mjs_point(color_accessor=kmeans.cluster) %>%
  mjs_labs(x="principal comp 1", y="principal comp 2")  #graph 1
fviz_cluster(k5, data = primers)  #graph 2

k3 <- kmeans(primers, centers = 3, nstart = 25)
k4 <- kmeans(primers, centers = 4, nstart = 25)
k5 <- kmeans(primers, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = primers) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = primers) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = primers) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = primers) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#random forset
rf.fit <- randomForest(x = primers, y = NULL, ntree = 5000, proximity = TRUE, oob.prox = TRUE)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=2)
DATA.pc$rf.clusters <- rf.cluster
table(rf.cluster, DATA$ret)
mjs_plot(DATA.pc, x=PC1, y=PC2) %>%
  mjs_point(color_accessor=rf.clusters) %>%
  mjs_labs(x="principal comp 1", y="principal comp 2")

#hcluster
library(psych)
plot(hclust(dist(abs(cor(na.omit(primers))))))
