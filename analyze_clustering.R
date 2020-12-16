analyzesse<-function(x,n){
  wss <- (nrow(mydatafilt)-1)*sum(apply(x,2,var))
  for (i in 2:n) wss[i] <- sum(kmeans(x,
                                       centers=i)$withinss)
  plot(1:n, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}
analyzefpc<-function(x){
  library(fpc)
  library(cluster)
  pamk.best <- pamk(x)
  cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
  plot(pam(x, pamk.best$nc))
}