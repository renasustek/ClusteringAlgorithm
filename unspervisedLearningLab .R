source("WK_R.R")
seeds_dataset <- read.csv('seeds_dataset.csv', sep=",")
seeds_dataset <- na.omit(seeds_dataset)
seeds_dataset <- scale(seeds_dataset) 
seeds_real <- read.csv("seeds_real - Copy.csv", sep=",")
 
wk_kmeans <- list()
wk_hc_single <- list()
wk_hc_complete <- list()
wk_hc_average <- list()

for (x in 2:10) {
  fit <- kmeans(seeds_dataset, x)
  aggregate(seeds_real,by=list(fit$cluster),FUN=mean)
  Kgroups = fit$cluster 
  plot(seeds_dataset, col=Kgroups)
  
  wk = WK_R(Kgroups, seeds_real$Real)
  wk_kmeans[[x]] <- wk
  print(paste("(k-means)The weighted kappa for ",x," --> ", wk))
}

linkage_methods <- c("average", "complete", "single")
d <- dist(seeds_dataset, method = "euclidean")
for (hMethods in linkage_methods) {
    for (x in 2:10) {
      fit <- hclust(d, method=hMethods)
      plot(fit) 
      Hgroups <- cutree(fit, k=x)
      rect.hclust(fit, k=x, border="red") 
      plot(seeds_dataset, col=Hgroups)
      wk = WK_R(Hgroups, seeds_real$Real)
      if (hMethods == "single") {
        wk_hc_single[[x]] <- wk
      } else if (hMethods == "complete") {
        wk_hc_complete[[x]] <- wk
      } else if (hMethods == "average") {
        wk_hc_average[[x]] <- wk
      }
      print(paste("(hc)The weighted kappa for method",hMethods ," number: ",x," --> ", wk))
  }
}

print(paste("kmeans ",wk_kmeans))
print(paste("hc single ",wk_hc_single))
print(paste("hc complete ",wk_hc_complete))
print(paste("hc average ",wk_hc_average))

wk_kmeans_vec <- unlist(wk_kmeans)
wk_hc_single_vec <- unlist(wk_hc_single)
wk_hc_complete_vec <- unlist(wk_hc_complete)
wk_hc_average_vec <- unlist(wk_hc_average)

x_vals <- 2:10

plot(x_vals, wk_kmeans_vec, type="o", col="blue", ylim=c(min(c(wk_kmeans_vec, wk_hc_single_vec, wk_hc_complete_vec, wk_hc_average_vec)), 
                                                         max(c(wk_kmeans_vec, wk_hc_single_vec, wk_hc_complete_vec, wk_hc_average_vec))), 
     xlab="Number of Clusters", ylab="Weighted Kappa", main="Weighted Kappa Graph")

lines(x_vals, wk_hc_single_vec, type="o", col="red")      # Single linkage
lines(x_vals, wk_hc_complete_vec, type="o", col="green")  # Complete linkage
lines(x_vals, wk_hc_average_vec, type="o", col="purple")  # Average linkage

legend("bottomright", legend=c("K-means", "HC Single", "HC Complete", "HC Average"),
       col=c("blue", "red", "green", "purple"), lty=1, pch=1)

