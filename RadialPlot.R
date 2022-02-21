
for(clusternumber in 1:6){cat(clusternumber)
  centroids_zscored_clusterk<-centroids_zscored1[,clusternumber]
  
  centroids_zscored_clusterk_positive<-replace(centroids_zscored_clusterk,which(centroids_zscored_clusterk<0),0)
  centroids_zscored_clusterk_negative<-replace(centroids_zscored_clusterk,which(centroids_zscored_clusterk>0),0)
  centroid_clusterk_pos<-NULL;centroid_clusterk_neg<-NULL
  
  for(i in 1:9){
    centroid_clusterk_pos<-rbind(centroid_clusterk_pos,mean(centroids_zscored_clusterk_positive[which(fs86_to_yeo_map==i)]))
    centroid_clusterk_neg<-rbind(centroid_clusterk_neg,mean(centroids_zscored_clusterk_negative[which(fs86_to_yeo_map==i)]))
  }
  cbind(centroid_clusterk_pos/max(centroid_clusterk_pos),centroid_clusterk_neg/(max(abs(centroid_clusterk_neg))))
  cbind(centroid_clusterk_pos,centroid_clusterk_neg)
  maxvalue<-max(max(centroid_clusterk_pos), max(abs(centroid_clusterk_neg)));maxvalue
  
  maxvalue<-0.68
  
  library(fmsb)
  data<-as.data.frame(rbind(as.numeric(centroid_clusterk_pos),as.numeric(abs(centroid_clusterk_neg))), ncol=9)
  max(data)
  colnames(data) <- c("DAN","VAN","LIM","FP","DMN","SUB" ,"CER","VIS","SOM")
  data <- rbind(rep(maxvalue,k) , rep(0,k) , data)
  # colors_border=c( rgb(0.8,0.2,0.5,0.9),rgb(0.2,0.5,0.5,0.9) )
  # colors_in=c( rgb(0.8,0.1,0.1,0.1),rgb(0.1,0.1,0.1,0.1) )
  library(RColorBrewer)
  coul <- brewer.pal(3, "BuPu")
  colors_border <- coul[3]
  
  colors_border=c(rgb(0.7,0.5,0.1,0.9), coul[3])
  # colors_in=c(rgb(0.7,0.5,0.1,0.4), coul[3])
  colors_in=c("white","white")
  
  pdf(file = "radialplot",   # The directory you want to save the file in
      width = 18, # The width of the plot in inches
      height = 12)
  par(mar=c(5,5,5,5));par(mfrow=c(1,1))
  library(fmsb)
  
  # plot with default options:
  radarchart( data  , axistype=1 , 
              #custom polygon
              pcol=colors_border , 
              # pfcol=colors_in , 
              plwd=4 , plty=1,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="white", 
              # caxislabels=seq(0,1.8,0.2), cglwd=0.8,
              caxislabels=seq(0,1,0.2), cglwd=0.8,
              #custom labels
              vlcex=1.5 
  )
  
  # Add a legend
  legend(x=1.1, y=1.3, legend = c("High amplitude","Low amplitude"), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=2, pt.cex=2)
  dev.off()
  
}
