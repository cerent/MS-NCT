library(R.matlab)
library(stringr)
library(R.matlab)
library(viridis)
library("lattice")
library(robustbase)
library(matrixStats)
library(vioplot)

rwb <- colorRampPalette(colors = c("blue", "white", "red"))
  col1<-rwb(200)
  labels<-c("VIS+","VIS-","VAN+","VAN-","SOM+","SOM-")
  
  beta_param1_v2<-matrix(beta_param1,6,6)
  colnames(beta_param1_v2) <- labels ; rownames(beta_param1_v2) <- labels
  p1<-levelplot(t(beta_param1_neworder2),main="HC vs MS without disability",col.regions=col1,at=at1)
  # the code is written for 4 heatmaps, therefore I'm additing the following line to have 4 heatmaps in total
  p2<-levelplot(t(beta_param1_neworder2),main="HC vs MS without disability",col.regions=col1,at=at1) 
  

  beta_param2_v2<-matrix(beta_param2,6,6)
  colnames(beta_param2_v2) <- labels ; rownames(beta_param2_v2) <- labels
  p3<-levelplot(t(beta_param2_v2),main="HC vs MS with disability",col.regions=col1,at=at1)
  
  beta_param3_v2<-matrix(beta_param3,6,6)
  colnames(beta_param3_v2) <- labels ; rownames(beta_param3_v2) <- labels
  p4<-levelplot(t(beta_param3_v2),main="MS without vs with disability",col.regions=col1,at=at1)
  
  print(p1, split = c(1, 1, 2, 2), more = TRUE)
  print(p2, split = c(2, 1, 2, 2), more = TRUE)
  print(p3, split = c(1, 2, 2, 2), more = TRUE)
  print(p4, split = c(2, 2, 2, 2), more = FALSE)
