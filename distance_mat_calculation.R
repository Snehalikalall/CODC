distance_matrix<-function(data1,data2) { 
  library("copula")
  library("stats")
  #u <- matrix(runif(20501*2), 19, 2)
  dist_mat<-matrix(c(0,0), nrow=2000,ncol = 2000)
  data_col1<- matrix(c(0,0), nrow=ncol(data1),ncol = 2)
  data_col2<- matrix(c(0,0), nrow=ncol(data2),ncol = 2)
  for (i in 1:1999)
  {
    for (j in (i+1):2000)
    {
      data_col1[,1]=t(data1[i,])
      data_col1[,2]=t(data1[j,])
      data_col2[,1]=t(data2[i,])
      data_col2[,2]=t(data2[j,])
      
      ec1 <- C.n(pobs(data_col1), data_col1)
      ec2 <- C.n(pobs(data_col2), data_col2)
      p=ks.test(ec1,ec2)
      dist_mat[i,j]=p$statistic
    }
  }
  return(dist_mat)
}