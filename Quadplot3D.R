QuadPlot3D<-function(B,Standardized=TRUE, Labels=c("V1","V2","V3","V4"),COL="black"){
   require(rgl)

   B2C <- function(M){
  K<-matrix(NA,ncol=3, nrow=length(M[,1]))

  for( i in 1:length(M[,1])){
    L<-M[i,]
   X<-c(-1,1,1)
   Y<-c(1,-1,1)
   Z<-c(1,1,-1)
   Q<-c(-1,-1,-1)

     x <- L[1]*X[1] +  L[2]*Y[1] +  L[3]*Z[1] +  L[4]*Q[1]
     y <- L[1]*X[2] +  L[2]*Y[2] +  L[3]*Z[2] +  L[4]*Q[2]
     z <- L[1]*X[3] +  L[2]*Y[3] +  L[3]*Z[3] +  L[4]*Q[3]

 K[i,]<-    c(x,y,z) }

 K
     }

    if(Standardized==TRUE){
    B2<-B
    }else{
           B2<-B
      for(i in 1:length(B[,1]))
      B2[i,]<-B[i,]/sum(B[i,])
         }

  wire3d( translate3d( tetrahedron3d(col = "red"), 0, 0, 0) )

  text3d(x=-1, y = 1, z = 1, Labels[1], adj = 0.5)
  text3d(x=1, y = -1, z = 1, Labels[2], adj = 0.5)
  text3d(x=1, y = 1, z = -1, Labels[3], adj = 0.5)
  text3d(x=-1, y = -1, z = -1, Labels[4], adj = 0.5)

  plot3d(x=B2C(B2)[,1], y=B2C(B2)[,2], z=B2C(B2)[,3], xlab="G", ylab="H", zlab="I", type = "p",  add = TRUE,col=COL,size=10)


  }
