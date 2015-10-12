`barnardw.test` <-
function(n1,n2,n3,n4,dp=0.001) {
  n1<-abs(as.integer(n1))
  n2<-abs(as.integer(n2))
  n3<-abs(as.integer(n3))
  n4<-abs(as.integer(n4))

  conmat<-matrix(c(n1,n2,n3,n4),ncol=2,byrow=TRUE,dimnames=list(c("Outcome I","Outcome II"),c("Treatment I","Treatment II")))
  alternative<-c("One sided","Two sided")
  
  if (any(rowSums(conmat)==0) || any(colSums(conmat)==0)) {
    warning("No observations found in at least one category")
    return(list(contingency.matrix = conmat,
                alternative = alternative,
                p.value = c(1,1)))
  }
      
  vec.size<-1.0+1.0/dp
  mat.size<-4.0*(n1+n3+1)*(n2+n4+1)
  ret1=.C("WaldS",
    as.integer(n1),
    as.integer(n2),
    as.integer(n3),
    as.integer(n4),
    as.numeric(dp),
    mat.size = as.integer(0),
    wald.statistic.table = as.double(vector("double",mat.size)),
    wald.statistic = as.double(0.0))
  
  xr<-seq(1,ret1$mat.size,4)+2
  ret1$wald.statistic.table[xr+1][(ret1$wald.statistic<=0 & ret1$wald.statistic.table[xr]<=ret1$wald.statistic) | (ret1$wald.statistic>=0 & ret1$wald.statistic.table[xr]>=ret1$wald.statistic)]<-1
  ret1$wald.statistic.table[xr+1][(ret1$wald.statistic<=0 & ret1$wald.statistic.table[xr]>=-ret1$wald.statistic) | (ret1$wald.statistic>=0 & ret1$wald.statistic.table[xr]<=-ret1$wald.statistic)]<-2
      
  ret2=.C("BarnardW",
    as.integer(n1),
    as.integer(n2),
    as.integer(n3),
    as.integer(n4),
    as.numeric(dp),
    as.integer(ret1$mat.size),
    nuisance.vector.x = as.double(vector("double",vec.size)),
    nuisance.vector.y0 = as.double(vector("double",vec.size)),
    nuisance.vector.y1 = as.double(vector("double",vec.size)),
    wald.statistic.table = as.double(ret1$wald.statistic.table))

  np0<-which.max(ret2$nuisance.vector.y0)
  np1<-which.max(ret2$nuisance.vector.y1)
  
  nuisance.matrix<-matrix(cbind(ret2$nuisance.vector.x,ret2$nuisance.vector.y0,ret2$nuisance.vector.y1),ncol=3)
  wald.statistic.table<-matrix(ret1$wald.statistic.table,ncol=4,byrow=TRUE,dimnames=list(c(),c("n1","n2","wald.statistic","include.in.p.value")))

  cat(sprintf("\nBarnard's Unconditional Test\n"),sep="\n")
  print(conmat)
  cat(sprintf("\nNull hypothesis: Treatments have no effect on the outcomes\nWald statistic = %g\nNuisance parameter = %g (%s), %g (%s)\nP-value = %g (%s), %g (%s)\n",
              ret1$wald.statistic,
              ret2$nuisance.vector.x[np0],
              alternative[1],
              ret2$nuisance.vector.x[np1],
              alternative[2],
              ret2$nuisance.vector.y0[np0],
              alternative[1],
              ret2$nuisance.vector.y1[np1],
              alternative[2]),sep="\n")
  
  return(invisible(
      list(wald.statistic.table = wald.statistic.table,
           nuisance.matrix = nuisance.matrix,
           dp = dp,
           contingency.matrix = conmat,
           alternative = alternative,
           wald.statistic = ret1$wald.statistic,
           nuisance.parameter = ret2$nuisance.vector.x[c(np0,np1)],
           p.value = c(ret2$nuisance.vector.y0[np0],ret2$nuisance.vector.y1[np1]))
      ))
}

