## The function "makeCacheMatrix" returns a list of functions which are 
## used in the next function to either find the inverse of the matrix from
## cache memory or calculate the same. The matrix is an input argument in
## this function.
makeCacheMatrix<-function(x) {
  I<-NULL
  set<-function(y){
    x<<-y
    I<<-NULL
  }
  get<-function() x
  setInverse<-function(Inverse) I<<-Inverse
  getInverse<-function() I
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
## This function searches inverse of matrix from cache memory or calculates
## the same.
cacheSolve<-function(x){
  I<-x$getInverse()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data<-x$get()
  I<-solve(data)
  x$setInverse(I)
  I
}