## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly.

## The function makeCacheMatrix creates a "special" matrix
## object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set=function(y){
      x<<-y
      inv<<-NULL
  }
  get=function()x
  setinv=function(inverse)inv<<-inverse
  getinv=function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The function cacheSolve computes the inverse of the
## "special matrix" returened by makeCacheMatrix; if the
## inverse is already calculated, then this function
## serves to retrieve it.

cacheSolve <- function(x=matrix(), ...) {
  inv=x$getinv()
  if(!is.null(inv)){
      message("getting cached data")
    return(inv)
  }
  matdata=x$get()
  inv=solve(matdata)
  x$setinv(inv)
  return(inv)
}
