## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## The pair of functions are to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  
  get<-function() x
  
  setinverse<-function(inv) inverse<<-inv
  
  getinverse<-function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv<-x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  
  data<-x$get()
  
  inv<-solve(data)
  
  x$setinverse(inv)
  
  inv
  
}
