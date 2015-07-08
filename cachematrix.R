## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## The pair of functions are to cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## initial the inverse as null
  inverse<-NULL
  
  ## define set/get methods
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  
  get<-function() x
  
  setinverse<-function(inv) inverse<<-inv
  
  getinverse<-function() inverse
  
  ## list all the methods
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse value of x
  inv<-x$getinverse()
  
  ## if inverse is calculated already, return the calculated value
  if(!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  
  ## otherwise, calcuate the inverse of x and save the inverse value of x
  data<-x$get()
  
  inv<-solve(data)
  
  x$setinverse(inv)
  
  inv
  
}
