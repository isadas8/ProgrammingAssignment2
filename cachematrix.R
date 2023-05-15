## Put comments here that give an overall description of what your
## functions do

## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_matrix<-NULL
  
  set<-function(y){
    x<<-y
    inverse_matrix<<-NULL
  }
  get <-function() x
  setinverse<-function(solve) inverse_matrix<<-solve
  getinverse<-function() inverse_matrix
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)

}


##this function computes the inverse of the special matrix returned by makeCacheMatrix.
## if the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     inverse_matrix<- x$getinverse()
     if(!is.null(inverse_matrix)){
       message("getting cached data")
       return(inverse_matrix)
     }
     data<-x$get()
     inverse_matrix<-solve(data, ...)
     x$setinverse(inverse_matrix)
     inverse_matrix
}


mat <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)

