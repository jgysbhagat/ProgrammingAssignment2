## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix (invertible matrix) which returns a 
##list containing a function to set the matrix, get the matrix, set the inverse,
##get the inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## The function calculates the inverse of the special matrix created with the
## above function. It first checks whether the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the
##computation. Else, it calculates the inverse of the data.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data.matrix<-x$get()
  i<-solve(data.matrix,...)
  x$setinverse(i)
  return(i)
}
