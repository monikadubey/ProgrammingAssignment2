## This program creates a matrix that can cache its inverse.
## The inverse of the matrix is calculated using the function cacheSolve. If the inverse already exists in cache, it retrives the inverse from there


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  op <- NULL
  set <- function(y) {
    x <<- y
    op <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) op <<- inv
  getinverse <- function() op
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)


}


## For Computing the inverse and retriving data from cache

cacheSolve <- function(x, ...) {
  op <- x$getinverse()
  if(!is.null(inv)) {
    return(op)
  }
  mat <- x$get()
  op <- solve(mat)
  x$setnverse(op)
  op
}
