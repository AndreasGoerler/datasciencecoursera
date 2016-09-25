## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = Matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
  


## Write a short comment describing this function

  cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
      return(inv)
    }
    dat <- x$get()
    inv <- solve(dat)
    x$setInverse(inv)
    inv      
  }