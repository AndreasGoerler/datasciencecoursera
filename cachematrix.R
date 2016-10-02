## This function creates an object (matrix) that can cache its invers

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
  
##This function calculates the inverse of the matrix created by the function
## makeCacheMatrix. The fuction retrieves the inverse from the cache if the inverse has already been calculated


  cacheSolve <- function(x, ...) {
    
    ## Returns a matrix that is the inverse of matrix x
    inv <- x$getInverse()
    if(!is.null(inv)){
      return(inv)
    }
    dat <- x$get()
    inv <- solve(dat)
    x$setInverse(inv)
    inv      
  }
