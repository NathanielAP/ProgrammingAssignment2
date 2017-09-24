## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to

# 1. set matrix value
# 2. get matrix value
# 3. set inverse matrix value
# 4. get inverse  matrix value

makeCacheMatrix <- function(cmParam = matrix()) {
  inv <- NULL
  set <- function(localParam) {
    cmParam <<- localParam
    inv <<- NULL
  }
  get <- function() cmParam
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function
# This function returns the inverse a matrix. 
# 1. Checks if the inverse is computed and skips the computation.
# 2. If inverse is not completed, it process the inverse and sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(csparam, ...) {
  inv <- csparam$getInv()
  if(!is.null(inv)) {
    message("retrieving cached data.")
    return(inv)
  }
  data <- csparam$get()
  inv <- solve(data)
  csparam$setInv(inv)
  inv
}
