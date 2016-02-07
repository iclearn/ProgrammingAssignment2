## The following functions create a matrix and provide us with functions in a list to set its value,
## get its value, set the inverse matrix, get the inverse matrix. So that if the value of the inverse is
## already there in the environment, the cached value is used, else it is computed and cached for future use.

## makeCahematrix(x) function takes a matrix as an input (or no input at all)
## and returns a list of functions: set, get, getInverse, setInverse to set the matrix, get the matrix,
## get the inverse matrix, and set the inverse matrix respectively


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve function caches the inverse value if its not there yet and returns it and returns the cached
## value if it is not null to save computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  inverse
}
