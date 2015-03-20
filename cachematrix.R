## Matrix inversion is a computationally expensive excersise
## With these functions we cache the data rather than
## perform continuous computations.

## The purpose of this function is to create a list of 
## 4 elements containing the following 4 functions:
## set the matrix value, get the matrix value
## set the matrix inverse value, get the matrix inverse value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the matrix.
## If the matrix was already computed then it returns the cache.
## Otherwise computes the inverse and sets that value to the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
