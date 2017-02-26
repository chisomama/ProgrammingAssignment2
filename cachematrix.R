## Hello Peer, this set of functions calculates the inverse of a matrix
## and caches it in the parent enviroment to reduce calculation time

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){ ## Mutate the matrix as needed
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve #set the inverse of the matrix
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve() # If this already exists, no need to calculate.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
