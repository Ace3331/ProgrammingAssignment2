# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Setter function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Invalidate cache when matrix is set
  }
  
  # Getter function to retrieve the matrix
  get <- function() x
  
  # Getter function to retrieve the cached inverse
  getInverse <- function() inv
  
  # Setter function to set the inverse and cache it
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Return a list of functions
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

# Function to compute the inverse of the special "matrix" object, utilizing caching
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # If the inverse is not cached, compute it, cache it, and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
