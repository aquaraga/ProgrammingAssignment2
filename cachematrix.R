## The following functions enable their consumers to efficiently compute the inverse of a matrix,
## by making use of caching. If the inverse of a particular matrix has never been computed, the
## normal course of computation is followed - and the inverse is cached.
## However; if the given matrix has previously been encountered, then the cached value of the
## inverse is returned.

## This function creates a special matrix object, which can additionally cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Creates and returns a special matrix object that can cache its inverse, given a 
  # standard R matrix object.
  #
  # Args:
  #   x: A standard R matrix object. If not passed in by a consumer, it defaults to an empty matrix
  #
  # Returns:
  #   A special matrix object, that can cache its inverse.
  inverse <- NULL
  set <- function(mat) {
    x <<- mat
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(anInverse) inverse <<- anInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Returns the inverse of a given special matrix, by looking it up in a cache or by computing it.
  #
  # Args:
  #   x: A special matrix object
  #
  # Returns:
  #   The inverse of the given special matrix
  
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}
