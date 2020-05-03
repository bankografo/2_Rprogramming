
### Thanks to functions below we can store a matrix and cache the inverse matrix
# This caching is useful because we needn't to compute it each time

####----
## In this function we create a special "matrix" that can cache its inverse .
####----

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  Set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  Take <- function() x
  SetInverse <- function(inverse) inver <<- inverse
  TakeInverse <- function() inver
  list(Set = Set,
       Take = Take,
       SetInverse = SetInverse,
       TakeInverse = TakeInverse)
}

####----
### Here we compute the inverse of the matrix created by "makeCacheMatrix" function.
#If the inverse matrix has already been computed, then we can make inverse matrix from the cache.
####----

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$TakeInverse()
  if (!is.null(inver)) {
    message("Get the cache of matrix!")
    return(inver)
  }
  mat <- x$Take()
  inver <- solve(mat, ...)
  x$SetInverse(inver)
  inver
}