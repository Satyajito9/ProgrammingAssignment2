##This function caches the inverse of a matrix to save time and avoid repetitive computations

## Creating the special matrix for which the inverse has to be calculated

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  setMat <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  getMat <- function() x
  setinvMat <- function(inverse) invMat <<- inverse
  getinvMat <- function() invMat
  list(setMat = setMat, getMat = getMat,
       setinvMat = setinvMat,
       getinvMat = getinvMat)
}


## caches the inverse of the special matrix created in the function earlier

cacheSolve <- function(x, ...) {
  invMat <- x$getinvMat()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$getMat()
  invMat <- solve(data, ...)
  x$setinvMat(invMat)
  invMat
}
