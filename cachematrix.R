## makeCacheMatrix and cacheSolve are functions that save time in computing matrix inverses

## makeCacheMatrix creates a "matrix" which contains the getters and setters

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) i <<- invert
  getinvert <- function() i
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)

}

## cacheSolve solves the inversion for the given matrix utilizing makeCacheMatrix

cacheSolve <- function(x, ...) {

  i <- x$getinvert()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvert(i)
  i
  
  ## Return a matrix that is the inverse of 'x'
  
}

#Test code:
aMatrix <- makeCacheMatrix(matrix(1:4,2,2))
aMatrix$get()
aMatrix$set(matrix(1:4,2,2))
aMatrix$getinvert()
cacheSolve(aMatrix)
