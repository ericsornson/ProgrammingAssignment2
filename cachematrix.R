## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i_matrix <- NULL
  
  set <- function(y) {
    x <<- y
    i_matrix <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverted_matrix) {
    i_matrix <<- inverted_matrix
  }
  
  getinverse <- function() {
    i_matrix
  }
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverted_matrix <- x$getinverse()
  if(!is.null(inverted_matrix)) {
    message("getting cached data")
    return(inverted_matrix)
  }
  data <- x$get()
  inverted_matrix <- solve(data, ...)
  x$setinverse(inverted_matrix)
  inverted_matrix
}
