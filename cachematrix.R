## cachematrix.R Store a matrix and it's inverse.  The makeCacheMatrix function 
## is a wrapper around a matrix.  Passing this matrix wrapper to the cacheSolve
## calculates the inverse of the matrix and stores the result in the wrapper
## object.


# Matrix wrapper that store the inverse of the matrix.
#
# Args:
#  x: a matrix to be stored
#
# Returns: 
#  a list of functions...
#    set: sets the matrix
#    get: gets the matrix
#    setinverse: sets the inverse of this matrix
#    getinverse: returns the inverse of this matrix
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


# Solve the inverse of a matrix object created by makeCacheMatrix. Cache the 
# value in the inverse in the makeCacheMatrix wrapper object.  If the
# makeCacheMatrix wrapper object already contrains the cache, this value will be
# returned.
# 
# Args: x: a makeCacheMatrix wrapper object
# 
# Returns: the inverted matrix

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
