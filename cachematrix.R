

makeCacheMatrix <- function(x = matrix()) {
## set the value of the matrix
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
}
## get the value of the matrix
  get <- function() x
  ## set the inverse of the matrix
  setinverse <- function(solve) mat <<- solve
  ## get the inverse of the matrix
  getinverse <- function() mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         ## get the inverse of the matrix   
  mat <- x$getinverse()
  ## check if there is the matrix
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  ## set the inverse of the matrix 
  x$setinverse(mat)
  mat
}
