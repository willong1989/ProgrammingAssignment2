## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Set to NULL for the first initiaalization
  m <- NULL
  
  #Set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Get the matrix
  get <- function() x
  
  #Set the inverse 
  setinverse <- function(inverse) m <<- inverse
  
  #Get the inverse
  getinverse <- function() m
  
  # four functions encapsulated in a list
  # 1. set the matrix
  # 2. get the matrix
  # 3. set the inverse of the matrix
  # 4. get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Get the current state of the inverse
  m <- x$getinverse()
  
  #If it has been computed then return the computed inverse
  if(!is.null(m)) {
    message("Getting cached matrix.")
    return(m)
  }
  
  #If not then get the matrix itself.
  data <- x$get()
  
  #Finding the inverse
  m <- solve(data, ...)
  
  ##Caching the result in an object
  x$setinverse(m)
  
  #Return this new result
  m
}
