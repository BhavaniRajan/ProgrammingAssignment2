## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix that can cache its inverse.
## cacheSolve returns the inverse of matrix. It returns the value of from cache, 
## if it had been calculated already

## Write a short comment describing this function

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 # set value of solve to null
  # set value of inverse to null
  inv <- NULL
  
  # set the value of the matrix 
  # resets the value of inverse to null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setinverse <- function(inv_val) inv <<- inv_val
  
  # get the value of the inverse
  getinverse <- function() inv

  list(set = set, # gives the name 'set' to the set() function defined above 
       get = get, # gives the name 'get' to the get() function defined above
       setinverse = setinverse, # gives the name 'setinverse' to the setinverse() function defined above
       getinverse = getinverse) # gives the name 'getinverse' to the getinverse() function defined above

}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inv <- x$getinverse()
    # getting the cached value of solve
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    # getting the value of solve
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
