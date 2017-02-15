## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix that can cache its inverse.
## cacheSolve returns the inverse of matrix. It returns the value of from cache, 
## if it had been calculated already

## Write a short comment describing this function

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 # set value of solve to null
  s <- NULL
  
  # set the value of the matrix 
  # resets the value of solve to null
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the solve
  setsolve <- function(solve_val) s <<- solve_val
  
  # get the value of the solve
  getsolve <- function() s
  list(set = set, # gives the name 'set' to the set() function defined above 
       get = get, # gives the name 'get' to the get() function defined above
       setsolve = setsolve, # gives the name 'setsolve' to the setsolve() function defined above
       getsolve = getsolve) # gives the name 'getsolve' to the getsolve() function defined above

}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    s <- x$getsolve()
    # getting the cached value of solve
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    # getting the value of solve
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
