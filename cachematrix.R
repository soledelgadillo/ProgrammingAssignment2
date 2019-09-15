## makeCacheMatrix will calculate the inverse of a matrix and 
## store both the matrix and its inverse in its environment.
## cacheSolve will either retrieve the inverse of the matrix or
## calculate it, if it has not been calculated and stored
## previously.

## Since makeCacheMatrix returns an object that contains functions 
## to its parent environment, the object has access to both the
## specific functions in the list and the entire environment defined 
## by makeCacheMatrix, even the argument of the function. 

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## cacheSolve will start with one argument, but because that one 
## argument is an object resulting from the makeCacheMatrix, additional
## arguments will be passed into the function. So, cacheSolve will first
## attempt to retrieve the inverse of the matrix from the object passed 
## in the argument. Since makeCacheMatrix sets inv to NULL everytime a new 
## matrix is set into the object, if inv is other than NULL there is a valid
## inverse matrix to return to the parent environment. Otherwise, it gets
## the matrix from the input object and calculates and returns the inverse
## of the new matrix. 

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
        

