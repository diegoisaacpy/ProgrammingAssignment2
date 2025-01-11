## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse to NULL
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix is changed
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse of the matrix
  getinverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Get the cached inverse from the input matrix object
  inv <- x$getinverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is not cached, calculate it
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...) # Calculate the inverse using solve()
  
  # Cache the inverse 
  x$setinverse(inv)
  
  # Return the inverse
  inv
}