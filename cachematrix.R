## Below are two functions created for Assignment 2 to cache the inverse
## of a matrix

## makeCacheMatrix function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## set the value of the vector
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get the value of the vector
  get <- function() x
  
  ## set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## get the value of the inverse
  getinverse <- function() i
  
  ## List of matrix and thier inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  cacheSolvefunction computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Get the inverse of Matrix from List
  i <- x$getinverse()
  
  ## If the value returned is not null return the inverse value 
  ## and get out of function
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## When no value is returned from list calculate the inverse
  ## save the value to the list and return the inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

