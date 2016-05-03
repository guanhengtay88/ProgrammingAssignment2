## Making use of lexical scoping to store cache. In this case the cache is stored in the environment where 
## makeCacheMatrix is.

## Creates a list of functions for cacheSolve to 'refer' to.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y  #y will be from the global environment
    inv <<- NULL
  }
  get <- function() x #function() means this function doesn't require an argument, and will print x.
  setinv <- function(inverse) inv <<- inverse #mean is also from global environment
  getinv <- function() inv #function() means this function doesn't require an argument, and will print m.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Solves the matrix by looking for the variable inv first. If it's empty, i.e. null, then it will move on to solving
## the matrix by using solve().

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv() #getinv() defined using variable inv, therefore will use inv in environment where makeCacheMatrix is.
  
  if(!is.null(inv)) { #if inv is not null... 
    message("getting cached data") 
    return(inv) #...it means there was an inverse that was being calculated previously, hence just print inv
  }
  
  inputmatrix <- x$get() #
  inv <- solve(inputmatrix, ...)
  x$setinv(inv)
  inv #printing the inverse matrix out.
}
