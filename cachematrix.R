## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##to store inverse of the matrix
  
  set <- function(y) {
    x <<- y ##to update matrix with the new matrix
    inv <- NULL ##to clear cached inverse (because matrix changed)
  }
  
  get <- function() x ##return current matrix
  
  setinverse <- function(inverse) inv <<- inverse ##to save inverse to cache
  getinverse <- function() inv ##return cached inverse
  
  list(set = set, get=get,
       setinverse=setinverse,
       getinverse=getinverse) ##return a list of function so matrix and inverse can be accessed
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() ##to check if there's a cached inverse already
  
  if(!is.null(inv)) {
    message("gettingg cached data") ##letting user know it's using cache
    return(inv) ##return the cached inverse
  }
  data <- x$get() ##to get original matrix
  inv <- solve(data, ...) ##solve the inverse
  x$setinverse(inv) ##cached it for next time
  
  inv ##return the inverse
}

