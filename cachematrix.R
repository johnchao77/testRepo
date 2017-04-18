## Caching the inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeChcheMatrix above. if the inverse has already been calculated (and the matrix
## has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  Inv <- solve(mat,...)
  x$setInv(Inv)
  message("Computing...")
  x$getInv()
}

