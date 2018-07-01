## These are functions to calculate the inverse of the matrix
## 

## Creating the caches of the matrix inversion
makeCacheMatrix <- function(x = matrix()) {
  TestInv <- NULL
  set <- function(y){
    x <<- y
    TestInv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) TestInv <<- inverse
  getInv <- function() TestInv
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)

}
## Computes the inverse of the matrix returned by makeCacheMatrix and return NULL 
## if hasn't been computed yet.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  TestInv <- x$getInv()
  if (!is.null(TestInv)){
    message("getting cached data")
        return(TestInv)
  }
  mat <- x$get()
  TestInv <- solve(mat, ...)
  x$setInv(TestInv)
  TestInv
}