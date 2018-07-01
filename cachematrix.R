## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
## Write a short comment describing this function

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