## Store input matrix and creates cache of inverse matrix "cm"
##
## set function set the inverse to NULL
## get function retreives the cached matrix
## 
makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  set <- function(y){
      cm <<- NULL
  }
  get <- function() x
    setinv <- function(inverse) cm <<- inverse
    getinv <- function() cm
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## Write a short comment describing this function
##
## Return a matrix that is the inverse of 'x', retrieves the cached matrix if exists
##
## getinv gets the inverse of matrix
## setinv calculates the inverse of matrix
cacheSolve <- function(x, ...) {
  cm <- x$getinv()
  if(!is.null(cm)) {
    message("getting cached data")
    return(cm)
  }
  a <- x$get()
  cm <- solve(a)
  x$setinv(cm)
  cm
}
## EXAMPLE OF USE
## 
##  > a=rbind(c(1, -1/4), c(-1/4, 1))
##  > a
##  [,1]  [,2]
##  [1,]  1.00 -0.25
##  [2,] -0.25  1.00
##  > b <- makeCacheMatrix(a)
##  > c <- cacheSolve(b)
##  > d <- cacheSolve(b)
##  getting cached data if matrix exist
