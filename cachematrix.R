## We create two functions to optimize the operation of inverse one matrix

## We make an object to store the inverse of the matrix 
## a simple skeleton of the matrix.

makeCacheMatrix <- function(x = matrix()) {


  inver <- NULL
  set <-function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function (inv) inver <<- inv 
  getInverse <- function () inver
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
}

##We calculate the inverse of the matrix,
## if the object receveing has not defined yet, otherwise we return the cache data


cacheSolve <- function(x, ...) {
 
   inv <- x$getInverse()
   if (!is.null(inv))
   {
     message("getting cache data")
     return (inv)
   }
   matrixOr <-x$get()
   inv <- solve(matrixOr,...)
   x$setInverse(inv)
   inv

}
