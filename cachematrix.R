makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Sample run:
## > x = rbind(c(1, 2/5), c(2/5, 1))
##> m = makeCacheMatrix(x)
##> m$get()
##      [,1] [,2]
##[1,]  1.0  0.4
##[2,]  0.4  1.0

## No cache in the first run
## >  cacheSolve(m)
##      [,1]       [,2]
##[1,]  1.1904762 -0.4761905
##[2,] -0.4761905  1.1904762

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##      [,1]       [,2]
##[1,]  1.1904762 -0.4761905
##[2,] -0.4761905  1.1904762
## > 
