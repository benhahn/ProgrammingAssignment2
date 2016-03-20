## These two functions work together to create a "matrix" object that caches its inverse
## and then compute the inverse of that "matrix". If the inverse has already been
## computed, the cached data is returned instead

## makeCacheMatrix creates a special "matrix", which is a list containing functions that do the following:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix using the solve function
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function() inverse
      list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix used in makeCacheMatrix
## If the inverse is already cached, the saved data is returned

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}
