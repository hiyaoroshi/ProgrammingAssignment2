## Two functions that are used to create
## a special object that sotres a numeric matrix
## and inverse of the matrix.

## The first function, makeCacheMatrix creates 
## a list containing functions to
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function, cacheSolve returns 
## the inverse.
## If called at the first time for the value of 
## the matrix, it calculate the inverse and 
## save it in the cache,
## otherwise it just returns the saved inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}