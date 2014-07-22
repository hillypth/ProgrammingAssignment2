## The first function, makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse. The "matrix" object is a list containing 
## a function to

## 1. set the value of the cached matrix
## 2. get the value of the cached matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m.inv <- NULL
  set <- function(y) {
    x <<- y
    m.inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m.inv <<- matrix
  getmatrix <- function() m.inv
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## The following function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` function. If the inverse
## has already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m.inv <- x$getmatrix()
  if(!is.null(m.inv)) {
    message("getting cached inverse matrix")
    return(m.inv)
  }
  data <- x$get()
  m.inv <- solve(data)
  x$setmatrix(m.inv)
  m.inv
}
