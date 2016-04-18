 makeVector <- function(x = numeric()) {
         m <- NULL
         set <- function(y) {
                 x <<- y
                 m <<- NULL
         }
         get <- function() x
         setmean <- function(mean) m <<- mean
         getmean <- function() m
         list(set = set, get = get,
              setmean = setmean,
              getmean = getmean)
 }
 cachemean <- function(x, ...) {
         m <- x$getmean()
         if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
         }
         data <- x$get()
         m <- mean(data, ...)
         x$setmean(m)
         m
 }
## This function creates a special "matrix" object that can cache its inverse.
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
 # The following function returns the inverse of the matrix. It first checks if
 # the inverse has already been computed. If so, it gets the result and skips the
 # computation. If not, it computes the inverse, sets the value in the cache via
 # setinverse function.
 
 # This function assumes that the matrix is always invertible.
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
  x = rbind(c(1, -1/4), c(-1/4, 1))
  m = makeCacheMatrix(x)
  m$get()

## No cache in the first run
 cacheSolve(m)
## Retrieving from the cache in the second run
 cacheSolve(m)
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getinverse()
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
 my_matrix$get()
my_matrix$getinverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getinverse()