# R programming course assignment 2
# Functions that cache the inverse of a matrix. For example:
# > source('cachematrix.R')
# > m <- makeCacheMatrix(matrix(c(4, 0, 0, 4), c(2, 2)))
# > cacheSolve(m)
# [,1] [,2]
# [1,]  0.25  0.0
# [2,]  0.0  0.25

# The first function, makeVector creates a special "vector", which is really a list containing a function to 
# 1.set the value of the vector
# 2.get the value of the vector
# 3.set the value of the mean
# 4.get the value of the mean

# 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL 
  set <- function(y) { 
    x <<- y 
    m <<- NULL 
    } 
  get <- function() x 
  setinverse <- function(inv) m <<- inv 
  getinverse <- function() m
  list( 
    set = set, 
    get = get, 
    setinverse = setinverse, 
    getinverse = getinverse 
    ) 
} 

#2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
  i <- x$getinverse() 
  if(!is.null(i)) { 
    message("getting cached data") 
    return(i) 
    } 
  m <- x$get() 
  i <- solve(m, ...) 
  x$setinverse(i) 
  i 
} 
