## R programming Class
## Programming Assignment 2 : Caching the Inverse of a Matrix
## The following functions are used to cache the inverse of a matrix, matrix inversion is usually a costly computation 
## therefore caching the inverse of a matrix rather than computing it repeatedly has some advantages in terms of efficiency
## These functions take advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object.



## This is the main function where the matrix object is created and all secondary functions are defined
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  
  #Set the value of the matrix
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x
  
  #set the value of the inverse of the matrix
  setInverse <- function(inverse) inv_x <<- inverse
  
  #get the value of the inverse
  getInverse <- function() inv_x
  
  #list of secondary functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the inverse of a matrix, if the inverse
## has been previously calculated then it will return the cached value
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  #check if inverse has already been calculated
  inv_x <- x$getInverse()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  
  #calculates the inverse of a matrix
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setInverse(inv_x)   #caches the inverse of the matrix just calculated
  inv_x                 #returns the inverse value
  
}
