## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Creating a special matrix with 4 functions named set, get, setinverse and getinverse
library(MASS)
makeCacheMatrix <- function(x = matrix()) {  
  inv <- NULL   
  set <- function(y) { 
    x <<- y
    inv <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function(){
    inver<-ginv(x)
    inver%*%x
  } 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x', if the matrix is unchanged then it will 
#provide the inverse from the cache, else will compute the new inverse and return it
cacheSolve <- function(x, ...) { 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv    
}