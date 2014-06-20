## A cached solution to calculating the inverse of a matrix.
## If a matrix inverse is already calculated, do not calculate again, instead return from a cache
## functions do

## Create a cacheable matrix which stores its inverse

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL #Initialize the inverse of matrix x to null
  
  set <- function(value) { #Set a new value of the matrix
    x <<- value #Overwrite x with a new value set
    matrixInverse <<- NULL #reset the inverse of x to null
  }
  
  get <- function() { #Get the value of the matrix
    x
  }
  
  setInverse <- function(inverse) { #Set the inverse of the matrix
    matrixInverse <<- inverse
  }
    
  getInverse <- function() { #Get the inverse
    matrixInverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculate the inverse of a matrix by skipping calculation if it is already in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse() #get the inverse of the matrix if already cached
  
  if(!is.null(inverse)) { 
    message("Returning a cached inverse")
    return(inverse) #return the cached inverse
  }
  
  #Otherwise calculate the inverse and cache it
  data <- x$get() #get the matrix
  inverse <- solve(data) #calculate the inverse
  x$setInverse(inverse) #cache the inverse
  inverse #return it
}
