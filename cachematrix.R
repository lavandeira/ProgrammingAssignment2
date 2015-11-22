## The first function makeCacheMatrix will take any matrix as an argument, assuming it has an inverse it will calculate it and store it.

## The second function cacheSolve will take as arguments the parameters stored as a result of the first function, it will check if the
## matrix used for the first function has an inverse and if it does it will print it, if the inverse of the matrix does not exist or is
## not there yet, it will calculate it and print it.


        ## FOR THE FIRST FUNCTION
##this creates a function which gets a null matrix as an argument
makeCacheMatrix <- function(x = matrix()) { 
  
  ## this initializes the value of the matrix inverse to NULL
  matinverse <- NULL 
  
  ## this delcares a function named "set" where the value will be cached
  set <- function(y) {                      
    x <<- y
    
    ## this sets the value of inverse of the matrix to null in case the matrix was changed.
        matinverse <<- NULL              
  }
  ## this declares a function that gets the value of the inverse 
  get <- function() x             
  
  #this calculates the inverse of the given matrix (assuming it has one) and gets its value
    setinverse <- function(solve) matinverse <<- solve 
    getinverse <- function() matinverse    
  
  ## this creates a list that gets the values of the function makeCacheMatrix        
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}

        ## FOR THE SECOND FUNCTION
# this creates a function which gets the returned values (matrix) from the previous function as arguments
cacheSolve<- function(x, ...) {                 
  matinverse <- x$getinverse()
  
  #this checks if the inverse is not null, it gets it and returns it
  if(!is.null(matinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matinverse)
  }
  #this checks if the inverse if not there, first it is calculated and then retrieved.
  data <- x$get()                               
  matinverse <- solve(data, ...)
  x$setinverse(matinverse)
  
  ## this returns the matrix that is the inverse of 'x' calculated in the "else" part of the loop
  matinverse
}
     

