## Create a function that creates a matrix object 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
    #Start out by clearing the cache (in child environment)
    m <- NULL     
    set <- function(y) {
      #Using << double arrow assignment in order to impact variables in the parent environment
      x <<- y              
      m <<- NULL
    }
    
    ## Simple function to get the value of a matrix
    get <- function() x
    ## Simple function to set the inverse of a vector/matrix
    setInverse <- function(inverse) m <<- inverse
    ## Simple function to get the inverse from the cache
    getInverse <- function() m
    
    ## The function returns a list of the outputs from set, get, setmean and getmean functions:
    return(list(set = set, get = get,
         setInverse = setInverse,
         getInverse= getInverse))
}


## Solve for the inverse of matrix

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of x
  
        #Get the value of the matrix from cache
        m <- x$getInverse() 
        
        if(is.null(m)){
          #If cache is empty, we need to calculate it first.
          A <- x$get()    #Get the matrix that was passed to function
          m <- solve(A)   #Solve for the inverse of A
          x$setInverse(m) #This will write the result to the cache
        }
        
        return(m)       #Return the calculated inverse (if cache was empty) or, if cache was not empty, just return cache
}
