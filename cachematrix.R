## Write a short comment describing this function

## Descriptions:
## makeCacheMatrix() function creates cache of initial matrix and
## return a list with results of using subfunctions.
## It allows us to monitor the execution 

makeCacheMatrix <- function(x = matrix()) {
    # Prepare dataset: Clear the old data
    m <- NULL  
    # Convert the initial matrix
    set <- function(y) {
        x <<- y    
        m <<- NULL 
    }
    # Return the result matrix
    get <- function() x
    # Set the inverse
    setInverse <- function(inverse) m <<- inverse
    # Get the inverse
    getInverse <- function() m
    
    # Return a list with subfunctions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve() function to calculate inverse transformation of cache matrix into initial matrix

cacheSolve <- function(x, ...) {
    # Return  the inverse matrix
    m <- x$getInverse() 
    # To control empty dataset
    if(!is.null(m)) { 
        message("getting cached data")
        return(m)
    }
    # Calculate the inverse and return matrix
    data <- x$get()  
    m <- solve(data) 
    x$setInverse(m)  
    m                
}
