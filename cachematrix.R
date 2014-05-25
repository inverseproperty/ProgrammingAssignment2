# Two functions to initialize a matrix, then create
# an inverse of that matrix, store the inverse of 
# the matrix in cache, and if the function is run 
# on that matrix again then the stored cache is returned.

makeCacheMatrix <- function(x = matrix()) { # woohoo initialize x as matrix
    m <- NULL    # set m as NULL, empty list
    set <- function(y) { # set set as function, setting value of the matrix
        x <<- y  # setting x to y function for current environment and parent environment
        m <<- NULL # poor m is still NULL
    }
    get <- function() x # setting 'get' FUN to retrieve matrix in the future
    setsolve <- function(solve) m <<- solve # save the matrix passed in and saves to m
    getsolve <- function() m # retrieve the matrix m
    list(set = set, get = get, # functions
         setsolve = setsolve, 
         getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve() # set m to the 
    if(!is.null(m)) { #check to see if m has anything in it
        message("getting cached data") # the message to be printed when the cache is called
        return(m) #if m has a value, then m is returned
    }
    data <- x$get() #use get function to get matrix and place in 'data'
    m <- solve(data, , ...) #set 'm' to be inverse of the matrix in 'data'
    x$setsolve(m) #save the inverse matrix into variable m
    m           # return m to console, go team!
}


