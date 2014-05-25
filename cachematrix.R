## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve #save the matrix passed in and saves to m
    getsolve <- function() m #retrieve the matrix m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) { #check to see if m has anything in it
        message("getting cached data") 
        return(m) #if m has a value, then m is returned
    }
    data <- x$get() #use get function to get matrix and place in 'data'
    m <- solve(data, , ...) #set 'm' to be inverse of the matrix in 'data'
    x$setsolve(m) #save the inverse matrix into variable m
    m #return m to console, go team!
}

a <- makeCacheMatrix()
a
class(a)
class(a$set)

a$set(matrix(8:23, 4, 4))          #set the vector
a$get()
cacheSolve(a)
