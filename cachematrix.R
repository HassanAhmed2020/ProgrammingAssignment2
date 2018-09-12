## This program contains two functions makeCacheMatrix and cacheSolve. These functions are used for storing value of a matrix 
## and also caching the matrix's inverse. This way if the inverse of a matrix is once calculated, it stores it's iverse value 
## which can be used in fucture.


## makeCacheMatrix function created a special object "vector" which stores a matrix and also cache's its inverse. 
## This special "vector" object also stores 4 functions that can get or set the values of the matrix and its inverse. 
## It returns list of four functions, get, set, getsolve and set solve.

makeCacheMatrix <- function(x = matrix()) {


        s <- matrix()
        
        set <- function(y) {
                x <<- y
                s <<- matrix()
        }
        
        get <- function() {
                x
        }
        
        setsolve <- function(sol) {
                s <<- sol
        }
        
        getsolve <- function() {
                s
        }
        
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
        
        
}


## Write a short comment describing this function
## cacheSolve takes special object created by makeCacheMatrix functions as an argument and checks if inverse of the matrix 
## is already created or not. If inverse is already created then it resturns the cached matrix otherwise it calculates the inverse 
## of the matrix and add to the cache of the special object. This function returns inverse value of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        s <- x$getsolve()
        
        if(!anyNA(s)) {
                message("getting cached data")
                return(s)
        }
        
        data <- x$get()
        
        s <- solve(data, ...)
        
        x$setsolve(s)
        
        s
        
        
}
