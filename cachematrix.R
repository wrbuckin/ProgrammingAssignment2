## The first function creates a matrix that has the ability to cache its own inverse
## cacheSolve() computes the inverse of the matrix from the makeCacheMatrix function
## This function checks to see if the inverse has been calcuated. If so, the function
## retrieves the cached inverse. If it is not present the function calculates the inverse using solve
## The caching speeds up the calculation  -serving as a demo for big data exercises

## Create the matrix and cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y ## <<- assigns value to the object in the env outside of the current
                invM <<- NULL
        }
        get <- function() x
        setInv <- function(inv) invM <<- inverse
        getInv <- function() invM
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv) # preps the functions for the cache test
}



## compute the inverse from above, check the cache, if present retireve - otherwise complete the calculation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        # function option if the inverse is in the cache
        if(!is.null(invM)) {
                # retrieves the inverse from the cache and prevents the computation from running, alerting the user
                message("getting cached data")
                return(invM)
        }
        # if inverse is not in cache, inverse calculation is run
        data <- x$get()
        m <- solve(data, ...)
        
        # runs the setInv function to compute the value for the inverse and sets the variable to that value
        x$setInv(m)
        return(invM)
}


