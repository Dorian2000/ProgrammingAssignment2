# For this assignment we're asked to write two functions. 
# The first one is called makeCacheMatrix: It creates a special "matrix" object that can cache 
# its inverse assuming the matrix inputted is invertible
# The second one is cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),
# then the cacheSolve should get the inverse from the cache.

############################################################################################################################
############################################################################################################################

## Comments on makeCacheMatrix
### The first function, makeCacheMatrix creates a special "matrix" 
### which is actually a list containing a function to :
### set the value of the matrix
### get the value of the matrix
### set the value of the inverse of the matrix inputted
### get the value of this inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
        y <- NULL # sets the value of y to NULL (provides a default if cacheSolve has not yet been used)
        setmatrix <- function(y) { #set the value of the matrix
                x <<- y ## caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
                m <<- NULL ## sets the value of m (the matrix inverse if used cacheSolve) to NULL
        }
        # Four sub-functions
        getmatrix <- function() x # get the matrix x
        setinverse <- function(inverse) m <<- inverse # set the value of the inverse
        getinverse <- function() m # get the value of the inverse
        list(setmatrix = setmatrix, getmatrix = getmatrix, # creates a list for the four functions
             setinverse = setinverse,
             getinverse = getinverse)
}

## Comments on cacheSolve
### The following function computes the inverse of the special "matrix" created with the makeCacheMatrix function.
### First of all checks to see if the inverse has already been calculated. 
### If so, it gets the inverse from the cache, skips the computation and returns the inverse. 
### If not, it calculates the inverse of the matrix, 
### sets the value of the inverse in the cache via the setinverse function and returns it.


cacheSolve <- function (x=matrix(), ...) {
        # Need to compare matrix to what was there before!
        m <- x$getinverse() # if an inverse has already been calculated this gets it
                if(!is.null(m)){ # check to see if cacheSolve has been run before
                message("getting cached data") # check that matrix hasn't changed, 
                # and if it hasn't, sends a text message and returns the cached matrix
                return(m)
        }
        
        # otherwise 
        y <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
        x$setmatrix(y) # run the setmatrix function on the input matrix to cache it
        m <- solve(y, ...) # compute the value of the inverse of the input matrix
        x$setinverse(m) # run the setinverse function on the inverse to cache the inverse
        m # return the inverse
}

# Test the functions
x <- matrix(c(4,9,7,2), nrow=2, ncol=2) 
y <- makeCacheMatrix(x)
inv <- cacheSolve(y)
inv
