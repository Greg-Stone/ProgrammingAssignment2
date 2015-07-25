## 25 July 2015

## Part of the intent of this exercise is to demonstrate 
## using <<- operator to assign a value to an object in an  
## environment that is different from the current environment.

## This function provides sub functions 
## get()    -- if uninitiated returns an empty matrix as indicated by (x = matrix())
##          -- if initiated with set function get() returns the uninverted matrix 'inPutMatrix'
## setInv() -- assigns variable 'inverted' the result of solve which is passed from function 'cacheSolve'
## getInv() -- retrieves the inverted matrix from cache

makeCacheMatrix <- function(x = matrix()) {
        
        inVerted <- NULL
        
        set <- function(inPutMatrix){
                
                x <<- inPutMatrix
                inVerted <<- NULL
                
        }
        
        get <- function() x
        
        setInv <- function(solve) inVerted <<- solve
        
        getInv <- function() inVerted
        
        list(set = set, 
             get = get,
             setInv = setInv,
             getInv = getInv)
        
}


## First attempt to retrieve the inverted matrix from the above function
## If it exists then get cached result and display message
## If it does not exist then invert then assign to cache, also display message

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        inVerted <- x$getInv()
        
        if(!is.null(inVerted)){
                
                message("getting cached data")
                return(inVerted)
                
        }
        
        message("not getting cached data")
        notInverted <- x$get()
        inVerted <- solve(notInverted)
        x$setInv(inVerted)
        
        inVerted
        
}
