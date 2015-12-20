## The cachematrix module defines two functions
## 
##  1. makeCacheMatrix, a closure which stores a matrix and its inverse matrix
##     for the purpose of caching the inverse matrix. It returns a CacheMatrix object
##     as a named list of getters/setters which access the closure variables
##  2. cacheSolve, a function which takes a CacheMatrix object and returns the inverse matrix


## The makeCacheMatrix function stores a quadratic invertible matrix along 
## with its inverse matrix and provides getter/setter access to both of them.
makeCacheMatrix <- function(m = matrix()) {
    invm <- NULL    ## initial value of cached inverse matrix
    
    ## matrix setter
    setmatrix <- function(mx) {
        m <<- mx
        invm <<- NULL
    }
    
    ## matrix getter
    getmatrix <- function() {
        m
    }
    
    ## inverse matrix setter
    setinvmatrix <- function(invmx){
        invm <<- invmx
    }
    
    ## inverse matrix getter
    getinvmatrix <- function() { 
        invm 
    }
    
    ## provide a named list of getters/setters
    list(setmatrix = setmatrix, 
         getmatrix = getmatrix,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## The cacheSolve function takes a cached matrix object, created by makeCacheMatrix
## and returns the inverse of this matrix. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invx <- x$getinvmatrix()
    if(!is.null(invx))   ## is inverse matrix already available?
    {
        return(invx)
    }
    else
    {
        invx <- solve(x$getmatrix())   ## compute the inverse matrix
        x$setinvmatrix(invx)           ## store the inverse matrix in the closure x
        return(invx)                   
    }
}

## a test function
test <- function()
{
    n <- 20
    D <- diag(n)
    D[1, n] <- 1
    cm <- makeCacheMatrix(D)
    InvD <- cm$getinvmatrix()
    # show that InvD is NULL
    stopifnot(is.null(InvD))
    
    # retrieve InvD from cache
    InvD <- cacheSolve(cm)
    
    # show that InvD == D^-1?
    stopifnot(identical(InvD, solve(D)))
    
    # look up InvD in cache
    InvD2 <- cm$getinvmatrix()
    
    # show that InvD == InvD2
    stopifnot(identical(InvD, InvD2))
    
    "OK"
}

