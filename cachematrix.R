## Modified by Jawad Rashid
## Put comments here that give an overall description of what your
## functions do
## These functions cache the inverse of a matrix to avoid re-computation of the same 
## matrix to make the computations faster. These functions become useful if we have to
## calculate the inverse of a large matrix over and over again in a loop. In that case in
## loop the first the time the inverse will be calculated and next time the result will be
## retrieved from the cache rather than re-computed.

## In the first function makeCacheMatrix it takes it input as a matrix and provides 
## functions to set the inverse computed or get the value from cache. The variable inv
## is used as a storage for inverse. The <<- operator in setInverse stores the inverse
## in global environment and the getInverse retrieves a computed inverse from global 
## environment which is used as a cache. 
## The x parameter is used to store the matrix for which inverse is to be calcuated

## The cacheSolve checks first is there is inverse in cache for the given matrix through
## makeCacheCatrix object. If there is an inverse it returns the answer matrix without
## doing any computations otherwise it calculates the inverse, sets the answer in 
## cache and then returns it

## REFERENCE: Code modified from the CacheMean code given in assignment summary

## To test the following code was used
## cacheObject <- makeCacheMatrix(x = matrix(c(2,0,0,5),nrow=2,ncol=2))
## cacheSolve(cacheObject)
## cacheSolve(cacheObject)
## The first time cacheSolve was called the inverse was returned. Next time the inverse
## was retrirved from cach and message "Getting inverse from cache" was returned.

## Write a short comment describing this function
## This function takes input x which is the matrix for which to calculate the inverse and
## provides setter and getter function to store or retrieve the x and the inverse from 
## cache. The inv variable is the inverse. 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function takes it's input the object from makeCacheMatrix and first checks if there
## any inverse for the data in cache if there is already an inverse it returns the result
## from cache otherwise it calculates and return the inverse by using solve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting inverse from cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
