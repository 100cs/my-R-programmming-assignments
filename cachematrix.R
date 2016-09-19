## Put comments here that give an overall description of what your
## functions do
## The 'makeCacheMatrix' function  creates a special "vector", a list containing
## a function to set, get, set the inverse, get the inverse of a given matrix
##
## The 'cacheSolve' function gets the inverse of the special "vector" 
## created with the 'makeCacheMatrix' function


## Write a short comment describing this function
## This function creates a list of functions to do the following:
## set, get, set the inverse, get the inverse of a given matrix 'x'
makeCacheMatrix <- function(x = matrix()) {
        iMatrix <- NULL
        
        set <- function(y){
                x <<- y
                iMatrix <<- NULL
        }
        
        get <- function() x
        setIMatrix <- function(inverse) iMatrix <<- inverse
        getIMatrix <- function() iMatrix
        
        ## output list of functions to do operations
        list(set=set,get=get,setIMatrix=setIMatrix,getIMatrix=getIMatrix)
        
}


## Write a short comment describing this function
## This functions is responsible for checking if the value of the 
## inverse of the matrix is cached, if its not then it solves it 
## and caches the value for future reference
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## 'x' is assumed to always be an invertible matrix
        
        ## assigns a cached value
        iMatrix <- x$getIMatrix()
        
        ## checks to see if there is actually a value
        if(!is.null(iMatrix)) {
                message("I am getting cached data, please wait...")
                ## return the value and stop here
                return(iMatrix)
        }
        
        ## ...else returns the matrix to 'data'
        data <- x$get()
        
        ## getting the inverse of a square matrix with the 'solve' function in R
        iMatrix <- solve(data, ...)
        
        ## cache the value of the inverse of the matrix 
        x$setIMatrix(iMatrix)
        
}

