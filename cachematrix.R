# R-Programming - Assignment 2:
## the assigment is based on the lexical scoping and caching concepts. chache is a concept in which objects are stored in the memory
## to speed up the susequent processes. 

## For example, some algebric computations , e.g. calculating the inverse of a matrix, is a time-consuming
## comuptational process. Therefore, it will be more convenient to save the inverse function out put in the memory
## instead to repeating the calculation every time. 

## in the currrent assignment, the objective is to build pair of functions that cache the inverse of the matrix


## This file, contains two functions. the first function , makeCacheMatrix(), meant to store a matrix and 
## its inverse. makeMatrix(), contains four functions " set(),get(), setInverse() and getInverse()" which are returned into a list stored in the 
## the global environment (parent environment). the second function, cacheSolve(), recalls the inversed matrix
## cached by makeCacheMatric().

## makeCachMatrix() function starts with defining two objects ' x and m'. x was initialized as a function 
## arrgument, where m was set as NULL object within the makeCacheMatrix() function. after setting the key objects, 
## the setters and getters were defined. set() function first assigns the argument 'y'
## to 'x' in the parent environment 'global environment'and then set 'm' to NULL. as long as  'x' was not reset",
## 'm'  object will call NULL.
## (get() x) function, recalls 'x' from the parent environment (lexical scoping)
## (setmean() m) function, define the settings for 'm', and assigns the value of retern into 'm' in the 
## parent environment (lexical scoping).
## and finally, we list all the getters and setters in the function and assign names to them which will ease the recall 
## in the cacheSolve() function.

## there is no inverse calculation been carried out till now. the inverse of the matrix will be calculated 
## and cached in the cacheSolve() function

makeCacheMatrix <- function(x = matrix()) { 
        i <- NULL # initializing the object that stores the inversed matrix
        set <- function(y){  # reseting 'x'
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inver) i <<- inver
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve() function starts with argument 'x' and ellipsis. Then the function attempting to recall 
## the inverse matrix from 'i'. if the recalled 'm' was NULL, then cacheSolve() gets the matrix from 'x' and 
## calculates the inverse, and then return the output ' inverse' to the parent environment 'i'.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)){
                message("Getting Inverse Matrix")
                return(i)
        }
        input <- x$get()
        inv <- solve(input) # the only place where the 'inverse' is calculated.
        x$setInverse(i)
        i
}
