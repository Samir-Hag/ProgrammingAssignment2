## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y){
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inver <<- solveMatrix
        getInverse <- function() inver
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inver <- x$getInverse()
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inver)
        inver
}
