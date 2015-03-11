## The functions below operates special "vector" object that contains 
## special functions that can set/get and inverse the matrix.
## The object allows to cach result of inversing and recalculates it only in 
## case the matrix has been changed

## this function returns "Vecotor" of functions 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setInv <- function(z) inv <<- z
        getInv <- function() inv
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)
                
}


## Allows to check for cached result or make an inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
