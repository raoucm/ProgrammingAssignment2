## Put comments here that give an overall description of what your
## functions do

## Creating a function for matrix that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    mm <- NULL
    set <- function(y) {
        x <<- y
        mm <<- NULL
    }
    get <- function(){
        x
    }
    setInv <- function(inv) {
        mm <<- inv
    }
    getInv <- function() {
        mm
    }
    
    list(set = set, 
         get = get, 
         setInv = setInv, 
         getInv = getInv)

}


## After crating the inverse of a matrix, here we retrieve the that function
## from the cache. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mm <- x$getInv()
    if (!is.null(mm)) {
        message('getting cached data')
        return(mm)
    }
    data <- x$get()
    
    mm <- solve(data) %*% data
    
    x$setInv(mm)
    
    mm
        
}
