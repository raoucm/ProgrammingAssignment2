## Put comments here that give an overall description of what your
## functions do

## Creating a function for matrix that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    mm <- NULL
    ## Setting the metrix here
    set <- function(y) {
        x <<- y
        mm <<- NULL
    }
    ## Getting the matrix here. 
    get <- function(){
        x
    }
    ## Setting the inverse of the matrix
    setInv <- function(inv) {
        mm <<- inv
    }
    ## Getting the inverse of the matrix
    getInv <- function() {
        mm
    }
    ## Retun the list of the methods
    list(set = set, 
         get = get, 
         setInv = setInv, 
         getInv = getInv)

}


## After crating the inverse of a matrix, here we retrieve the that function
## from the cache. 

cacheSolve <- function(x, ...) {
    ## return a the inverse of x
    mm <- x$getInv()
    ## return the inverse if it is set
    if (!is.null(mm)) {
        message('getting cached data')
        return(mm)
    }
    data <- x$get()
    ## matrix multiplication
    mm <- solve(data) %*% data
    ## set the inverse
    x$setInv(mm)
    ## Return a matrix that is the inverse of 'x'
    mm
        
}
