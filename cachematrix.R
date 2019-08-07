## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
