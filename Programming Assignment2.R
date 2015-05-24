## makeCacheMatrix creates a vector of functions to apply to a matrix.  They will:
## i) set - sets the value of a matrix
## ii) get - gets the value of a matrix
## iii) setinverse - sets the inverse of a matrix, and
## iv) getinverse - gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ## restore inverse value to null
        set <- function(y) {
                x <<- y   ##<<- operator expands scope for matrix assignment to parent environment
                inv <<- NULL   ## to prevent out of date inverse matrix value if set is called
        }
        get <- function() {
                x         # returns the matrix value
        }
        setinverse <- function(mat) {
                inv <<- solve(mat)
        }
        getinverse <- function() {
                inv
        }
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve first checks to see if the inverse has been cached, and if so returns it.
## If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()   # 1st check if inverse exists in cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matx <- x$get()     # default if inverse isn't in cache
        inv <- solve(matx)
        x$setinverse(inv)
        inv
}