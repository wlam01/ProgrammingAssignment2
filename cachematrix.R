## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of 
## a matrix rather than compute it repeatedly. The following
## two functions, makeCAcheMatrix and cacheSolve, are used to 
## cache the inverse of a matrix.

## Function: makeCacheMatrix
## Purpose:  This function creates a special "matrix" object 
##           that can cache its inverse.  It contains a list 
##           of  function to:
##           1.  set the value of the matrix
##           2.  get the value of the matrix
##           3.  set the value of the inverse of the matrix
##           4.  get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## init the cached inverse 
        m <- NULL
        
        ## set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get the matrix
        get <- function() x
        
        ## set the cached inverse matrix
        setinverse <- function(inverse) m <<- inverse
        
        ## get the cached inverse matrix
        getinverse <- function() m
        
        ## return a list of set, get, setinverse, and getinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Function: cacheSolve
## Purpose:  This function computes the inverse of the special 
##           "matrix" returned by makeCacheMatrix above. 
##           If the inverse has already been calculated (and 
##           the matrix has not changed), then the function 
##           retrieve the inverse from the cache.
## Note:     This function assumes x is a square invertible 
##           matrix

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        
        ## check if the cached inverse matrix is available
        if(!is.null(m)) {
                
                ## available, return the cached inverse matrix
                message("getting cached data")
                return(m)
        }
        
        ## cached inverse matrix not available, get the matrix,
        ## calculate the inverse and set it
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
## Sample Run
        
##m1<-matrix(c(1,2,3,4),2,2)
##a<-makeCacheMatrix(m1)
##cacheSolve(a)
##a$getinverse()
##cacheSolve(a)
##a$get()
##a$getinverse()
##a$get() %*% a$getinverse()   ## to prove it's the correct inverse    

}
