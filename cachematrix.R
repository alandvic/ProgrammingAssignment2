## The two functions below allow caching the inverse of a matrix rather than 
## calculating it repeatedly, which is potentially a time-consuming operation. 
## This is especially evident when the inverse of a matrix has to be computed
## multiple times. If the contents of a matrix are not changing, it make sense 
## to cache the inverse.


## makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.
##
## Input: matrix x
## Output: a list containg functions to
##    * set the value of the matrix
##    * get the value of the matrix
##    * set the inverse of the matrix
##    * get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
    im <- NULL                 ## initiate im (the inverse of the matrix) to NULL
    set <- function(y) 
    {       
        x <<- y                ## (re)set the value of the matrix 
        im <<- NULL            ## (re)initialize im to NULL    
    }
    get <- function() x        ## return the original matrix    
    setsolve <- function(solve) 
    {
        im <<- solve           ## set the inverse to the input parameter "solve"
    }
    getsolve <- function() im  ## return the inverse matrix im, if it has already 
                               ## been calculated. Otherwise, return NULL
    # return the list of functions defined above
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)    
}


## cacheSolve() function returns the inverse of the special "matrix" created with 
## makeCacheMatrix() function above. If the inverse has already been calculated, 
## cacheSolve() gets the inverse from the cache and skips the calculation.
## Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache 
## via the setsolve() function.
##
## Input: special "matrix" x created by makeCacheMatrix() function above
## Output: the inverse of the original matrix that was used to create 
##         the special matrix x

cacheSolve <- function(x, ...) 
{
    im <- x$getsolve()         ## get the cached inverse 
    if (!is.null(im))          ## has the inverse been calculated already?
    {
        message("getting cached data")  ## yes, so display a message
        return(im)                      ## and return the inverse;
    }
    data <- x$get()            ## otherwise, get the original martrix,
    im <- solve(data)          ## calculate the inverse,
    x$setsolve(im)             ## and set the inverse in the cache
    im                         ## return the inverse
}



