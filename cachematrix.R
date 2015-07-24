## This pair of functions are used to effeciently calculate matrix inverses
## Once the inverse of a specific matrix is calculated,
## it's cached, allowing the calculation function to return
## the correct answer without re-preforming the underlying calculation

## makeCacheMatrix adds functions to support caching to a vanilla matrix
makeCacheMatrix <- function(originalMatrix = matrix()) {
    ## cachedInverse will store the inverse of originalMatrix
    ## When set to NULL, it indicates no inverse has been cached.
    ## As we haven't calculated anything yet, it's set to NULL by default
    cachedInverse <- NULL
    
    ## The setMatrix function resets our enhanced matrix without requring external reassignment
    setMatrix <- function(newMatrix) {
        ## change the value of our originalMatrix to that of newMatrix
        originalMatrix <<- newMatrix
        ## as our originalMatrix has (probably) changed, dump our cached inverse
        cachedInverse <<- NULL
    }
    
    ## The setInverse function receives an inverse matrix to be cached
    ## inverse gets stored to the cachedInverse in the parent environment
    ## Note that this function is completely trusting that an outside function
    ## has correctly calculated the inverse, and isn't just handing it some random vector
    setInverse <- function(inverse=matrix()) cachedInverse <<- inverse
    
    ## The getMatrix function simply returns the matrix contained in originalMatrix from the parent environment
    getMatrix <- function() originalMatrix
    
    ## The getInverse function returns the cached inverse from the parent environment, even if it's NULL
    getInverse <- function() cachedInverse
    
    ## The list below contains named references to each of the functions within makeCacheMatrix(),
    ## making them available externally via subsetting the list
    list(setMatrix=setMatrix,
         getMatrix=getMatrix,
         setInverse=setInverse,
         getInverse=getInverse)
}


## cacheSolve() takes an object created with makeCacheMatrix()
## and finds its cached inverse if one exists.  Otherwise,
## it calcualtes the inverse and caches it
cacheSolve <- function(theMatrix, ...) {
    ## get the existing cahched matrix inverse value from theMatrix
    cachedInverse <- theMatrix$getInverse()
    ## check if a valid inverse alread exists (NULL indicates no cached inverse)
    if(!is.null(cachedInverse)) { ## cachedInverse isn't NULL, so a cached inverse exists
        ## let the world know!
        message ("Retreiving Cached Matrix Inverse (SO EFFECIENT!)")
        ## return the cached inverse, breaking out of cacheSolve() now
        return(cachedInverse)
    }
    ## If we reached this point, it's because cachedInverse was NULL
    ## Therefore, we need to actually compute the inverse and cache it
    ## First, get the original matrix contained in theMatrix
    original <- theMatrix$getMatrix()
    ## Next, calculate the inverse using solve()
    inverse <- solve(original)
    ## Then, cache the calculated inverse
    theMatrix$setInverse(inverse)
    ## Finally, return the calculated inverse
    inverse
}
