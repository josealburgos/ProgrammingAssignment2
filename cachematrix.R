## this is a set of two functions.  
## makeCacheMatrix() captures a matrix
## cachesolve calculates the inverse of the matrix and keeps its value
## in memory.

## makeCacheMatrix stores a squared matrix.  If matrix is not squared
## there is no inverse.  The function generates an error if a non-squared
## matrix is entered

makeCacheMatrix <- function(x = matrix()) {
        if (!(nrow(x) == ncol(x)))
            stop("Please enter a squared matrix")
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL    
        }
        get <- function(){
                x    
        }
        setmatrix <- function(mat){
                m <<- mat     
        } 
        getmatrix <- function() m
        list(set=set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve takes the squared matrix and calcualtes the inverse
## it stores the inverse in m.  Wen this function is run for a second time
## on the same matrix, the inverse is already stored in m, so the function
## returns m (doesn't not calcualte the inverse again)

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

b <- matrix(rnorm(16),4,4)
a <- makeCacheMatrix(b)
