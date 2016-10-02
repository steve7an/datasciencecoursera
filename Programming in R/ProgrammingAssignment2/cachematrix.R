## Assignment 2: Caching the Inverse of a Matrix
## more info can be found about this here: 
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y){
        x <<- y
        mat <<- NULL
    }
    get <- function(){ x }
    setmatrix <- function(matrix){ mat <<- matrix }
    getmatrix <- function(){ mat }
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getmatrix()
    if(!is.null(mat)){
        message("getting cached data")
        return (mat)
    }
    data <- x$get()
    mat <- solve(data)
    x$setmatrix(mat)
    mat
}

