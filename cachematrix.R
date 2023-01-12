## These functions are used to save time when inverting matrices 
## by saving matrices and their solved outputs with "<<-"

## Function saves a reference point to a matrix as an object in a
## cached parent environment. 

## Syntax is object <- makeCacheMatrix(matrix)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setmatrix <- function(inverse) i <<-inverse
    getmatrix <- function() i
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
} 

## Returns solved inverse of a matrix then stores the inverted 
## matrix in a cached parent environment
## if object is already cached will give a message "getting cached data"
## before returning an inverted matrix

## Syntax is cachesolve(matrix)
cachesolve <- function(x, ...) {
    i <- x$getmatrix()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setmatrix(i)
    i
}
