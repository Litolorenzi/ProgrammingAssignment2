## Caching the Inverse of a Matrix:
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverted <<- inverse
        getInverse <- function() inverted
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix.
## If the inverse has already been calculated (without matrix change), 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

## Some tests

> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
Error: could not find function "makeCacheMatrix"
> makeCacheMatrix <- function(x = matrix()) {
        +         inverted <- NULL
        +         set <- function(y) {
                +                 x <<- y
                +                 inverted <<- NULL
                +         }
        +         get <- function() x
        +         setInverse <- function(inverse) inverted <<- inverse
        +         getInverse <- function() inverted
        +         list(set = set,
                       +              get = get,
                       +              setInverse = setInverse,
                       +              getInverse = getInverse)
        + }
> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
> my_matrix$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
> 
        > 
        > my_matrix$getInverse()
NULL
cacheSolve <- function(x, ...) {
        +         ## Return a matrix that is the inverse of 'x'
                +         inv <- x$getInverse()
                +         if (!is.null(inv)) {
                        +                 message("getting cached data")
                        +                 return(inv)
                        +         }
                +         mat <- x$get()
                +         inv <- solve(mat, ...)
                +         x$setInverse(inv)
                +         inv
                + }
> cacheSolve(my_matrix)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(my_matrix)
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(my_matrix)
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$getInverse()
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
> my_matrix$get()
[,1] [,2]
[1,]    2    1
[2,]    2    4
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
[,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> cacheSolve(my_matrix)
getting cached data
[,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> my_matrix$getInverse()
[,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333

