# Caching and Inversing a squared matrix.

## This cachematrix.R file contains two functions:
##1. makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
##2. cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
##   If the inverse has already been calculated (and the matrix has not changed), then the cachesolve function retrieves the inverse from the global cache.

## The makeCacheMatrix function creates a special "matrix".
## The return value is the list of specialized matrix functions to:
## 1.   set the value of the matrix
## 2.	get the value of the matrix
## 3.	set the inversed value
## 4.	get the inversed value
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}


## The cacheSolve function creates and retrieves an inversed matrix.
## The return value is the inversed matrix that was processed using the solve() function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Now getting cached inversed matrix data.")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


### Examples and Results:
###amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))

###amatrix$get()         # Returns original matrix
###[,1] [,2]
###[1,]    1    3
###[2,]    2    4

###cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
###[,1] [,2]
###[1,]   -2  1.5
###[2,]    1 -0.5

###amatrix$getinverse()  # Returns matrix inverse  ???????????
###[,1] [,2]
###[1,]   -2  1.5
###[2,]    1 -0.5

###cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse getting cached data getting cached data
###[,1] [,2]
###[1,]   -2  1.5
###[2,]    1 -0.5

###amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix

###cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
###[,1] [,2]
###[1,] -0.13333333  0.2
###[2,]  0.01010101  0.0

###amatrix$get()         # Returns matrix
###[,1] [,2]
###[1,]    0   99
###[2,]    5   66

###amatrix$getinverse()  # Returns matrix inverse????
###[,1] [,2]
###[1,] -0.13333333  0.2
###[2,]  0.01010101  0.0