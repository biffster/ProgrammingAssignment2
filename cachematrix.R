## Programming Assignment 2
## Michael Fierro
## August 23, 2015

## This assignment solution contains major sections of code from the example
## given in the assignment description examples at:

## https://class.coursera.org/rprog-031/human_grading/view/courses/975105/assessments/3/submissions

## The following two functions work in conjunction to compute the inverse of a
## passed matrix. A cache is consulted on each run: if a value already exists in
## the cache for the passed matrix, that value is returned instead of being
## re-computed.


## makeCacheMatrix creates a matrix that contains the cache of the previous
## matrix inverse computations. It makes available methods for the main function
## to read and write to the cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(mean) m <<- mean
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cachematrix does the heavy lifting of the pair. It first searches the matrix
## cache to see if the computed data is already there. If the cache is valid, that
## value is returned. If not, then this function uses solve() to compute the
## new value, writes this to the cache, and then returns the value.

cachematrix <- function(x = matrix(), ...) {
        #class(x)
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
