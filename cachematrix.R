## Assigment 2: create a special R objet that caches the inverse of a matrix so
## so that it can be looked up in the cache rather than being recomputated.

## The first function makeCacheMatrix creates a special matrix objet which contains
## the function to i) set the values of the matrix, ii) get the value of the matrix
## iii) set the value of the inverse, iv) get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x<<- y
                m<<- NULL
        }
        get <- function()x
        setInverse <- function (inverse) m<<- inverse
        getInverse <- function() m
        list (set = set, 
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## cacheSolve is a function that calculates the inverse of the special 
## matrix created above. It first checks if inverse of matrix has already 
## been calculated. If inverse has already been calculated the function 
## gets inverse from cache, if not the function calculates the inverse and 
## stores the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ##Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cache data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
}
