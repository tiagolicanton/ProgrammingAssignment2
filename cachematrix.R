## makeCacheMatrix creates a special vector that is a list of functions to 
## 1. set the matrix of the vector
## 2. get the matrix of the vector
## 3. set the solve of the vector
## 4. get the solve of the vector

makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(s) m <<- s
    getsolve <- function() m
    list (set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}

## cachasolve calculates the inverse of the special vector created with makeCacheMatrix.
## It first checks if the inverse has been already calculated. It so, the cache is fetched
## and returned without calculation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse via setsolve function

cachesolve <- function(x, ...){
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached solve")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}