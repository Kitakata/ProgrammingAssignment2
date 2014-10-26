## The functions following create a specific inversible matrix and cache the inverse matrix of that, 
## enabling not calculate again the inverse matrix of this matrix specifies whether done

## store the matrix x and a list of functions to store and retrieve the inverse matrix of x and, 
## change or retrieve the matrix stored(the matrix x)

makeCacheMatrix <- function(x = matrix()) {
        s  <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## return the inverse matrix stored in x or invert the matrix stored in x case that isn't stored in x

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
