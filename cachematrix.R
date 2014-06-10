## makeCacheMatrix: build "matrix" that can 
#       cache its inverse
## cacheSolve: get the cached inverse if it exists

## initialize cached matrix, provide functions
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s<<-solve
        getsolve <- function() s
        list(set=set, get=get,
             setsolve=setsolve,
             getsolve=getsolve)
}


## return inverse of matrix from cache if exists,
## otherwise calculate inverse and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        
        if (!is.null(s)){
                message("getting cached data")
                return (s)
        }
        #cache empty, use solve() to get the inverse
        data <- x$get()
        s <- solve(data,...)
        #and store to cache
        x$setsolve(s)
        s
}
