## Put comments here that give an overall description of what your
## functions do
## The matrix inversion calculation is very time-comsuming,
## especially when the matrix size becomes large.
## Write a short comment describing this function
## This one creates object which can store inverse already computed.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(n){
                m <<- n
                inv <<- NULL
        }
        get <- function() m
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## The function can retrive the inversion if exists,
## otherwise, compute it and store it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- m$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data <- m$get()
        inv <- solve(data, diag(nrow(data)))
        m$setinv(inv)
        inv
}
