makeCacheMatrix <- function(x=matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<-inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if ( ! is.null(m)) {
        print("getting cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setInverse(m)
    m
}
## Testing and soultions
> a <- makeCacheMatrix(matrix(1:4,2))
> a$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> a$set(matrix(5:8,2))
> a$get()
     [,1] [,2]
[1,]    5    7
[2,]    6    8
> cacheSolve(a)
     [,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> a$getInverse()
     [,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> b=a$getInverse()
> a$get() %*% b
     [,1]         [,2]
[1,]    1 3.552714e-15
[2,]    0 1.000000e+00
