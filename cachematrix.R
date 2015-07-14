## makeCacheMatrix creates a matrix that can cache its inverse.

## I set the matrix, get the matrix, set the inverse of the matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        invm<-NULL
        set<-function(y){
                x<<-y
                invm<<-NULL
        }
        get<-function() x
        setinv<-function(inverse) invm<<- inverse
        getinv<-function() invm
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)
}


## It calculates the inverse of the matrix calculated before. (First, it gets the inverse if it was calculated before.) If it was not calculated before it solves it.

cacheSolve <- function(x=matrix(), ...) {
        invm<-x$getinv()
        if(!is.null(invm)){
                message("getting cached data")
                return(invm)
        }
        inv<-x$get()
        invm<-solve(inv, ...)
        x$setinv(invm)
        invm
}
