## cachematrix has 2 functions that caches the inverse of matrix
## There may be some benefit (in terms of computation cost) 
## to caching the inverse of matrix rather than compute it immediately

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    
    get<-function() x    
    setInverse<-function(Inverse) m<<-Inverse
    getInverse<- function() m
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cachesolve function computes and return the inverse of the special "matrix" returned 
## by makeCacheMatrix function.If the inverse has already been calculated then 
## function retrieves the inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getInverse()
    if(!is.null(m)) return(m)
    data<-x$get()
    m<-solve(data)
    x$setInverse(m)
    m
  
}
