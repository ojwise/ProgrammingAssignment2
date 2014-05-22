## This code contains two functions: "makeCacheMatrix" and "cacheSolve." The "makeCacheMatrix" function takes advantage of the
##<<- operator so that assignments made within the environment of that function apply outside of it, so that such values can
##be used in the "cacheSolve" function.

## The makeCacheMatrix" function creates a list of the outputs of four functions. It also creates the cache variable m.
## set - sets the value of a matrix.
## get - prints the value of the matrix assigned to "set" 
## setinverse - sets the value of the inverse of the matrix
## getinverse - 
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setinverse<-function(anon) m<<-anon
    getinverse<-function()m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
 ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) { 
    m<- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m<- solve(data, ...)
        x$setinverse(m)
        m
}
       
