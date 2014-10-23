# Leslie Self
# R Programming Class
# Programming Assignment #2, Lexical Scoping and getting the Inverse of a Matrix
# Date:  October 23, 2014

## I have two functions, makeCacheMatrix and cacheSolve. The makeCacheMatrix manages my cached value.
##   I will set it initially to null and then will create functions to get it and set it that value.
##   The cacheSolve matrix will either return the cache value of the inverse or it will calculate
##   the inverse and set it.


## this function create a list of matrix objects that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL  #initially sets i (for inverse) to null
        
        #returning i to be null so the next time it will get its new value
        set <- function(y) {
                x <<- y   #sets x to be the value supplied whichever enivironment x is stored
        
                i <<- NULL 
        }
        
        #get function just returns the value of x in the first enviornment it is found
        get <-  function() {
                x    
        }
        
        #sets the inverse variable to its new value
        setinverse <- function(inverse){
                i <<- inverse
        }
        
        #returns the value of i
        getinverse <- function()
        {
                i
        }
        
        #this creates my list of all the different functions and names them 
        # the same thing.  So in my cacheSolve function I can just call that name 
        # and get the result I want.
        list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)
        
}


## Returns the inverse of a matrix that is supplied to this function
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        #if i in the first environment it is found is NOT null, then return that i and message
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        #if i is null then we need to get the data and create its inverse 
        # and then sets its value in the parent and returns i
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
