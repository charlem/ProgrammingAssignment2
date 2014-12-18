## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function compute, store and retrun the inverse with the first cacheSolve access of the input matrix. 
# It fetches previously stored inverse with the subsequent cacheSolve access of the same input matrix 



makeCacheMatrix <- function(x = matrix()) {             # input will be a matrix
        
        inv = NULL                                      # will be the inverse matrix and is reset to null 
                                                        # every time makecachematrix is called
                
        
        get <- function() { x }                         # this function returns the original matrix
        
        setsolve <- function(solve) {inv <<- solve}     # called by cachesolve during the first cachesolve
                                                        # access and will store the value using superassignment
        
        getsolve <- function() {inv}                    # this will return the cached value on subsequent accesses
        
        list(get = get,                                 # accessed each time makecachematrix is called, 
             setsolve = setsolve,                       # This is a list of the internal functions ('methods')  
             getsolve = getsolve)                       # so a calling function knows how to access those methods.    

}
## This function uses the object and functions defined in the makecachematrix function to return 
## the inverse of the matrix or to access the and retrun the inverse from the cache if already computed 


cacheSolve <- function(x, ...) {                # input x is an object created by makeCacheMatrix
## Return a matrix that is the inverse of 'x'
        
        
        inv <- x$getsolve                       # accesses the object x and gets its inverse
        
        if (!is.null(inv)){                      # if inverse was already cached
                
                message("getting cached data")  #send this message to the console
                
                return(inv)                     #and returns the inverse. Note that "return" ends the function
        }
        data <- x$get()                         # we reach this code only if x$getsolve() returned NULL
        
        inv <- solve (data, ...)                # if inv was NULL then we have to calculate the inverse
        
        x$setsolve(inv)                         # store the calculated inverse value in x (see setmean() 
                                                # in makeCacheMatrix
        
        inv                                     # return the inverse to the code that called this function
}

