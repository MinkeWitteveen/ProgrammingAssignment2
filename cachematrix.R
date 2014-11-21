## Code for a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    #Initialise a variable called i (which will be the inverse of the matrix x)
    i <- NULL
    
    #Create a function called set which specifies new values for the matrix
    set <- function(matrix) {
        #Superassigns the matrix values for x, writing over existing values if present
        x <<- matrix
        
        #As a new matrix has been set the inverse has to be recalculated
        #Clear any existing values of i superassigning a null value
        i <<- NULL
    }
    
    #A function called get() which returns the values of the matrix
    get <- function() {
        #Return the matrix
        x
    }
    
    #A function called setInv() which sets the inverse of matrix x to matrix i
    setInv <- function(inverse) {
        #Superassigns the value for i as the user inputted values
        i <<- inverse
    }
    
    #A function which returns the values for i (a matrix of the inverse of matrix x)
    getInv <- function() {
        #Returns the values for i
        i
    }
    
    #Links the internal functions of makeCacheMatrix to a name or 'tag'
    #Allows the called function to know where to find the methods of the tag thats called
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    #i is assigned the returned value from the internal function getInv() fron the makeCacheMatrix function
    #i could be the value of inverse of matrix x saved in i
    #or the user-defined values from makeCacheMatrix$setInv() saved in i
    #or null
    i <- x$getInv()
    
    #Check whether the inverse has been calculated or set
    if(!is.null(i)) {
        #Inverse has been calculated/set so print out this message
        message("getting cached data")
        
        #and return the values of the inverse of matrix x saved in i
        #or the user-defined values from makeCacheMatrix$setInv() saved in i
        return(i)
    }
    
    #If i is empty (null)
    #Save the values of the matrix x
    data <- x$get()
           
    #Calculate the inverse of the matrix using 'solve' and save it to i
    i <- solve(data)
    
    #Cache the values of i 
    x$setInv(i)
    
    ## Return a matrix that is the inverse of matrix x
    i 
}
